import ammonite.ops._
import cats.implicits._
import io.chrisdavenport.cats.time._
import org.jsoup._
import org.rogach.scallop._
import io.circe._
import io.circe.parser._
import scala.collection.JavaConverters._
import scala.util.chaining._
import scala.util.control.Exception._
import scala.util.Try
import sys.process._
import wvlet.log.LogSupport

import java.nio.file.{Paths, Files}
import java.nio.charset.StandardCharsets

import java.time._
import java.time.format._
import java.io.File



object nytimes extends LogSupport {
  object io {
    def write(path: String, txt: String): Unit = {
      Files.write(Paths.get(path), txt.getBytes(StandardCharsets.UTF_8))
    }
    def toPDF(targetDirectory: Path, briefing: Briefing): Unit = {
      val inputFile = targetDirectory/briefing.htmlFilename
      val outputFile = targetDirectory/briefing.pdfFilename
      s"weasyprint $inputFile $outputFile" ! ProcessLogger(line => ())
    }
    def uploadToRemarkable(targetDirectory: Path, briefing: Briefing): Unit = {
      val targetFile = targetDirectory/briefing.pdfFilename
      s"rmapi put $targetFile".!
    }
  }

  val baseUrl = "https://www.nytimes.com/"
  val usBriefingUrl = "spotlight/us-briefing"

  case class Briefing(date: LocalDate, url: String, name: String, htmlFilename: String)  {
    def pdfFilename = htmlFilename.replaceAllLiterally(".html", ".pdf")
  }
  object Briefing {
    def fetchUSSpotlight = Jsoup.connect(s"$baseUrl$usBriefingUrl").get()
    def fetch(briefing: Briefing) = Jsoup.connect(s"$baseUrl${briefing.url}").get()
    def urlToName(s: String) = s.replaceAllLiterally(".html", "").split("-").map(_.capitalize).mkString(" ")

    def from(node: nodes.Element): Option[Briefing] = {
      val url = node.attr("href")
      val dateFormat = DateTimeFormatter.ofPattern("yyyy/MM/dd")
      val dateString = url.stripPrefix("/").split('/').take(3).mkString("/")
      val htmlFilename = url.split('/').lastOption
      for {
         date <- Try{LocalDate.parse(dateString, dateFormat)}.toOption
         name <- htmlFilename.map(urlToName)
         htmlFilename <- htmlFilename
      } yield new Briefing(date, url, name, htmlFilename)
    }

    def toDoc(doc: nodes.Document): List[Briefing] = 
      doc.select("a").asScala
        .filter(_.attr("href").contains("/briefing/"))
        .filterNot(_.attr("href").contains("signup.html"))
        .flatMap(Briefing.from)
        .toList
  }

  val prefix = "window.__preloadedData = "
  def briefingsData(doc: nodes.Document): Option[String] =
    doc.select("script").asScala
      .filter(_.data().startsWith(prefix))
      .headOption
      .map(_.data().replaceAllLiterally(prefix, "").dropRight(1))

  def keyHasValue(key: String, value: String)(hcursor: ACursor) =
    hcursor.get[String](key).toOption.contains(value)

  // TODO: this code is nasty, but it was the first thing I go working. :D
  def getImages(json: Json): List[String] = {
    val hcursor = json.hcursor.downField("initialState")
    hcursor.keys
      .fold(List[String]())(
        _.filterNot(_.endsWith("ledeMedia"))
          .map(hcursor.downField)
          .filter(keyHasValue("__typename", "ImageBlock"))
          .map(_.downField("media"))
          .filter(keyHasValue("typename", "Image"))
          .flatMap(_.get[String]("id").toOption)
          .map(k => hcursor.downField(k))
          .flatMap(lens => {
            lens.keys.map(
              _.filter(_.startsWith("crops"))
                .map(lens.downField)
                .flatMap(_.values)
                .flatten
                .flatMap(_.hcursor.get[String]("id").toOption)
            )
          })
          .flatten
          .map(hcursor.downField)
          //.flatMap(_.focus)
          .flatMap(_.downField("renditions").values)
          .flatten
          // TODO: For some reason the following line failes
          //       if I use .get[String]("id").toOption!?!
          .flatMap(_.hcursor.downField("id").as[String].toOption)
          .filter(_.contains("superJumbo"))
          .map(hcursor.downField)
          .flatMap(_.get[String]("url").toOption)
          .toList
      )
  }

  def insertLazyImages(doc: nodes.Document)(json: Json): nodes.Document = {
      val elements = doc.select("""figure div[data-testid="lazyimage-container"]""").iterator().asScala
      val images = getImages(json).drop(1)
      for {
        (url, element) <- (images zip elements)
      } {
        element.attr("style", "height: auto")
        val picture = element.appendElement("picture")
        picture.attr("style", "opacity: 1; display: block; width: 100%")
        val img = picture.appendElement("img")
        img.attr("class", "css-doesntmattr");
        img.attr("src", f"${url}?quality=75&amp;auto=webp&amp;disable=upscale")
        img.attr("decoding", "async")
        img.attr("style", "width:100%;vertical-align:top; height: auto; max-width: 100%")
      }
      doc
  }

  def downloadAndProcessBriefing(targetDirectory: Path, briefing: Briefing): nodes.Document = {
    val doc = Briefing.fetch(briefing)
    doc.pipe(briefingsData)
      .map(parse)
      .getOrElse(Left("No Json to Parse"))
      .tap(_.left.map(err => err.tap(err => error(s"Unable to parse JSON: $err"))))
      .toOption
      .map(insertLazyImages(doc))
      .getOrElse(doc)
  }

}

object fetch extends LogSupport  {

  def statFile(p: Path) = Try{stat! p}.toOption
  def isFile(p: Path) = statFile(p).map(_.isFile) getOrElse false
  def isDirectory(p: Path) = statFile(p).map(_.isFile) getOrElse false

  class Conf(arguments: Seq[String]) extends ScallopConf(arguments) {
    val targetDirectory = opt[String](required = true)
    verify()
  }

  def main(args: Array[String]): Unit = {
    val conf = new Conf(args)
    val targetDirectory = Path(conf.targetDirectory());
    if (!isDirectory(targetDirectory)) {
      mkdir! targetDirectory
    }
    info("Downloading briefings")
    val today = LocalDate.now
    nytimes.Briefing.fetchUSSpotlight
      .pipe(nytimes.Briefing.toDoc)
      .filter(_.date == today)
      .tap(b => info(f"Found ${b.length} briefings"))
      .filterNot(briefing => isFile(targetDirectory/briefing.pdfFilename))
      .tap(b => info(f"Updating ${b.length} briefings"))
      .tapEach(briefing => {
        info(f"Processing ${briefing.htmlFilename}")
        val doc = nytimes.downloadAndProcessBriefing(targetDirectory, briefing)
        nytimes.io.write((targetDirectory/briefing.htmlFilename).toString, doc.toString())
      })
      .tapEach(briefing => {
        info(f"Turning into pdf ${briefing.pdfFilename}")
        nytimes.io.toPDF(targetDirectory, briefing)
      })
      .tapEach(briefing => {
        info(f"Uploading to remarkable ${briefing.pdfFilename}")
        nytimes.io.uploadToRemarkable(targetDirectory, briefing)
      })
  }
}
