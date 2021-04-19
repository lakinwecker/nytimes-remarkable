import ammonite.ops._
import cats.implicits._
import io.chrisdavenport.cats.time._
import org.jsoup._
import org.rogach.scallop._
import io.circe._
import io.circe.parser._
import scala.collection.JavaConverters._
import scala.util.control.Exception._
import scala.util.Try
import scala.util.chaining._
import sys.process._
import wvlet.log.LogSupport


import java.nio.file.{Paths, Files}
import java.nio.charset.StandardCharsets

import java.time._
import java.time.format._
import java.io.File

object nytimes extends LogSupport {
  val baseUrl = "https://www.nytimes.com/"
  val briefingsListings = "spotlight/us-briefing"

  def dbg[A](f: A): A = { println(f); f }
  def urlToName(s: String) = s.replaceAllLiterally(".html", "").split("-").map(_.capitalize).mkString(" ")

  case class Briefing(date: LocalDate, url: String, name: String, htmlFilename: String)  {
    def pdfFilename = htmlFilename.replaceAllLiterally(".html", ".pdf")
    def fetchDoc = Jsoup.connect(s"$baseUrl$url").get()
  }

  def toBriefing(node: nodes.Element): Option[Briefing] = {
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

  def fetchBriefingsDoc = Jsoup.connect(s"$baseUrl$briefingsListings").get()
  def parseBriefings(doc: nodes.Document): List[Briefing] = 
    doc.select("a").asScala
      .filter(_.attr("href").contains("/briefing/"))
      .filterNot(_.attr("href").contains("signup.html"))
      .flatMap(toBriefing)
      .toList

  val prefix = "window.__preloadedData = "
  def briefingsData(doc: nodes.Document): Option[String] = {
    doc.select("script").asScala
      .filter(_.data().startsWith(prefix))
      .take(1)
      .headOption
      .map(_.data().replaceAllLiterally(prefix, "").dropRight(1))
    }

  def write(path: String, txt: String): Unit = {
    Files.write(Paths.get(path), txt.getBytes(StandardCharsets.UTF_8))
  }

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

  def downloadAndProcessBriefing(targetDirectory: Path, briefing: Briefing): Unit = {
    val doc = briefing.fetchDoc
    val data = briefingsData(doc)
    data.map(json => {
      write((targetDirectory/s"data${briefing.htmlFilename}.json").toString, json)  
      parse(json) match {
        case Right(obj) => {
          val images = getImages(obj).drop(1)
          val elements = doc.select("""figure div[data-testid="lazyimage-container"]""").iterator().asScala
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
        }
        case Left(err) => error(s"Unable to find data for images: $err")
      }
    })
    write((targetDirectory/briefing.htmlFilename).toString, doc.toString())
  }

  def toPDF(targetDirectory: Path, briefing: Briefing): Unit = {
    val inputFile = targetDirectory/briefing.htmlFilename
    val outputFile = targetDirectory/briefing.pdfFilename
    s"weasyprint $inputFile $outputFile".!
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
    val doc = nytimes.fetchBriefingsDoc
    val briefings = nytimes.parseBriefings(doc)
    val today = LocalDate.now
    info(f"Found ${briefings.length}")

    briefings
      .sortBy(_.date)
      .reverse
      .take(2)
      .filterNot(_.date == today)
      .filterNot(briefing => isFile(targetDirectory/briefing.pdfFilename))
      .tapEach(briefing => {
        info(f"Processing ${briefing.htmlFilename}")
        nytimes.downloadAndProcessBriefing(targetDirectory, briefing)
      })
      .tapEach(briefing => {
        info(f"Turning into pdf ${briefing.pdfFilename}")
        nytimes.toPDF(targetDirectory, briefing)
      })
  }
}
