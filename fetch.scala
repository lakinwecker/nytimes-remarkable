import ammonite.ops._
import cats.implicits._
import io.chrisdavenport.cats.time._
import org.jsoup._
import org.rogach.scallop._
import scala.collection.JavaConverters._
import scala.util.control.Exception._
import scala.util.Try
import wvlet.log.LogSupport

import java.time._
import java.time.format._

object weasyprint {
  def toPDF(htmlFilename: String): Unit = {
  }
}

object nytimes {
  val baseUrl = "https://www.nytimes.com/"
  val briefingsListings = "spotlight/us-briefing"

  def dbg[A](f: A): A = { println(f); f }
  def urlToName(s: String) = s.replaceAllLiterally(".html", "").split("-").map(_.capitalize).mkString(" ")

  case class Briefing(date: LocalDate, url: String, name: String, htmlFilename: String)  {
    def pdfFilename = htmlFilename.replaceAllLiterally(".html", ".pdf")
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

  def briefingsData(doc: nodes.Document): Option[nodes.Element] = 
    doc.select("script").asScala
      .filter(_.data().startsWith("window.__preloadedData = "))
      .take(1)
      .headOption

  def downloadAndProcessBriefing(briefing: Briefing): Briefing = briefing
}

object fetch extends LogSupport  {

  def exists(p: Path) = Try{stat! p}.toOption.map(_.isFile) getOrElse false

  class Conf(arguments: Seq[String]) extends ScallopConf(arguments) {
    val targetDirectory = opt[String](required = true)
    verify()
  }

  def main(args: Array[String]): Unit = {
    val conf = new Conf(args)
    val targetDirectory = Path(conf.targetDirectory());
    println(targetDirectory)
    if (! (stat! targetDirectory).isDir) {
      mkdir! targetDirectory
    }
    info("Downloading briefings")
    val doc = nytimes.fetchBriefingsDoc
    val briefings = nytimes.parseBriefings(doc)
    info(f"Found ${briefings.length}")
    briefings
      .reverse
      .take(1)
      .filterNot(briefing => exists(targetDirectory/briefing.pdfFilename))
      .foreach(briefing => {
        info(f"Processing ${briefing.htmlFilename}")
        nytimes.downloadAndProcessBriefing(briefing)
        info(f"Turning into pdf ${briefing.pdfFilename}")
        weasyprint.toPDF((targetDirectory/briefing.htmlFilename).toString)
      })
  }
}
