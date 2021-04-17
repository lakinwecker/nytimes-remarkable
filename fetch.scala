import ammonite.ops._
import cats.implicits._
import io.chrisdavenport.cats.time._
import org.jsoup._
import org.rogach.scallop._
import scala.collection.JavaConverters._
import scala.util.control.Exception._
import scala.util.Try
import sys.process._
import wvlet.log.LogSupport


import java.nio.file.{Paths, Files}
import java.nio.charset.StandardCharsets

import java.time._
import java.time.format._
import java.io.File

object nytimes {
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

  def briefingsData(doc: nodes.Document): Option[nodes.Element] = 
    doc.select("script").asScala
      .filter(_.data().startsWith("window.__preloadedData = "))
      .take(2)
      .headOption

  def write(path: String, txt: String): Unit = {
    Files.write(Paths.get(path), txt.getBytes(StandardCharsets.UTF_8))
  }

  def downloadAndProcessBriefing(targetDirectory: Path, briefing: Briefing): Unit = {
    val doc = briefing.fetchDoc
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
    info(f"Found ${briefings.length}")

    briefings
      .sortBy(_.date)
      .reverse
      .take(2)
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
