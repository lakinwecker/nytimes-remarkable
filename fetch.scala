import cats.implicits._
import io.chrisdavenport.cats.time._
import org.jsoup._
import scala.collection.JavaConverters._
import scala.util.control.Exception._
import scala.util.{Try}

import java.time._
import java.time.format._

object nytimes {
  val baseUrl = "https://www.nytimes.com/"
  val briefingsListings = "spotlight/us-briefing"

  def dbg[A](f: A): A = { println(f); f }
  def urlToName(s: String) = s.replaceAllLiterally(".html", "").split("-").map(_.capitalize).mkString(" ")

  case class Briefing(date: LocalDate, url: String, name: String)

  def toBriefing(node: nodes.Element): Option[Briefing] = {
    val url = node.attr("href")
    val dateFormat = DateTimeFormatter.ofPattern("yyyy/MM/dd")
    val dateString = url.stripPrefix("/").split('/').take(3).mkString("/")
    for {
       date <- Try{LocalDate.parse(dateString, dateFormat)}.toOption
       name <- url.split('/').lastOption.map(urlToName)
    } yield new Briefing(date, url, name)

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
}

object fetch {
  def main(args: Array[String]): Unit = {
    val doc = nytimes.fetchBriefingsDoc
    println(nytimes.parseBriefings(doc).map(_.url))
    println(nytimes.parseBriefings(doc).map(_.date))
    println(nytimes.parseBriefings(doc))
  }
}
