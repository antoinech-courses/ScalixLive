package scalixObject

import org.json4s._
import org.json4s.native.JsonMethods._
import org.json4s.native.JsonMethods.{parse => jsonParse}
import scala.io.Source
import java.io.{File, PrintWriter}

class APIClient(apiKey: String, dataDir: String = "./data") {
  implicit val formats: Formats = DefaultFormats

  // Ensure data directory exists
  new File(dataDir).mkdirs()

  private def readFromFile(filename: String): Option[String] = {
    val file = new File(filename)
    if (file.exists && file.isFile) {
      val source = Source.fromFile(file)
      try {
        Some(source.mkString)
      } catch {
        case _: Exception => None
      } finally {
        source.close()
      }
    } else {
      None
    }
  }

  private def writeToFile(filename: String, content: String): Unit = {
    val writer = new PrintWriter(filename)
    try writer.write(content) finally writer.close()
  }

  private def fetchAndCache(url: String, cacheFile: String): JValue = {
    readFromFile(cacheFile).map(jsonParse(_)).getOrElse {
      val source = Source.fromURL(url)
      try {
        val contents = source.mkString
        writeToFile(cacheFile, contents)
        jsonParse(contents)
      } finally source.close()
    }
  }

  def makeRequest(endpoint: String, parameters: Map[String, String] = Map.empty, cacheFile: Option[String] = None): JValue = {
    val queryParams = parameters.map { case (k, v) => s"$k=${v.replace(" ", "%20")}" }.mkString("&")
    val url = s"https://api.themoviedb.org/3/$endpoint?api_key=$apiKey&$queryParams"

    cacheFile.map(file => fetchAndCache(url, s"$dataDir/$file.json")).getOrElse {
      val source = Source.fromURL(url)
      try parse(source.mkString) finally source.close()
    }
  }
}
