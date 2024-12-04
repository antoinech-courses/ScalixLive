package scalixObject

import scalixObject.APIClient
import org.json4s._
import org.json4s.native.JsonMethods._

class Movie(val id: Int, apiClient: APIClient) {
  implicit val formats: Formats = DefaultFormats
  private var titleCache: Option[String] = None

  def getTitle: String = {
    if (titleCache.isEmpty) {
      val movieDetails = apiClient.makeRequest(s"movie/$id", cacheFile = Some(s"movie_detail$id"))
      titleCache = Some((movieDetails \ "title").extract[String])
    }
    titleCache.get
  }

  def getDirector: Option[Actor] = {
    val results = (apiClient.makeRequest(s"movie/$id/credits", cacheFile = Some(s"movie$id")) \ "crew").extract[List[JValue]]
    results.find(member => (member \ "job").extract[String] == "Director")
      .map(d => new Actor(FullName((d \ "name").extract[String], ""), apiClient))
  }
}