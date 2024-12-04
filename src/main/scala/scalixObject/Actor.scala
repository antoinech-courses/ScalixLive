package scalixObject
import scalixObject.APIClient
import org.json4s._
import org.json4s.native.JsonMethods._
import org.json4s.native.JsonMethods.{parse => jsonParse}


case class FullName(firstName: String, lastName: String)

class Actor(val fullName: FullName, apiClient: APIClient) {
  implicit val formats: Formats = DefaultFormats

  val id: Int = findActorId(fullName).getOrElse {
    throw new NoSuchElementException(s"Actor not found: ${fullName.firstName} ${fullName.lastName}")
  }

  def getMovies: Set[Movie] = {
    val results = (apiClient.makeRequest(s"person/$id/movie_credits", cacheFile = Some(s"actor$id")) \ "cast").extract[List[JValue]]
    results.map(movie => new Movie((movie \ "id").extract[Int], apiClient)).toSet
  }

  def findCollaborations(otherActor: Actor): Set[(String, String)] = {
    val movies1 = this.getMovies.map(_.id)
    val movies2 = otherActor.getMovies.map(_.id)
    val commonMovies = movies1.intersect(movies2)

    commonMovies.flatMap { movieId =>
      val movie = new Movie(movieId, apiClient)
      movie.getDirector.map(director => (director.fullName.firstName, movie.getTitle))
    }
  }

  private def findActorId(fullName: FullName): Option[Int] = {
    val results = (apiClient.makeRequest("search/person", Map("query" -> s"${fullName.firstName} ${fullName.lastName}")) \ "results").extract[List[JValue]]
    results.headOption.map(result => (result \ "id").extract[Int])
  }
}