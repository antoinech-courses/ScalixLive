package scalix

import org.json4s._
import org.json4s.native.JsonMethods._
import scala.io.Source

case class FullName(firstName: String, lastName: String)

object Scalix extends App {
  implicit val formats: Formats = DefaultFormats

  object Constants {
    val key: String = "33ffb9eb0bee38fe44db188c48c41728"
  }

  // Fonction générique pour effectuer une requête à l'API et parser le résultat en JSON
  def makeRequest(endpoint: String, parameters: Map[String, String] = Map.empty): Either[String, JValue] = {
    val queryParams = parameters.map { case (k, v) => s"$k=${v.replace(" ", "%20")}" }.mkString("&")
    val url = s"https://api.themoviedb.org/3/$endpoint?api_key=${Constants.key}&$queryParams"
    try {
      val source = Source.fromURL(url).mkString
      Right(parse(source))
    } catch {
      case e: Exception => Left(s"Error fetching data from $url: ${e.getMessage}")
    }
  }

  // Trouver l'ID d'un acteur
  def findActorId(fullName: FullName): Option[Int] = {
    makeRequest("search/person", Map("query" -> s"${fullName.firstName} ${fullName.lastName}")) match {
      case Right(json) =>
        val results = (json \ "results").extract[List[JValue]]
        results.headOption.map(result => (result \ "id").extract[Int])
      case Left(_) => None
    }
  }

  // Trouver les films associés à un acteur
  def findActorMovies(actorId: Int): Set[(Int, String)] = {
    makeRequest(s"person/$actorId/movie_credits") match {
      case Right(json) =>
        val movies = (json \ "cast").extract[List[JValue]]
        movies.map(movie => ((movie \ "id").extract[Int], (movie \ "title").extract[String])).toSet
      case Left(_) => Set.empty
    }
  }

  // Trouver le réalisateur d'un film
  def findMovieDirector(movieId: Int): Option[(Int, String)] = {
    makeRequest(s"movie/$movieId/credits") match {
      case Right(json) =>
        val crew = (json \ "crew").extract[List[JValue]]
        crew.find(member => (member \ "job").extract[String] == "Director")
          .map(d => ((d \ "id").extract[Int], (d \ "name").extract[String]))
      case Left(_) => None
    }
  }

  // Trouver les collaborations entre deux acteurs
  def collaboration(actor1: FullName, actor2: FullName): Set[(String, String)] = {
    for {
      id1 <- findActorId(actor1).toSet
      id2 <- findActorId(actor2).toSet
      movies1 = findActorMovies(id1)
      movies2 = findActorMovies(id2)
      commonMovies = movies1.intersect(movies2)
      (movieId, movieTitle) <- commonMovies
      director <- findMovieDirector(movieId)
    } yield (director._2, movieTitle)
  }

  // Exemple d'utilisation
  println("Hello, Scalix!")
  val result = collaboration(FullName("Tom", "Hanks"), FullName("Tom", "Cruise"))
  println(result)
}
