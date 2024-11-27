import org.json4s._
import org.json4s.native.JsonMethods._
import org.json4s.native.JsonMethods.{parse => jsonParse}
import scala.io.Source
import java.io.{File, PrintWriter}

case class FullName(firstName: String, lastName: String)

object Scalix extends App {
  implicit val formats: Formats = DefaultFormats

  object Constants {
    val key: String = "33ffb9eb0bee38fe44db188c48c41728"
    val dataDir: String = "./data"
  }

  // Création du répertoire pour stocker les fichiers JSON
  new File(Constants.dataDir).mkdirs()

  // Cache primaire (en mémoire)
  private val actorCache: collection.mutable.Map[(String, String), Option[Int]] = collection.mutable.Map()
  private val movieCache: collection.mutable.Map[Int, Set[(Int, String)]] = collection.mutable.Map()
  private val directorCache: collection.mutable.Map[Int, Option[(Int, String)]] = collection.mutable.Map()

  // Fonction générique pour lire ou écrire dans le cache secondaire
  private def readFromFile(filename: String): Option[String] =
    try {
      Some(Source.fromFile(filename).mkString)
    } catch {
      case _: Exception => None
    }

  private def writeToFile(filename: String, content: String): Unit = {
    val writer = new PrintWriter(filename)
    try writer.write(content)
    finally writer.close()
  }

  // Fonction générique pour effectuer une requête avec gestion des deux caches
  def fetchAndCache(url: String, cacheFile: String): JValue = {
    readFromFile(cacheFile)
      .map(jsonParse(_)) // Lecture depuis le cache secondaire
      .getOrElse {
        val contents = Source.fromURL(url).mkString
        writeToFile(cacheFile, contents) // Mise à jour du cache secondaire
        jsonParse(contents)
      }
  }

  // Requête TMDB générique avec gestion de la clé API
  def makeRequest(endpoint: String, parameters: Map[String, String] = Map.empty, filename: Option[String] = None): JValue = {
    val queryParams = parameters.map { case (k, v) => s"$k=${v.replace(" ", "%20")}" }.mkString("&")
    val url = s"https://api.themoviedb.org/3/$endpoint?api_key=${Constants.key}&$queryParams"
    if (filename.isDefined) {
      val cacheFile = s"${Constants.dataDir}/${filename.get}.json"
      fetchAndCache(url, cacheFile)
    } else {
      jsonParse(Source.fromURL(url).mkString)
    }
  }

  // Trouver l'ID d'un acteur avec cache primaire
  def findActorId(fullName: FullName): Option[Int] = {
    actorCache.getOrElseUpdate((fullName.firstName, fullName.lastName), {
      val results = (makeRequest("search/person", Map("query" -> s"${fullName.firstName} ${fullName.lastName}")) \ "results").extract[List[JValue]]
      results.headOption.map(result => (result \ "id").extract[Int])
    })
  }

  // Trouver les films d'un acteur avec gestion de cache secondaire via makeRequest
  def findActorMovies(actorId: Int): Set[(Int, String)] = {
    movieCache.getOrElseUpdate(actorId, {
      val results = (makeRequest(s"person/$actorId/movie_credits", filename = Some(s"actor$actorId")) \ "cast").extract[List[JValue]]
      results.map(movie => ((movie \ "id").extract[Int], (movie \ "title").extract[String])).toSet
    })
  }

  // Trouver le réalisateur d'un film avec gestion de cache secondaire via makeRequest
  def findMovieDirector(movieId: Int): Option[(Int, String)] = {
    directorCache.getOrElseUpdate(movieId, {
      val results = (makeRequest(s"movie/$movieId/credits", filename = Some(s"movie$movieId")) \ "crew").extract[List[JValue]]
      results.find(member => (member \ "job").extract[String] == "Director")
        .map(d => ((d \ "id").extract[Int], (d \ "name").extract[String]))
    })
  }

  // Trouver les collaborations entre deux acteurs avec cache primaire et secondaire
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
