package scalix

import scala.io.Source
import java.io.{File, PrintWriter}
import java.net.URLEncoder
import org.json4s._
import org.json4s.native.JsonMethods._

object TMDBClientNew extends App {

  // Configuration de l'API
  val apiKey = "711f1ecf07ccb1fdfc0885804b343360"
  val apiBase = "https://api.themoviedb.org/3"
  implicit val formats: DefaultFormats.type = DefaultFormats

  // Cache primaire (en mémoire)
  private var actorIdCache: Map[(String, String), Int] = Map()
  private var actorMoviesCache: Map[Int, Set[(Int, String)]] = Map()
  private var movieDirectorCache: Map[Int, (Int, String)] = Map()

  // Méthode utilitaire pour lire/écrire dans des fichiers
  private def readFile(filename: String): Option[String] = {
    val file = new File(filename)
    if (file.exists()) Some(Source.fromFile(file).mkString)
    else None
  }

  private def writeFile(filename: String, contents: String): Unit = {
    val out = new PrintWriter(new File(filename))
    try out.print(contents)
    finally out.close()
  }

  // Méthode utilitaire pour effectuer une requête GET
  private def get(url: String): String = {
    val source = Source.fromURL(url)
    try source.mkString
    finally source.close()
  }

  // Méthode pour trouver l'ID d'un acteur avec cache
  def findActorId(name: String, surname: String): Option[Int] = {
    val cacheKey = (name, surname)
    actorIdCache.get(cacheKey) match {
      case Some(id) =>
        Some(id) // Cache primaire
      case None =>
        val query = URLEncoder.encode(s"$name $surname", "UTF-8")
        val url = s"$apiBase/search/person?api_key=$apiKey&query=$query"
        val response = parse(get(url))

        val actorIdOpt = (response \ "results").extract[List[JValue]].headOption.flatMap { result =>
          (result \ "id").extractOpt[Int]
        }

        actorIdOpt.foreach { id =>
          actorIdCache += cacheKey -> id // Mise à jour du cache primaire
        }
        actorIdOpt
    }
  }

  // Méthode pour récupérer les films d'un acteur avec cache
  def findActorMovies(actorId: Int): Set[(Int, String)] = {
    val cacheFilename = s"data/actor$actorId.json"
    actorMoviesCache.get(actorId) match {
      case Some(movies) =>
        movies // Cache primaire
      case None =>
        readFile(cacheFilename) match {
          case Some(contents) =>
            val response = parse(contents)
            val movies = (response \ "cast").extract[List[JValue]].map { movie =>
              ((movie \ "id").extract[Int], (movie \ "title").extract[String])
            }.toSet
            actorMoviesCache += actorId -> movies // Mise à jour du cache primaire
            movies
          case None =>
            val url = s"$apiBase/person/$actorId/movie_credits?api_key=$apiKey"
            val contents = get(url)
            writeFile(cacheFilename, contents) // Mise à jour du cache secondaire
            val response = parse(contents)
            val movies = (response \ "cast").extract[List[JValue]].map { movie =>
              ((movie \ "id").extract[Int], (movie \ "title").extract[String])
            }.toSet
            actorMoviesCache += actorId -> movies // Mise à jour du cache primaire
            movies
        }
    }
  }

  // Méthode pour trouver le réalisateur d'un film avec cache
  def findMovieDirector(movieId: Int): Option[(Int, String)] = {
    val cacheFilename = s"data/movie$movieId.json"
    movieDirectorCache.get(movieId) match {
      case Some(director) =>
        Some(director) // Cache primaire
      case None =>
        readFile(cacheFilename) match {
          case Some(contents) =>
            val response = parse(contents)
            val directorOpt = (response \ "crew").extract[List[JValue]].find { crewMember =>
              (crewMember \ "job").extract[String] == "Director"
            }.map { director =>
              ((director \ "id").extract[Int], (director \ "name").extract[String])
            }
            directorOpt.foreach { director =>
              movieDirectorCache += movieId -> director // Mise à jour du cache primaire
            }
            directorOpt
          case None =>
            val url = s"$apiBase/movie/$movieId/credits?api_key=$apiKey"
            val contents = get(url)
            writeFile(cacheFilename, contents) // Mise à jour du cache secondaire
            val response = parse(contents)
            val directorOpt = (response \ "crew").extract[List[JValue]].find { crewMember =>
              (crewMember \ "job").extract[String] == "Director"
            }.map { director =>
              ((director \ "id").extract[Int], (director \ "name").extract[String])
            }
            directorOpt.foreach { director =>
              movieDirectorCache += movieId -> director // Mise à jour du cache primaire
            }
            directorOpt
        }
    }
  }

  // Classe pour représenter un nom complet
  case class FullName(firstName: String, lastName: String)

  // Méthode collaboration reste inchangée
  def collaboration(actor1: FullName, actor2: FullName): Set[(String, String)] = {
    val actor1IdOpt = findActorId(actor1.firstName, actor1.lastName)
    val actor2IdOpt = findActorId(actor2.firstName, actor2.lastName)

    for {
      actor1Id <- actor1IdOpt.toSet
      actor2Id <- actor2IdOpt.toSet
      actor1Movies = findActorMovies(actor1Id)
      actor2Movies = findActorMovies(actor2Id)
      commonMovies = actor1Movies.intersect(actor2Movies)
      movieDirectors <- commonMovies.flatMap { case (movieId, movieTitle) =>
        findMovieDirector(movieId).map(director => (director._2, movieTitle))
      }
    } yield movieDirectors
  }

  // Exemple d'utilisation
  val actor1 = FullName("Leonardo", "DiCaprio")
  val actor2 = FullName("Kate", "Winslet")
  val result = collaboration(actor1, actor2)

  println(s"Collaborations between ${actor1.firstName} ${actor1.lastName} and ${actor2.firstName} ${actor2.lastName}:")
  result.foreach { case (director, movie) =>
    println(s" - Movie: $movie, Director: $director")
  }
  
}
