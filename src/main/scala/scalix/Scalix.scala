package scalix

import org.json4s._
import org.json4s.native.JsonMethods._
import org.json4s.native.JsonMethods.{parse => jsonParse}
import scala.io.Source
import java.io.{File, PrintWriter}

// Case class representing the full name of a person
case class FullName(firstName: String, lastName: String)

object Scalix extends App {
  implicit val formats: Formats = DefaultFormats

  private object Constants {
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
  private def fetchAndCache(url: String, cacheFile: String): JValue = {
    readFromFile(cacheFile)
      .map(jsonParse(_)) // Lecture depuis le cache secondaire
      .getOrElse {
        val contents = Source.fromURL(url).mkString
        writeToFile(cacheFile, contents) // Mise à jour du cache secondaire
        jsonParse(contents)
      }
  }

  // Requête TMDB générique avec gestion de la clé API
  private def makeRequest(endpoint: String, parameters: Map[String, String] = Map.empty, filename: Option[String] = None): JValue = {
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
  private def findActorId(fullName: FullName): Option[Int] = {
    actorCache.getOrElseUpdate((fullName.firstName, fullName.lastName), {
      val results = (makeRequest("search/person", Map("query" -> s"${fullName.firstName} ${fullName.lastName}")) \ "results").extract[List[JValue]]
      results.headOption.map(result => (result \ "id").extract[Int])
    })
  }

  // Trouver les films d'un acteur avec gestion de cache secondaire via makeRequest
  private def findActorMovies(actorId: Int): Set[(Int, String)] = {
    movieCache.getOrElseUpdate(actorId, {
      val results = (makeRequest(s"person/$actorId/movie_credits", filename = Some(s"actor$actorId")) \ "cast").extract[List[JValue]]
      results.map(movie => ((movie \ "id").extract[Int], (movie \ "title").extract[String])).toSet
    })
  }

  // Trouver le réalisateur d'un film avec gestion de cache secondaire via makeRequest
  private def findMovieDirector(movieId: Int): Option[(Int, String)] = {
    directorCache.getOrElseUpdate(movieId, {
      val results = (makeRequest(s"movie/$movieId/credits", filename = Some(s"movie$movieId")) \ "crew").extract[List[JValue]]
      results.find(member => (member \ "job").extract[String] == "Director")
        .map(d => ((d \ "id").extract[Int], (d \ "name").extract[String]))
    })
  }

  // Trouver les collaborations entre deux acteurs avec cache primaire et secondaire
  private def collaboration(actor1: FullName, actor2: FullName): Set[(String, String)] = {
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

  private def findMostFrequentActorPairs(): List[((String, String), Int)] = {
    // Étape 1 : Inverser le mapping pour associer chaque film à son ensemble d'acteurs
    val movieToActors: Map[Int, Set[Int]] = movieCache.toSeq
      .flatMap { case (actorId, movies) => movies.map(movie => (movie._1, actorId)) } // (movieId, actorId)
      .groupBy(_._1) // Grouper par ID de film
      .view.mapValues(_.map(_._2).toSet) // Obtenir un Set d'acteurs par film
      .toMap

    // Étape 2 : Trouver les paires d'acteurs pour chaque film
    val actorPairsWithCount: Map[(Int, Int), Int] = movieToActors.values
      .flatMap { actors =>
        for {
          actor1 <- actors
          actor2 <- actors if actor1 < actor2 // Générer des paires uniques
        } yield (actor1, actor2)
      }
      .groupBy(identity) // Grouper les paires
      .view.mapValues(_.size) // Compter les occurrences de chaque paire
      .toMap

    // Étape 3 : Associer les IDs d'acteurs à leurs noms et trier les résultats
    actorPairsWithCount.toList
      .map { case ((actor1Id, actor2Id), count) =>
        val actor1Name = actorCache.collectFirst { case ((firstName, lastName), Some(id)) if id == actor1Id => s"$firstName $lastName" }.getOrElse(s"Actor $actor1Id")
        val actor2Name = actorCache.collectFirst { case ((firstName, lastName), Some(id)) if id == actor2Id => s"$firstName $lastName" }.getOrElse(s"Actor $actor2Id")
        ((actor1Name, actor2Name), count)
      }
      .sortBy(-_._2) // Trier par fréquence décroissante
  }

  // Exemple d'utilisation
  println("Hello, Scalix!")
  val result = collaboration(FullName("Tom", "Hanks"), FullName("Tom", "Cruise"))
  println(result)

  val result1 = collaboration(FullName("Leonardo", "DiCaprio"), FullName("Kate", "Winslet"))
  println(result1)

  val result2 = collaboration(FullName("Robert", "De Niro"), FullName("Al", "Pacino"))
  println(result2)

  val result3 = collaboration(FullName("Brad", "Pitt"), FullName("Angelina", "Jolie"))
  println(result3)

  val result4 = collaboration(FullName("Johnny", "Depp"), FullName("Helena", "Bonham Carter"))
  println(result4)

  val result5 = collaboration(FullName("Tom", "Hanks"), FullName("Meg", "Ryan"))
  println(result5)
  println(movieCache)
  val mostFrequentPairs = findMostFrequentActorPairs()
  println(mostFrequentPairs.take(10))
}
