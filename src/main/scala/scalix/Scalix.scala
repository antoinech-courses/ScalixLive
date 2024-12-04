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

  // Project constants
  private object Constants {
    val key: String = "33ffb9eb0bee38fe44db188c48c41728"
    val dataDir: String = "./data"
  }

  // Create folder for cached data
  new File(Constants.dataDir).mkdirs()

  // Primary cache

  // Actor cache : associate an actor id option to a first name and last name
  private var actorCache: Map[(String, String), Option[Int]] = Map()

  // Movie cache : associate a set of movies (id and title) to an actor id
  private var movieCache: Map[Int, Set[(Int, String)]] = Map()

  // Director cache : associate a director (id and name) to a movie id
  private var directorCache: Map[Int, Option[(Int, String)]] = Map()

  // Function to read from file and return content
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

  // Function to write to file
  private def writeToFile(filename: String, content: String): Unit = {
    val writer = new PrintWriter(filename)
    try writer.write(content)
    finally writer.close()
  }

  // Function to fetch and cache data
  private def fetchAndCache(url: String, cacheFile: String): JValue = {
    readFromFile(cacheFile)
      .map(jsonParse(_)) // Read from secondary cache
      .getOrElse {
        val source = Source.fromURL(url)
        try {
          val contents = source.mkString
          writeToFile(cacheFile, contents) // Update secondary cache
          jsonParse(contents)
        } finally {
          source.close()
        }
      }
  }

  // Generic TMDB request with API key management
  private def makeRequest(endpoint: String, parameters: Map[String, String] = Map.empty, filename: Option[String] = None): JValue = {
    // Generate string of parameters to include in the request URL. This enables to add the API key easily
    val queryParams = parameters.map { case (k, v) => s"$k=${v.replace(" ", "%20")}" }.mkString("&")
    // Request URL
    val url = s"https://api.themoviedb.org/3/$endpoint?api_key=${Constants.key}&$queryParams"

    // If filename is passed, we will cache the data in a file
    if (filename.isDefined) {
      val cacheFile = s"${Constants.dataDir}/${filename.get}.json"
      fetchAndCache(url, cacheFile) // We get data from cache file or create it
    } else {
      // Directly fetch data
      val source = Source.fromURL(url)
      try {
        jsonParse(source.mkString)
      } finally {
        source.close()
      }
    }
  }

  // Find actor ID with full name
  private def findActorId(fullName: FullName): Option[Int] = {
    actorCache.getOrElse((fullName.firstName, fullName.lastName), {
      // If not in primary cache

      // Request URL with parameter and result extract
      val results = (makeRequest("search/person", Map("query" -> s"${fullName.firstName} ${fullName.lastName}")) \ "results").extract[List[JValue]]
      val actorIdOpt = results.headOption.map(result => (result \ "id").extract[Int]) // First found id

      // Primary cache update
      actorCache += (fullName.firstName -> fullName.lastName) -> actorIdOpt
      actorIdOpt
    })
  }

  // Find movies of actor with actor id
  private def findActorMovies(actorId: Int): Set[(Int, String)] = {
    movieCache.getOrElse(actorId, {
      // If not in primary cache
      // Get or fetch from file cache
      val results = (makeRequest(s"person/$actorId/movie_credits", filename = Some(s"actor$actorId")) \ "cast").extract[List[JValue]]
      val movies = results.map(movie => ((movie \ "id").extract[Int], (movie \ "title").extract[String])).toSet

      // Update primary cache
      movieCache += actorId -> movies
      movies
    })
  }

  // Find movie director with movie id
  private def findMovieDirector(movieId: Int): Option[(Int, String)] = {
    directorCache.getOrElse(movieId, {
      // If not in primary cache
      // Get or fetch from file cache
      val results = (makeRequest(s"movie/$movieId/credits", filename = Some(s"movie$movieId")) \ "crew").extract[List[JValue]]
      val directorOpt = results.find(member => (member \ "job").extract[String] == "Director")
        .map(d => ((d \ "id").extract[Int], (d \ "name").extract[String]))

      // Update primary cache
      directorCache += movieId -> directorOpt
      directorOpt
    })
  }

  // Find collaboration between two actors given their full names
  private def collaboration(actor1: FullName, actor2: FullName): Set[(String, String)] = {
    for {
      // Ids of actors
      id1 <- findActorId(actor1).toSet
      id2 <- findActorId(actor2).toSet
      // Movies of actors
      movies1 = findActorMovies(id1)
      movies2 = findActorMovies(id2)

      // Common movies of actors
      commonMovies = movies1.intersect(movies2)

      // Get movieId by destructuration
      (movieId, movieTitle) <- commonMovies
      // Get director
      director <- findMovieDirector(movieId)
    } yield (director._2, movieTitle) // Return director's name and movie title
  }

  private def findMostFrequentActorPairs(): List[((String, String), Int)] = {
    // Step 1 : Invert mapping of cache to associate actors to movies
    val movieToActors: Map[Int, Set[Int]] = movieCache.toSeq
      .flatMap { case (actorId, movies) => movies.map(movie => (movie._1, actorId)) } // (movieId, actorId)
      .groupBy(_._1) // Group by movie ID
      .view.mapValues(_.map(_._2).toSet) // Obtain a set of actors for each movie
      .toMap

    // Step 2 : Generate pairs of actors for each movie
    val actorPairsWithCount: Map[(Int, Int), Int] = movieToActors.values
      .flatMap { actors =>
        for {
          actor1 <- actors
          actor2 <- actors if actor1 < actor2 // Avoid duplicates by comparing ids
        } yield (actor1, actor2)
      }
      .groupBy(identity) // Group pairs
      .view.mapValues(_.size) // Count occurrences of each pairs
      .toMap

    // Step 3 : Convert actor IDs to names and sort by frequency
    actorPairsWithCount.toList
      .map { case ((actor1Id, actor2Id), count) =>
        // Get names from cache or default to "Actor ID" if not available
        val actor1Name = actorCache.collectFirst { case ((firstName, lastName), Some(id)) if id == actor1Id => s"$firstName $lastName" }.getOrElse(s"Actor $actor1Id")
        val actor2Name = actorCache.collectFirst { case ((firstName, lastName), Some(id)) if id == actor2Id => s"$firstName $lastName" }.getOrElse(s"Actor $actor2Id")
        ((actor1Name, actor2Name), count) // Generate pairs with names and count
      }
      .sortBy(-_._2) // Sort by frequency in descending order
  }


  // Tests and results
  println("Hello from Scalix!")

  println("Find actor by id. Id of Tom Hanks is : " + findActorId(FullName("Tom", "Hanks")).get)

  println("Find movies of actor. Movies of Tom Hanks are : ")
  findActorMovies(findActorId(FullName("Tom", "Hanks")).get).foreach(movie => println("Id: " + movie._1 + " Title: " + movie._2))

  println("Find movie director. Director of movie with id 13 is : " + findMovieDirector(13).get._2)

  println("Collaboration between Tom Hanks and Tom Cruise : ")
  collaboration(FullName("Tom", "Hanks"), FullName("Tom", "Cruise")).foreach(movie => println("Director: " + movie._1 + " Movie: " + movie._2))

  // Load some cache data
  collaboration(FullName("Leonardo", "DiCaprio"), FullName("Kate", "Winslet"))
  collaboration(FullName("Robert", "De Niro"), FullName("Al", "Pacino"))
  collaboration(FullName("Brad", "Pitt"), FullName("Angelina", "Jolie"))
  collaboration(FullName("Johnny", "Depp"), FullName("Helena", "Bonham Carter"))
  collaboration(FullName("Tom", "Hanks"), FullName("Meg", "Ryan"))

  println("Most frequent actor pairs : ")
  findMostFrequentActorPairs().foreach(pair => println("Actor " + pair._1._1 + " and actor " + pair._1._2 + " : " + pair._2 + " times"))
}