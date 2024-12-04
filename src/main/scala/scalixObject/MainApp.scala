package scalixObject

import scalixObject.{Actor, FullName}
import scalixObject.APIClient
import scalixObject.ActorAnalytics

object MainApp extends App {
  val apiKey = "33ffb9eb0bee38fe44db188c48c41728"
  val apiClient = new APIClient(apiKey)

  try {
    val tomHanks = new Actor(FullName("Tom", "Hanks"), apiClient)
    val tomCruise = new Actor(FullName("Tom", "Cruise"), apiClient)

    // Movies of actor
    println(s"Movies of ${tomHanks.fullName.firstName} ${tomHanks.fullName.lastName}:")
    tomHanks.getMovies.foreach(movie => println(s"- ${movie.getTitle}"))

    // Collaboration between actors
    println(s"Collaborations between ${tomHanks.fullName.lastName} and ${tomCruise.fullName.lastName}:")
    tomHanks.findCollaborations(tomCruise).foreach {
      case (director, title) => println(s"Director: $director, Movie: $title")
    }

    val actors = List(
      new Actor(FullName("Tom", "Hanks"), apiClient),
      new Actor(FullName("Tom", "Cruise"), apiClient),
      new Actor(FullName("Leonardo", "DiCaprio"), apiClient),
      new Actor(FullName("Robert", "De Niro"), apiClient)
    )


    // Most frequent actor pairs
    val analytics = new ActorAnalytics(apiClient)
    val frequentPairs = analytics.findMostFrequentActorPairs(actors)

    println("Most frequent actor pairs:")
    frequentPairs.foreach { case ((actor1, actor2), count) =>
      println(f"$actor1%-20s and $actor2%-20s appeared together in $count movies")
    }


  } catch {
    case e: NoSuchElementException => println(e.getMessage)
  }
}