package scalixObject

import scalixObject.{Actor, Movie}
import scalixObject.APIClient

class ActorAnalytics(apiClient: APIClient) {

  def findMostFrequentActorPairs(actors: List[Actor]): List[((String, String), Int)] = {
    // Step 1: Invert mapping to associate movies with the actors who acted in them
    val movieToActors: Map[Int, Set[Int]] = actors.flatMap { actor =>
        actor.getMovies.map(movie => (movie.id, actor.id)) // Create pairs (movieId, actorId)
      }.groupBy(_._1) // Group by movie ID
      .view.mapValues(_.map(_._2).toSet) // Map each movie ID to the set of actor IDs
      .toMap

    // Step 2: Generate all unique pairs of actors for each movie
    val actorPairsWithCount: Map[(Int, Int), Int] = movieToActors.values
      .flatMap { actorIds =>
        for {
          actor1 <- actorIds
          actor2 <- actorIds if actor1 < actor2 // Ensure each pair is unique (actor1 < actor2)
        } yield (actor1, actor2)
      }
      .groupBy(identity) // Group identical pairs
      .view.mapValues(_.size) // Count occurrences of each pair
      .toMap

    // Step 3: Convert actor IDs to names and sort pairs by frequency
    actorPairsWithCount.toList
      .map { case ((actor1Id, actor2Id), count) =>
        // Retrieve actor names from the list of actors, default to "Actor ID" if not found
        val actor1Name = getActorNameById(actors, actor1Id).getOrElse(s"Actor $actor1Id")
        val actor2Name = getActorNameById(actors, actor2Id).getOrElse(s"Actor $actor2Id")
        ((actor1Name, actor2Name), count) // Return the pair with their occurrence count
      }
      .sortBy(-_._2) // Sort pairs by frequency in descending order
  }

  // Helper function to find an actor's name by their ID
  private def getActorNameById(actors: List[Actor], actorId: Int): Option[String] = {
    actors.find(_.id == actorId).map(actor => s"${actor.fullName.firstName} ${actor.fullName.lastName}")
  }
}
