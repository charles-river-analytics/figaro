/*
 * SimpleMovie.scala
 * A simple probabilistc relational model example with single-valued attributes.
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Jan 1, 2009
 * 
 * Copyright 2017 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.example

import com.cra.figaro.algorithm.factored._
import com.cra.figaro.library.compound._
import com.cra.figaro.util
import com.cra.figaro.language._
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.library.atomic.discrete._

/**
 * A simple probabilistc relational model example with single-valued attributes.
 */
object SimpleMovie {
  private val random = new scala.util.Random()

  private class Actor {
    val famous = Flip(0.1)
  }

  private class Movie {
    val quality = Select(0.3 -> 'low, 0.5 -> 'medium, 0.2 -> 'high)
  }

  private class Appearance(val actor: Actor, val movie: Movie) {
    def probAward(quality: Symbol, famous: Boolean) =
      (quality, famous) match {
        case ('low, false) => 0.001
        case ('low, true) => 0.01
        case ('medium, false) => 0.01
        case ('medium, true) => 0.05
        case ('high, false) => 0.05
        case ('high, true) => 0.2
      }
    val award = SwitchingFlip(Apply(movie.quality, actor.famous, (q: Symbol, f: Boolean) => probAward(q, f)))
  }

  private val numActors = 3 // 200
  private val numMovies = 2 // 100
  private val numAppearances = 3 // 300
  private val actor1 = new Actor
  private val actor2 = new Actor
  private val actor3 = new Actor
  private val movie1 = new Movie
  private val movie2 = new Movie
  private val appearance1 = new Appearance(actor1, movie1)
  private val appearance2 = new Appearance(actor2, movie2)
  private val appearance3 = new Appearance(actor3, movie2)
  private val otherActors = Array.fill(numActors - 3)(new Actor)
  private val otherMovies = Array.fill(numMovies - 2)(new Movie)
  private val actors = otherActors ++ Array(actor1, actor2, actor3)
  private val movies = otherMovies ++ Array(movie1, movie2)
  private val otherAppearances =
    Array.fill(numAppearances - 3)(new Appearance(actors(random.nextInt(numActors)), movies(random.nextInt(numMovies))))
  private val appearances = otherAppearances ++ Array(appearance1, appearance2, appearance3)

  // Ensure that exactly one appearance gets an award.
  private def uniqueAwardCondition(awards: List[Boolean]) = awards.count((b: Boolean) => b) == 1
  private val allAwards: Element[List[Boolean]] = Inject(appearances.map(_.award): _*)
  allAwards.setCondition(uniqueAwardCondition)

  // A proposal either proposes to switch the awardee to another awardee or proposes the properties of a movie or
  // an actor.
  private def chooseScheme(): ProposalScheme = {
    DisjointScheme(
      (0.5, () => switchAwards()),
      (0.25, () => ProposalScheme(actors(random.nextInt(numActors)).famous)),
      (0.25, () => ProposalScheme(movies(random.nextInt(numMovies)).quality)))
  }

  /*
   * It's possible that as a result of other attributes changing, an appearance becomes awarded or unawarded.
   * Therefore, we have to take this into account in the proposal scheme.
   */
  private def switchAwards(): ProposalScheme = {
    val (awarded, unawarded) = appearances.partition(_.award.value)
    awarded.length match {
      case 1 =>
        val other = unawarded(random.nextInt(numAppearances - 1))
        ProposalScheme(awarded(0).award, other.award)
      case 0 =>
        ProposalScheme(appearances(random.nextInt(numAppearances)).award) // make something unawarded awarded
      case _ =>
        ProposalScheme(awarded(random.nextInt(awarded.length)).award)
    }
  }

  def main(args: Array[String]): Unit = {
    actor3.famous.observe(true)
    movie2.quality.observe('high)

    // We first make sure the initial state satisfies the unique award condition, and then make sure that all
    // subsequent proposals keep that condition.
    appearances.foreach(_.award.randomness = 0.0)
    appearances(random.nextInt(numAppearances)).award.randomness = 1.0
    appearances.foreach(appearance =>
      appearance.award.value = appearance.award.generateValue(appearance.award.randomness))
    allAwards.generate()

    val alg = MetropolisHastings(200000, chooseScheme, 5000, appearance1.award, appearance2.award, appearance3.award)

    util.timed(alg.start(), "Inference")
    alg.stop()
    println(alg.probability(appearance3.award, true))
    alg.kill()
  }
}
