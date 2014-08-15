/*
 * MutableMovieTest.scala  
 * Movie with inverse attributes test.
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Jan 1, 2009
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.test.example

import org.scalatest.Matchers
import org.scalatest.WordSpec
import com.cra.figaro.algorithm._
import com.cra.figaro.algorithm.factored._
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.language._
import com.cra.figaro.library.atomic.discrete._
import com.cra.figaro.test._
import com.cra.figaro.test.tags.Example
import com.cra.figaro.test.tags.NonDeterministic

class MutableMovieTest extends WordSpec with Matchers {
  "A PRM with a global constraint with mutation" should {
    "produce the correct probability under variable elimination" taggedAs (Example, NonDeterministic) in {
      test((e: Element[Boolean]) => VariableElimination(e))
    }

    "produce the correct probability under importance sampling" taggedAs (Example, NonDeterministic) in {
      test((e: Element[Boolean]) => Importance(12000, e))
    }

    "produce the correct probability under Metropolis-Hastings" taggedAs (Example, NonDeterministic) in {
      test((e: Element[Boolean]) => MetropolisHastings(200000, chooseScheme, e))
    }
  }

  val random = new scala.util.Random()

  val numActors = 2
  val numMovies = 2
  val numAppearances = 3
  var actors: Array[Actor] = _
  var movies: Array[Movie] = _
  var appearances: Array[Appearance] = _

  def test(algorithmCreator: Element[Boolean] => ProbQueryAlgorithm): Unit = {
    Universe.createNew()
    val actor1 = new Actor
    val actor2 = new Actor
    val movie1 = new Movie
    val movie2 = new Movie
    val appearance1 = new Appearance(actor1, movie1)
    val appearance2 = new Appearance(actor1, movie2)
    val appearance3 = new Appearance(actor2, movie2)
    actors = Array(actor1, actor2)
    movies = Array(movie1, movie2)
    appearances = Array(appearance1, appearance2, appearance3)
    // Ensure that exactly one appearance gets an award.
    val allAwards: Element[List[Boolean]] = Inject(appearances.map(_.award): _*)
    def uniqueAwardCondition(awards: List[Boolean]) = awards.count((b: Boolean) => b) == 1
    allAwards.setCondition(uniqueAwardCondition)

    actor1.famous.observe(true)
    movie2.quality.observe('high)

    // We first make sure the initial state satisfies the unique award condition, and then make sure that all
    // subsequent proposals keep that condition.
    appearances.foreach(_.award.randomness = 0.0)
    appearances(random.nextInt(numAppearances)).award.randomness = 1.0
    appearances.foreach(appearance =>
      appearance.award.value = appearance.award.generateValue(appearance.award.randomness))
    allAwards.generate()

    val qActor1SkillfulActor2SkillfulMovie1QualityHighActor2Famous = 0.1 * 0.1 * 0.5 * 0.5 * 0.8 * 0.1
    val qActor1SkillfulActor2SkillfulMovie1QualityHighActor2NotFamous = 0.1 * 0.1 * 0.5 * 0.5 * 0.8 * 0.9
    val qActor1SkillfulActor2SkillfulMovie1QualityMediumActor2Famous = 0.1 * 0.1 * 0.3 * 0.5 * 0.1 * 0.1
    val qActor1SkillfulActor2SkillfulMovie1QualityMediumActor2NotFamous = 0.1 * 0.1 * 0.3 * 0.5 * 0.1 * 0.9
    val qActor1SkillfulActor2SkillfulMovie1QualityLowActor2Famous = 0.1 * 0.1 * 0.2 * 0.5 * 0.1 * 0.1
    val qActor1SkillfulActor2SkillfulMovie1QualityLowActor2NotFamous = 0.1 * 0.1 * 0.2 * 0.5 * 0.1 * 0.9
    val qActor1SkillfulActor2NotSkillfulMovie1QualityHighActor2Famous = 0.1 * 0.9 * 0.5 * 0.2 * 0.8 * 0.1
    val qActor1SkillfulActor2NotSkillfulMovie1QualityHighActor2NotFamous = 0.1 * 0.9 * 0.5 * 0.2 * 0.8 * 0.9
    val qActor1SkillfulActor2NotSkillfulMovie1QualityMediumActor2Famous = 0.1 * 0.9 * 0.3 * 0.2 * 0.1 * 0.1
    val qActor1SkillfulActor2NotSkillfulMovie1QualityMediumActor2NotFamous = 0.1 * 0.9 * 0.3 * 0.2 * 0.1 * 0.9
    val qActor1SkillfulActor2NotSkillfulMovie1QualityLowActor2Famous = 0.1 * 0.9 * 0.2 * 0.2 * 0.1 * 0.1
    val qActor1SkillfulActor2NotSkillfulMovie1QualityLowActor2NotFamous = 0.1 * 0.9 * 0.2 * 0.2 * 0.1 * 0.9
    val qActor1NotSkillfulActor2SkillfulMovie1QualityHighActor2Famous = 0.9 * 0.1 * 0.2 * 0.2 * 0.8 * 0.1
    val qActor1NotSkillfulActor2SkillfulMovie1QualityHighActor2NotFamous = 0.9 * 0.1 * 0.2 * 0.2 * 0.8 * 0.9
    val qActor1NotSkillfulActor2SkillfulMovie1QualityMediumActor2Famous = 0.9 * 0.1 * 0.3 * 0.2 * 0.1 * 0.1
    val qActor1NotSkillfulActor2SkillfulMovie1QualityMediumActor2NotFamous = 0.9 * 0.1 * 0.3 * 0.2 * 0.1 * 0.9
    val qActor1NotSkillfulActor2SkillfulMovie1QualityLowActor2Famous = 0.9 * 0.1 * 0.5 * 0.2 * 0.1 * 0.1
    val qActor1NotSkillfulActor2SkillfulMovie1QualityLowActor2NotFamous = 0.9 * 0.1 * 0.5 * 0.2 * 0.1 * 0.9
    val qActor1NotSkillfulActor2NotSkillfulMovie1QualityHighActor2Famous = 0.9 * 0.9 * 0.2 * 0.2 * 0.8 * 0.1
    val qActor1NotSkillfulActor2NotSkillfulMovie1QualityHighActor2NotFamous = 0.9 * 0.9 * 0.2 * 0.2 * 0.8 * 0.9
    val qActor1NotSkillfulActor2NotSkillfulMovie1QualityMediumActor2Famous = 0.9 * 0.9 * 0.3 * 0.2 * 0.1 * 0.1
    val qActor1NotSkillfulActor2NotSkillfulMovie1QualityMediumActor2NotFamous = 0.9 * 0.9 * 0.3 * 0.2 * 0.1 * 0.9
    val qActor1NotSkillfulActor2NotSkillfulMovie1QualityLowActor2Famous = 0.9 * 0.9 * 0.5 * 0.2 * 0.1 * 0.1
    val qActor1NotSkillfulActor2NotSkillfulMovie1QualityLowActor2NotFamous = 0.9 * 0.9 * 0.5 * 0.2 * 0.1 * 0.9

    val qMovie1QualityHighActor2Famous =
      qActor1SkillfulActor2SkillfulMovie1QualityHighActor2Famous +
        qActor1SkillfulActor2NotSkillfulMovie1QualityHighActor2Famous +
        qActor1NotSkillfulActor2SkillfulMovie1QualityHighActor2Famous +
        qActor1NotSkillfulActor2NotSkillfulMovie1QualityHighActor2Famous

    val qMovie1QualityHighActor2NotFamous =
      qActor1SkillfulActor2SkillfulMovie1QualityHighActor2NotFamous +
        qActor1SkillfulActor2NotSkillfulMovie1QualityHighActor2NotFamous +
        qActor1NotSkillfulActor2SkillfulMovie1QualityHighActor2NotFamous +
        qActor1NotSkillfulActor2NotSkillfulMovie1QualityHighActor2NotFamous

    val qMovie1QualityMediumActor2Famous =
      qActor1SkillfulActor2SkillfulMovie1QualityMediumActor2Famous +
        qActor1SkillfulActor2NotSkillfulMovie1QualityMediumActor2Famous +
        qActor1NotSkillfulActor2SkillfulMovie1QualityMediumActor2Famous +
        qActor1NotSkillfulActor2NotSkillfulMovie1QualityMediumActor2Famous

    val qMovie1QualityMediumActor2NotFamous =
      qActor1SkillfulActor2SkillfulMovie1QualityMediumActor2NotFamous +
        qActor1SkillfulActor2NotSkillfulMovie1QualityMediumActor2NotFamous +
        qActor1NotSkillfulActor2SkillfulMovie1QualityMediumActor2NotFamous +
        qActor1NotSkillfulActor2NotSkillfulMovie1QualityMediumActor2NotFamous

    val qMovie1QualityLowActor2Famous =
      qActor1SkillfulActor2SkillfulMovie1QualityLowActor2Famous +
        qActor1SkillfulActor2NotSkillfulMovie1QualityLowActor2Famous +
        qActor1NotSkillfulActor2SkillfulMovie1QualityLowActor2Famous +
        qActor1NotSkillfulActor2NotSkillfulMovie1QualityLowActor2Famous

    val qMovie1QualityLowActor2NotFamous =
      qActor1SkillfulActor2SkillfulMovie1QualityLowActor2NotFamous +
        qActor1SkillfulActor2NotSkillfulMovie1QualityLowActor2NotFamous +
        qActor1NotSkillfulActor2SkillfulMovie1QualityLowActor2NotFamous +
        qActor1NotSkillfulActor2NotSkillfulMovie1QualityLowActor2NotFamous

    val qAppearance1AwardOnly =
      qMovie1QualityHighActor2Famous * 0.2 * 0.8 * 0.8 +
        qMovie1QualityHighActor2NotFamous * 0.2 * 0.8 * 0.95 +
        qMovie1QualityMediumActor2Famous * 0.05 * 0.8 * 0.8 +
        qMovie1QualityMediumActor2NotFamous * 0.05 * 0.8 * 0.95 +
        qMovie1QualityLowActor2Famous * 0.01 * 0.8 * 0.8 +
        qMovie1QualityLowActor2NotFamous * 0.01 * 0.8 * 0.95

    val qAppearance2AwardOnly =
      qMovie1QualityHighActor2Famous * 0.8 * 0.2 * 0.8 +
        qMovie1QualityHighActor2NotFamous * 0.8 * 0.2 * 0.95 +
        qMovie1QualityMediumActor2Famous * 0.95 * 0.2 * 0.8 +
        qMovie1QualityMediumActor2NotFamous * 0.95 * 0.2 * 0.95 +
        qMovie1QualityLowActor2Famous * 0.99 * 0.2 * 0.8 +
        qMovie1QualityLowActor2NotFamous * 0.99 * 0.2 * 0.95

    val qAppearance3AwardOnly =
      qMovie1QualityHighActor2Famous * 0.8 * 0.8 * 0.2 +
        qMovie1QualityHighActor2NotFamous * 0.8 * 0.8 * 0.05 +
        qMovie1QualityMediumActor2Famous * 0.95 * 0.8 * 0.2 +
        qMovie1QualityMediumActor2NotFamous * 0.95 * 0.8 * 0.05 +
        qMovie1QualityLowActor2Famous * 0.99 * 0.8 * 0.2 +
        qMovie1QualityLowActor2NotFamous * 0.99 * 0.8 * 0.05

    val pAppearance2AwardOnly =
      qAppearance2AwardOnly / (qAppearance1AwardOnly + qAppearance2AwardOnly + qAppearance3AwardOnly)

    val alg = algorithmCreator(appearance2.award)
    alg.start()
    alg.stop()
    alg.probability(appearance2.award, true) should be(pAppearance2AwardOnly +- 0.01)
    alg.kill()
  }

  class Actor {
    var movies: List[Movie] = List()

    lazy val skillful = Flip(0.1)

    lazy val famous = Flip(Apply(Inject(movies.map(_.quality): _*), probFamous _))

    private def probFamous(qualities: Seq[Symbol]) = if (qualities.count(_ == 'high) >= 2) 0.8; else 0.1
  }

  class Movie {
    var actors: List[Actor] = List()

    lazy val actorsAllGood = Apply(Inject(actors.map(_.skillful): _*), (s: Seq[Boolean]) => !(s.contains(false)))

    lazy val probLow = Apply(actorsAllGood, (b: Boolean) => if (b) 0.2; else 0.5)

    lazy val probHigh = Apply(actorsAllGood, (b: Boolean) => if (b) 0.5; else 0.2)

    lazy val quality = Select(probLow -> 'low, Constant(0.3) -> 'medium, probHigh -> 'high)
  }

  class Appearance(actor: Actor, movie: Movie) {
    actor.movies ::= movie
    movie.actors ::= actor

    def probAward(quality: Symbol, famous: Boolean) =
      (quality, famous) match {
        case ('low, false) => 0.001
        case ('low, true) => 0.01
        case ('medium, false) => 0.01
        case ('medium, true) => 0.05
        case ('high, false) => 0.05
        case ('high, true) => 0.2
      }
    lazy val award = SwitchingFlip(Apply(movie.quality, actor.famous, (q: Symbol, f: Boolean) => probAward(q, f)))
  }

  // A proposal either proposes to switch the awardee to another awardee or proposes the properties of a movie or
  // an actor.
  private def chooseScheme: ProposalScheme = {
    DisjointScheme(
      (0.4, () => switchAwards()),
      (0.2, () => ProposalScheme(actors(random.nextInt(numActors)).skillful)),
      (0.2, () => ProposalScheme(movies(random.nextInt(numMovies)).quality)),
      (0.2, () => ProposalScheme(actors(random.nextInt(numActors)).famous)))
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
}
