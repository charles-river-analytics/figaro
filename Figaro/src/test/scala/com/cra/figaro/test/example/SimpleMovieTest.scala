/*
 * SimpleMovieTest.scala  
 * Simple movie example tests.
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
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.algorithm.factored._
import com.cra.figaro.library.atomic.discrete._
import com.cra.figaro.language._
import com.cra.figaro.language.Universe._
import com.cra.figaro.test._
import com.cra.figaro.test.tags.Example

class SimpleMovieTest extends WordSpec with Matchers {
  "A PRM with a global constraint without mutation" should {
    "produce the correct probability under variable elimination" taggedAs (Example) in {
      test((e: Element[Boolean]) => VariableElimination(e))
    }

    "produce the correct probability under importance sampling" taggedAs (Example) in {
      test((e: Element[Boolean]) => Importance(20000, e))
    }

    "produce the correct probability under Metropolis-Hastings" taggedAs (Example) in {
      test((e: Element[Boolean]) => { val m = MetropolisHastings(100000, chooseScheme, 0, e); /*m.debug = true;*/ m })
    }
  }

  class Actor(name: String) {
    val famous = Flip(0.1)(name + "famous", universe)
  }

  class Movie(name: String) {
    val quality = Select(0.3 -> 'low, 0.5 -> 'medium, 0.2 -> 'high)(name + "quality", universe)
  }

  class Appearance(name: String, val actor: Actor, val movie: Movie) {
    def probAward(quality: Symbol, famous: Boolean) =
      (quality, famous) match {
        case ('low, false) => 0.001
        case ('low, true) => 0.01
        case ('medium, false) => 0.01
        case ('medium, true) => 0.05
        case ('high, false) => 0.05
        case ('high, true) => 0.2
      }
    val pa = Apply(movie.quality, actor.famous, (q: Symbol, f: Boolean) => probAward(q, f))(name + "probAward", universe)
    val award = SwitchingFlip(pa)(name + "award", universe)
  }

  val numActors = 3
  val numMovies = 2
  val numAppearances = 3
  var actors: Array[Actor] = _
  var movies: Array[Movie] = _
  var appearances: Array[Appearance] = _
  val random = new scala.util.Random()

  // A proposal either proposes to switch the awardee to another awardee or proposes the properties of a movie or
  // an actor.
  def chooseScheme(): ProposalScheme = {
    DisjointScheme(
      (0.5, () => switchAwards()),
      (0.25, () => ProposalScheme(actors(random.nextInt(numActors)).famous)),
      (0.25, () => ProposalScheme(movies(random.nextInt(numMovies)).quality)))
  }

  /*
   * It's possible that as a result of other attributes changing, an appearance becomes awarded or unawarded.
   * Therefore, we have to take this into account in the proposal scheme.
   */
  def switchAwards(): ProposalScheme = {
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

  def test(algorithmCreator: Element[Boolean] => ProbQueryAlgorithm): Unit = {
    Universe.createNew()
    val actor1 = new Actor("actor1")
    val actor2 = new Actor("actor2")
    val actor3 = new Actor("actor3")
    val movie1 = new Movie("movie1")
    val movie2 = new Movie("movie2")
    val appearance1 = new Appearance("appearance1", actor1, movie1)
    val appearance2 = new Appearance("appearance2", actor2, movie2)
    val appearance3 = new Appearance("appearance3", actor3, movie2)
    actors = Array(actor1, actor2, actor3)
    movies = Array(movie1, movie2)
    appearances = Array(appearance1, appearance2, appearance3)

    // Ensure that exactly one appearance gets an award.
    val appearanceAwards: Array[Element[Boolean]] = appearances map (_.award)
    val allAwards: Element[List[Boolean]] = Inject(appearanceAwards: _*)("allAwards", universe)
    def uniqueAwardCondition(awards: List[Boolean]) = awards.count((b: Boolean) => b) == 1
    allAwards.setCondition(uniqueAwardCondition)

    actor3.famous.observe(true)
    movie2.quality.observe('high)

    // We first make sure the initial state satisfies the unique award condition, and then make sure that all
    // subsequent proposals keep that condition.
    appearances.foreach(_.award.randomness = 0.0)
    appearances(random.nextInt(numAppearances)).award.randomness = 1.0
    appearances.foreach(appearance =>
      appearance.award.value = appearance.award.generateValue(appearance.award.randomness))
    allAwards.generate()

    val qAppearance1Award = 0.1 * (0.3 * 0.01 + 0.5 * 0.05 + 0.2 * 0.2) + 0.9 * (0.3 * 0 + 0.5 * 0.01 + 0.2 * 0.05)
    val qAppearance2Award = 0.1 * 0.2 + 0.9 * 0.05
    val qAppearance3Award = 0.2
    val qAppearance1Only = qAppearance1Award * (1 - qAppearance2Award) * (1 - qAppearance3Award)
    val qAppearance2Only = qAppearance2Award * (1 - qAppearance1Award) * (1 - qAppearance3Award)
    val qAppearance3Only = qAppearance3Award * (1 - qAppearance1Award) * (1 - qAppearance2Award)
    val pAppearance3Award = qAppearance3Only / (qAppearance1Only + qAppearance2Only + qAppearance3Only)

    val alg = algorithmCreator(appearance3.award)
    alg.start()
    alg.stop()
    alg.probability(appearance3.award, true) should be(pAppearance3Award +- 0.01)
    alg.kill()
  }

}
