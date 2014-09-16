/*
 * ParticleFilter.scala
 * Particle Filtering
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Jan 1, 2009
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.algorithm.filtering

import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.language._
import com.cra.figaro.util._
import sun.swing.AccumulativeRunnable

/**
 * An abstract class of particle filters.
 * A particle filter is provided with three models:
 * a static model, containing a universe defining a distribution over static elements that do not change over time;
 * an initial model, containing a universe defining a distribution over the initial state of time-varying elements;
 * and a transition model, which is a function from the previous universe to a new universe. defining the way the distribution over the new state
 * of the time-varying variables depends on their values in the previous state.
 * The fourth argument to the particle filter is the number of particles to use at each time step.
 *
 * The particle filter works in an online fashion. At each point in time, it maintains its current beliefs about the state of the system as a set of
 * representative states. advanceTime is used to move forward one time step. The particle filter updates its beliefs in light
 * of the new evidence.
 *
 * @param static A universe with static elements that do not change over time.
 * @param intitial The universe describing the initial distribution of the model.
 * @param transition The transition function that returns a new universe from a static and previous universe, respectively.
 */
abstract class ParticleFilter(static: Universe = new Universe(), initial: Universe, transition: (Universe, Universe) => Universe, numParticles: Int)
  extends Filtering(static, initial, transition) {

  /** The belief about the state of the system at the current point in time. */
  val beliefState: ParticleFilter.BeliefState = Array.fill(numParticles)(null)

  protected var logProbEvidence: Double = 0.0
  protected var previousUniverse: Universe = _
  protected var currentUniverse = initial

  /**
   * Returns the expectation of the element referred to by the reference
   * under the given function at the current time point.
   */
  def computeCurrentExpectation[T](reference: Reference[T], function: T => Double): Double = {
    val fValues: Seq[Double] = beliefState.map(state => function(state.get(reference)))
    val total = (fValues :\ 0.0)(_ + _)
    total.toDouble / numParticles
  }

  /**
   * Returns the distribution over the element referred to by the reference at the current time point.
   */
  def computeCurrentDistribution[T](reference: Reference[T]): Stream[(Double, T)] = {
    val map = scala.collection.mutable.Map[T, Int]()
    for {
      state <- beliefState
    } {
      val t = state.get(reference)
      val prevCount = map.getOrElse(t, 0)
      map += t -> (prevCount + 1)
    }
    val z = 1.0 / beliefState.size
    val normalized = map.toList.map((pair: (T, Int)) => (pair._2 * z, pair._1))
    normalized.toStream
  }

  /*
   * Careful: makeWeightedParticle overwrites the previous state with the new state. That means we can't use it to generate another new particle from the same previous
   * state. The reason for this design is to avoid creating new snapshots and states to conserve memory.
   */
  protected def makeWeightedParticle(previousState: State): ParticleFilter.WeightedParticle = {
    Forward(currentUniverse)
    // avoiding recursion
    var satisfied = true
    var conditionedElementsRemaining = currentUniverse.conditionedElements
    while (!conditionedElementsRemaining.isEmpty) {
      satisfied &= conditionedElementsRemaining.head.conditionSatisfied
      conditionedElementsRemaining = conditionedElementsRemaining.tail
    }
    val weight =
      if (satisfied) {
        var w = 1.0
        var constrainedElementsRemaining = currentUniverse.constrainedElements
        while (!constrainedElementsRemaining.isEmpty) {
          w *= math.exp(constrainedElementsRemaining.head.constraintValue)
          constrainedElementsRemaining = constrainedElementsRemaining.tail
        }
        w
      } else 0.0
    val snapshot = new Snapshot
    snapshot.store(currentUniverse)
    val state = new State(snapshot, previousState.static)
    (weight, state)
  }

  private[figaro] def updateBeliefState(weightedParticles: Seq[ParticleFilter.WeightedParticle]) {
    // If all the particles have weight 1, there is no need to resample
    // If all the particles have weight 0, none of them satisfy the conditions, so the best we can do is produce a uniform distribution over them.
    if (weightedParticles.forall(_._1 == 1.0) || weightedParticles.forall(_._1 == 0.0)) {
      val weightedParticleArray = weightedParticles.toArray
      for { i <- 0 until numParticles } {
        beliefState(i) = weightedParticleArray(i)._2
      }
    } else {
      val resampler = new MapResampler(weightedParticles)
      for { i <- 0 until numParticles } {
        beliefState(i) = resampler.resample()
      }
    }
  }

  private[figaro] def computeProbEvidence(weightedParticles: Seq[ParticleFilter.WeightedParticle]) {
    // compute probability of evidence here by taking the average weight of the weighted particles and store it so you can later return it as a query result
    val weightedParticleArray = weightedParticles.toArray
    val sum = weightedParticleArray.map(_._1).sum
    logProbEvidence = logProbEvidence + scala.math.log(sum / numParticles)
  }

  protected def addWeightedParticle(evidence: Seq[NamedEvidence[_]], index: Int): ParticleFilter.WeightedParticle = {
    val previousState = beliefState(index)
    previousState.dynamic.restore(previousUniverse)
    previousState.static.restore(static)
    currentUniverse.assertEvidence(evidence)
    val result = makeWeightedParticle(previousState)
    result
  }

  protected def initialWeightedParticle(): ParticleFilter.WeightedParticle = {
    Forward(static)
    val staticSnapshot = new Snapshot
    staticSnapshot.store(static)
    val state = new State(new Snapshot, staticSnapshot)
    makeWeightedParticle(state)
  }

  /*
   * Advance the universe one time step.
   * The previous universe becomes a copy of the current universe with all named elements replaced by constants.
   * This is done so we don't have to store the previous universe (and the universes previous to it), and we can release the memory.
   */
  protected def advanceUniverse() {

    previousUniverse = Universe.createNew()
    for { element <- currentUniverse.activeElements.filter(!_.name.isEmpty) } {
      new Settable(element.name.string, element.value, previousUniverse)
    }
    currentUniverse = transition(static, previousUniverse)

  }

  def getlogProbEvidence(): Double = {
    logProbEvidence
  }

  def probEvidence(): Double = {
    val probEvidence = scala.math.exp(logProbEvidence)
    probEvidence
  }

}

/**
 * A one-time particle filter.
 *
 * @param static The universe of elements whose values do not change over time
 * @param initial The universe describing the distribution over the initial state of the system
 * @param transition The transition model describing how the current state of the system depends on the previous
 * @param numParticles The number of particles to use at each time step
 */
class OneTimeParticleFilter(static: Universe = new Universe(), initial: Universe, transition: (Universe, Universe) => Universe, numParticles: Int)
  extends ParticleFilter(static, initial, transition, numParticles) with OneTimeFiltering {
  private def doTimeStep(weightedParticleCreator: Int => ParticleFilter.WeightedParticle) {
    val weightedParticles = for { i <- 0 until numParticles } yield weightedParticleCreator(i)

    // compute probability of evidence here by taking the average weight of the weighted particles and store it so you can later return it as a query result
    computeProbEvidence(weightedParticles)

    updateBeliefState(weightedParticles)
  }

  /**
   * Begin the particle filter, determining the initial distribution.
   */
  def run(): Unit = {
    doTimeStep((i: Int) => initialWeightedParticle())
  }

  /**
   * Advance the filtering one time step, conditioning on the given evidence at the new time point.
   */
  def advanceTime(evidence: Seq[NamedEvidence[_]] = List()): Unit = {

    advanceUniverse()
    doTimeStep((i: Int) => addWeightedParticle(evidence, i))

  }
}

object ParticleFilter {

  /**
   * A one-time particle filter.
   *
   * @param static The universe of elements whose values do not change over time
   * @param initial The universe describing the distribution over the initial state of the system
   * @param transition The transition model describing how the current state of the system depends on the static and previous, respectively
   * @param numParticles Number of particles to use at each time step
   */
  def apply(static: Universe, initial: Universe, transition: (Universe, Universe) => Universe, numParticles: Int): OneTimeParticleFilter =
    new OneTimeParticleFilter(static, initial, transition, numParticles)

  /**
   * A one-time particle filter.
   *
   * @param static The universe of elements whose values do not change over time
   * @param initial The universe describing the distribution over the initial state of the system
   * @param transition The transition model describing how the current state of the system depends on the previous
   * @param numParticles Number of particles to use at each time step
   */
  @deprecated("If the static universe is defined, use the constructor where the transition function takes two universes", "2.3.0.0")
  def apply(static: Universe, initial: Universe, transition: Universe => Universe, numParticles: Int): OneTimeParticleFilter =
    new OneTimeParticleFilter(static, initial, (static: Universe, previous: Universe) => transition(previous), numParticles)

  /**
   * A one-time particle filter in which the static universe is empty.
   *
   * @param initial The universe describing the distribution over the initial state of the system
   * @param transition The transition model describing how the current state of the system depends on the previous
   * @param numParticles Number of particles to use at each time step
   */
  def apply(initial: Universe, transition: Universe => Universe, numParticles: Int): OneTimeParticleFilter =
    apply(new Universe(), initial, (static: Universe, previous: Universe) => transition(previous), numParticles)

  /**
   *  A representation of the current beliefs of the particle filter.
   *  A BeliefState should not be confused with a State, which is a particular configuration of the system.
   *  A BeliefState represents a distribution over States, and in a particle filter, it is implemented as a collection of representative States.
   */
  type BeliefState = Array[State] // dynamic and static

  /** Weighted particles, consisting of a weight and a state. */
  type WeightedParticle = (Double, State)

}
