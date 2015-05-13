/*
 * MetropolisHastingsAnnealer.scala
 * Metropolis-Hastings Annealer.
 * 
 * Created By:      Brian Ruttenberg (bruttenberg@cra.com)
 * Creation Date:   Mar 1, 2013
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.algorithm.sampling

import com.cra.figaro.algorithm._
import com.cra.figaro.language._
import com.cra.figaro.util._
import scala.collection.mutable.Map
import scala.language.existentials
import scala.math.log
import scala.annotation.tailrec

/**
 * Metropolis-Hastings based Annealer.
 *
 * @param annealSchedule The schedule that determines how to anneal the model
 * @param burnIn The number of iterations to run before annealing starts
 * @param interval The number of iterations to perform before recording the annealing state
 * .
 */
// proposalScheme might evaluate to a different step each time it is called; making it by name gets the right effect.
abstract class MetropolisHastingsAnnealer(universe: Universe, proposalScheme: ProposalScheme, annealSchedule: Schedule,
  burnIn: Int, interval: Int)
  extends MetropolisHastings(universe, proposalScheme, burnIn, interval, List(): _*) with MPEAlgorithm {
  import MetropolisHastings._

  private var lastTemperature = 1.0
  private var lastIter = 0
  private var transProb = 0.0
  protected var bestEnergy: Double = Double.MinValue
  protected var currentEnergy: Double = _

  /**
   * The last computed transition probability.
   */
  def lastTransProb = transProb
  /**
   * The current temperature of the model.
   */
  def getTemperature = lastTemperature

  /*
   * The rule for acceptance is:
   * - if the current status is the InitialStatus always accept; otherwise
   * - if the new set of dissatisfied elements is a proper subset of the old set, always accept
   * - if it is a proper superset, never accept
   * - otherwise, use the acceptance probability scaled by the temperature of the annealer
   */
  override protected def decideToAccept(newState: State): Boolean = {
    val nothingNewDissatisfied = newState.dissatisfied subsetOf dissatisfied
    val somethingOldSatisfied = dissatisfied exists (_.conditionSatisfied)
    if (nothingNewDissatisfied && somethingOldSatisfied) true
    else if (!nothingNewDissatisfied && !somethingOldSatisfied) false
    else {
      lastTemperature = annealSchedule.temperature(lastTemperature, sampleCount)
      transProb = newState.proposalProb + lastTemperature * newState.modelProb
      if (debug) println("Transition Probability of " + transProb)
      log(random.nextDouble) < transProb
    }
  }

  override protected def mhStep(): State = {
    val newStateUnconstrained = proposeAndUpdate()
    val newState = State(newStateUnconstrained.oldValues, newStateUnconstrained.oldRandomness,
      newStateUnconstrained.proposalProb, newStateUnconstrained.modelProb + computeScores, newStateUnconstrained.dissatisfied)
    if (decideToAccept(newState)) {
      accepts += 1
      accept(newState)
      currentEnergy += newState.modelProb
    } else {
      rejects += 1
      undo(newState)
    }
    newState
  }

  private def saveState: Map[Element[_], Any] = {
    bestEnergy = currentEnergy
    Map(universe.activeElements.map(e => (e -> e.value)): _*)
  }

  override protected def initUpdates() = allLastUpdates = Map(universe.activeElements.map(e => (e -> (e.value, 0))): _*)

  override protected def updateTimesSeenForTarget[T](elem: Element[T], newValue: T): Unit = {
    allLastUpdates += (elem -> (newValue, sampleCount))
  }

  /**
   * Produce a single sample.
   */
  override def sample(): (Boolean, Sample) = {
    val nextState = mhStep()    

    if (dissatisfied.isEmpty) {
      sampleCount += 1
      val toUpdate = if (currentEnergy > bestEnergy) {
        saveState
      } else Map[Element[_], Any]()
      (true, toUpdate)
    } else {
      (false, Map())
    }
  }

  override def doInitialize(): Unit = {
    Forward(true)(universe)
    initConstrainedValues()
    dissatisfied = universe.conditionedElements.toSet filter (!_.conditionSatisfied)
    currentEnergy = universe.constrainedElements.map(_.constraintValue).sum
    for { i <- 1 to burnIn } {
      val nextState = mhStep()
      currentEnergy += nextState.modelProb
    }
  }

  def mostLikelyValue[T](target: Element[T]): T = {
    allLastUpdates(target)._1.asInstanceOf[T]
  }

  /**
   * Return the best energy computed by the annealer.
   */
  def getBestEnergy = bestEnergy
  /**
   * Return the current energy of the annealer.
   */
  def getCurrentEnergy = currentEnergy

}

/**
 * Anytime Metropolis-Hastings annealer.
 *
 * @param annealSchedule The schedule that determines how to anneal the model
 * @param burnIn The number of iterations to run before annealing starts
 * @param interval The number of iterations to perform before recording the annealing state
 *
 */
class AnytimeMetropolisHastingsAnnealer(universe: Universe,
  scheme: ProposalScheme, annealSchedule: Schedule, burnIn: Int, interval: Int)
  extends MetropolisHastingsAnnealer(universe, scheme, annealSchedule, burnIn, interval)
  with AnytimeMPESampler {
  /**
   * Initialize the annealer.
   */
  override def initialize(): Unit = {
    super.initialize()
    doInitialize()
  }

  /**
   * Clean up the annealer, freeing memory.
   */
  override def cleanUp(): Unit = {
    universe.clearTemporaries()
    super.cleanUp()
  }
}

/**
 * One-time Metropolis-Hastings annealer.
 *
 * @param annealSchedule The schedule that determines how to anneal the model
 * @param burnIn The number of iterations to run before annealing starts
 * @param interval The number of iterations to perform before recording the annealing state
 *
 */
class OneTimeMetropolisHastingsAnnealer(universe: Universe, myNumSamples: Int, scheme: ProposalScheme, annealSchedule: Schedule,
  burnIn: Int, interval: Int)
  extends MetropolisHastingsAnnealer(universe, scheme, annealSchedule, burnIn, interval)
  with OneTimeMPESampler {

  val numSamples = myNumSamples

  /**
   * Run the algorithm, performing its computation to completion.
   */
  override def run(): Unit = {
    doInitialize()
    super.run()
  }
}

object MetropolisHastingsAnnealer {

  /**
   * Create an anytime Metropolis-Hastings annealer using the given proposal scheme and annealing schedule.
   */
  def apply(scheme: ProposalScheme, annealSchedule: Schedule)(implicit universe: Universe) =
    new AnytimeMetropolisHastingsAnnealer(universe, scheme, annealSchedule, 0, 1)

  /**
   * Create a one-time Metropolis-Hastings annealer using the given number of samples, proposal
   * scheme and annealing schedule.
   */
  def apply(numSamples: Int, scheme: ProposalScheme, annealSchedule: Schedule)(implicit universe: Universe) =
    new OneTimeMetropolisHastingsAnnealer(universe, numSamples, scheme, annealSchedule, 0, 1)

  /**
   * Create an anytime Metropolis-Hastings annealer using the given proposal scheme, annealing schedule and number
   * of burn-in samples.
   */
  def apply(scheme: ProposalScheme, annealSchedule: Schedule, burnIn: Int)(implicit universe: Universe) =
    new AnytimeMetropolisHastingsAnnealer(universe, scheme, annealSchedule, burnIn, 1)

  /**
   * Create a one-time Metropolis-Hastings annealer using the given number of samples, proposal scheme,
   *  annealing schedule and number of burn-in samples.
   */
  def apply(numSamples: Int, scheme: ProposalScheme, annealSchedule: Schedule, burnIn: Int)(implicit universe: Universe) =
    new OneTimeMetropolisHastingsAnnealer(universe, numSamples, scheme, annealSchedule, burnIn, 1)

  /**
   * Create an anytime Metropolis-Hastings annealer using the given proposal scheme, annealing schedule, number of burn-in
   * samples, and interval between samples.
   */
  def apply(scheme: ProposalScheme, annealSchedule: Schedule, burnIn: Int, interval: Int)(implicit universe: Universe) =
    new AnytimeMetropolisHastingsAnnealer(universe, scheme, annealSchedule, burnIn, interval)

  /**
   * Create a one-time Metropolis-Hastings annealer using the given number of samples, proposal scheme, annealing schedule,
   * number of burn-in samples, and interval between samples.
   */
  def apply(numSamples: Int, scheme: ProposalScheme, annealSchedule: Schedule,
    burnIn: Int, interval: Int)(implicit universe: Universe) =
    new OneTimeMetropolisHastingsAnnealer(universe, numSamples, scheme, annealSchedule, burnIn, interval: Int)

}
