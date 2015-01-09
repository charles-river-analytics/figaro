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
  burnIn: Int, interval: Int,
  targets: Element[_]*)
  extends UnweightedAnnealer(universe, targets: _*) {
  import MetropolisHastings._

  // Used for debugging
  private var elementsToTrack: Map[Element[_], Null] = Map()
  private var proposalCounts: Map[Element[_], Int] = Map()
  // Make sure these maps don't cause memory leaks
  universe.register(elementsToTrack)
  universe.register(proposalCounts)

  private var accepts = 0
  private var rejects = 0

  private val nextConstraintValues: Map[Element[_], Double] = Map()
  universe.register(nextConstraintValues)

  /**
   * Set this flag to true to obtain debugging information.
   */
  var debug = false

  private def newState: State = State(Map(), Map(), 0.0, 0.0, scala.collection.mutable.Set())

  private var lastTemperature = 1.0
  private var lastIter = 0
  private var transProb = 0.0

  /**
   * The last computed transition probability.
   */
  def lastTransProb = transProb
  /**
   * The current temperature of the model.
   */
  def getTemperature = lastTemperature

  /*
   * We continually update the values of elements while making a proposal. In order to be able to undo it, we need to
   * store the old value.
   * We keep track of the improvement in the constraints for the new proposal compared to the original value.
   * We keep track of which elements do not have their condition satisfied by the new proposal.
   */
  private def attemptChange[T](state: State, elem: Element[T]): State = {
    val newValue = elem.generateValue(elem.randomness)
    // if an old value is already stored, don't overwrite it
    val newOldValues =
      if (state.oldValues contains elem) state.oldValues; else state.oldValues + (elem -> elem.value)
    if (elem.value != newValue) {
      val newDissatisfied =
        if (elem.condition(newValue)) state.dissatisfied -= elem; else state.dissatisfied += elem
      elem.value = newValue
      State(newOldValues, state.oldRandomness, state.proposalProb, state.modelProb, newDissatisfied)
    } else {
      // We need to make sure to add the element to the dissatisfied set if its condition is not satisfied,
      // even if the value has not changed, because we compare the dissatisfied set with the old dissatisfied set
      // when deciding whether to accept the proposal.
      val newDissatisfied =
        if (elem.condition(newValue)) {
          state.dissatisfied - elem
        } else {
          state.dissatisfied + elem
        }
      State(newOldValues, state.oldRandomness, state.proposalProb, state.modelProb, newDissatisfied)
    }
  }

  private def propose[T](state: State, elem: Element[T]): State = {
    if (debug) println("Proposing " + elem.name.string)
    if (elementsToTrack contains elem) proposalCounts += elem -> (proposalCounts(elem) + 1)
    val oldValue = elem.value
    val oldRandomness = elem.randomness
    val (randomness, proposalProb, modelProb) = elem.nextRandomness(elem.randomness)
    val state1 =
      if (randomness == oldRandomness) state
      else {
        val newOldRandomness =
          if (state.oldRandomness contains elem) state.oldRandomness
          else state.oldRandomness + (elem -> elem.randomness)
        val newProb = state.proposalProb + log(proposalProb)
        elem.randomness = randomness
        State(state.oldValues, newOldRandomness, newProb, state.modelProb + log(modelProb), state.dissatisfied)
      }
    val result = attemptChange(state1, elem)
    if (debug) println("old randomness = " + oldRandomness +
      ", old value = " + oldValue +
      ", new randomness = " + elem.randomness +
      ", new value = " + elem.value)
    result
  }

  /*
   * Switch the randomness of two elements.
   */
  private def switch[T, U](state: State, elem1: Element[T], elem2: Element[U]): State = {
    if (debug) println("Switching " + elem1.name.string + " and " + elem2.name.string)
    if (elementsToTrack contains elem1) proposalCounts += elem1 -> (proposalCounts(elem1) + 1)
    if (elementsToTrack contains elem2) proposalCounts += elem2 -> (proposalCounts(elem2) + 1)
    val oldValue1 = elem1.value
    val oldValue2 = elem2.value
    val oldRandomness1 = elem1.randomness
    val oldRandomness2 = elem2.randomness
    val x = elem1.randomness
    elem1.randomness = elem2.randomness.asInstanceOf[elem1.Randomness]
    elem2.randomness = x.asInstanceOf[elem2.Randomness]
    val state1 = {
      val newOldRandomness1 =
        if (state.oldRandomness contains elem1) state.oldRandomness
        else state.oldRandomness + (elem1 -> oldRandomness1)
      val newOldRandomness2 =
        if (newOldRandomness1 contains elem2) newOldRandomness1
        else newOldRandomness1 + (elem2 -> oldRandomness2)
      State(state.oldValues, newOldRandomness2, state.proposalProb, state.modelProb, state.dissatisfied)
    }
    val state2 = attemptChange(state1, elem1)
    val result = attemptChange(state2, elem2)
    if (debug) println(elem1.name.string + ": " + "old randomness = " + oldRandomness1 +
      ", old value = " + oldValue1 +
      ", new randomness = " + elem1.randomness +
      ", new value = " + elem1.value)
    if (debug) println(elem2.name.string + ": " + "old randomness = " + oldRandomness2 +
      ", old value = " + oldValue2 +
      ", new randomness = " + elem2.randomness +
      ", new value = " + elem2.value)
    result
  }

  private def continue(state: State, rest: Option[ProposalScheme]): State =
    rest match {
      case None => state
      case Some(step) => runStep(state, step)
    }

  private def runStep(state: State, step: ProposalScheme): State =
    step match {
      case FinalScheme(elem) => propose(state, elem())
      case TypedScheme(first, rest) =>
        val firstElem = first()
        val state1 = propose(state, firstElem)
        continue(state1, rest(firstElem.value))
      case UntypedScheme(first, rest) =>
        val firstElem = first()
        val state1 = propose(state, firstElem)
        continue(state1, rest)
      case ds: DisjointScheme =>
        val (probs, schemes) = ds.choices.toList.unzip
        val choice = sampleMultinomial(normalize(probs) zip schemes)
        runStep(state, choice())
      case SwitchScheme(first, rest) =>
        val (elem1, elem2) = first()
        val state1 = switch(state, elem1, elem2)
        continue(state1, rest)
    }

  private def runScheme(): State = runStep(newState, proposalScheme)

  /*
   * To update an element, first we update the element itself, then update all elements that directly use it.
   * If we update elements in directlyUsedBy order, we are guaranteed that if both x and y use a changed element,
   * and if y uses x, then the last update of y will come after the last update of target.
   *
   * Updating an element itself requires generating its value using the current randomness and setting its value. An
   * alternative implementation would also propose changing the randomness element. We choose not to do that, because
   * we believe the proposal scheme should fully specify which elements have their randomness changed. To make our
   * strategy work well, it is important that the Randomness of non-deterministic elements be kept minimal, so that
   * as much change can be made to them as possible without changing their randomness. For example, the randomness of
   * Flip is a probability rather than the Boolean result. Generating the Flip's value requires comparing a random
   * Double to this probability. Therefore, if we have an element Flip(Uniform(0.2, 0.7)), if the value of the Uniform
   * expression changes, the value of the Flip might change if its current randomness probability is appropriate.
   */
  private def updateOne[T](state: State, elem: Element[T]): State = {
    if (debug) println("Updating " + elem.name.string)
    val oldValue = elem.value
    val result = attemptChange(state, elem) // compare to propose
    if (debug) println("randomness = " + elem.randomness + ", old value = " + oldValue + ", new value = " + elem.value)
    result
  }

  /*
   * Update a set of elements after a proposal given the current state.
   * Since elements must be updated in generative order, we have to ensure the arguments 
   * of an element are updated being an element itself. To do that, we check the intersection
   * of the an element's arguments with the elements that still need to be updated. If the intersection
   * is not empty, we recursively update the intersecting elements. Once those updates are completed,
   * we update an element and move on to the next element in the set.
   */
  private def updateMany[T](state: State, toUpdate: Set[Element[_]]): State = {
    var returnState = state
    var updatesLeft = toUpdate
    while (!updatesLeft.isEmpty) {
      // Check the intersection of an element's arguments with the updates that still need to occur
      var argsRemaining = universe.uses(updatesLeft.head).intersect(updatesLeft)
      while (!argsRemaining.isEmpty) {
        // update the element's arguments first
        returnState = updateManyHelper(returnState, argsRemaining.toSet)
        argsRemaining = argsRemaining.tail
      }
      // once the args are updated, update this element
      returnState = updateOne(returnState, updatesLeft.head)
      updatesLeft = updatesLeft.tail
    }
    returnState
  }

  /*
   * A recursive function to work in conjunction with updateMany to check the order of the element
   * updates.
   */
  @tailrec
  private def updateManyHelper(state: State, toUpdate: Set[Element[_]]): State = {
    var returnState = state
    var updatesLeft = toUpdate
    var argsRemaining = universe.uses(updatesLeft.head).intersect(updatesLeft)
    if (argsRemaining.isEmpty) {
      returnState = updateOne(returnState, updatesLeft.head)
      returnState
    } else { updateManyHelper(returnState, argsRemaining.toSet) }
  }
  /*
   * A single step of MetropolisHastings consists of proposing according to the scheme and updating any elements that depend on those
   * changed.
   */
  private def proposeAndUpdate(): State = {
    val state1 = runScheme()
    val updatesNeeded = state1.oldValues.keySet flatMap (universe.usedBy(_))
    updateMany(state1, updatesNeeded.toSet)
  }

  private var dissatisfied: Set[Element[_]] = universe.conditionedElements.toSet filter (!_.conditionSatisfied)

  private def getDissatisfied = dissatisfied // for testing

  /*
   * Computes the scores of the constrained elements from caches scores. If the element
   * is not found in the cache, then it's value is 1.0.
   */
  private def computeScores(): Double = {
    val constrainedElements = universe.constrainedElements
    constrainedElements foreach (e => nextConstraintValues.update(e, e.constraintValue))
    val scores = constrainedElements.map { e =>
      nextConstraintValues(e) - currentConstraintValues.getOrElseUpdate(e, 0.0)
    }
    scores.sum
  }

  private def accept(state: State): Unit = {
    if (debug) println("Accepting!\n")
    currentConstraintValues.keys.foreach(e => currentConstraintValues += e -> nextConstraintValues(e))
    dissatisfied = (dissatisfied filter (!_.conditionSatisfied)) ++ state.dissatisfied
  }

  private def setValue(pair: (Element[_], Any)) = {
    val (elem, value) = pair
    elem.value = value.asInstanceOf[elem.Value]
  }

  private def setRandomness(pair: (Element[_], Any)) = {
    val (elem, randomness) = pair
    elem.randomness = randomness.asInstanceOf[elem.Randomness]
  }

  private def undo(state: State): Unit = {
    if (debug) println("Rejecting!\n")
    state.oldValues foreach (setValue(_))
    state.oldRandomness foreach (setRandomness(_))
  }

  /*
   * The rule for acceptance is:
   * - if the current status is the InitialStatus always accept; otherwise
   * - if the new set of dissatisfied elements is a proper subset of the old set, always accept
   * - if it is a proper superset, never accept
   * - otherwise, use the acceptance probability
   */
  private def decideToAccept(newState: State, annealer: Schedule, iter: Int): Boolean = {
    val nothingNewDissatisfied = newState.dissatisfied subsetOf dissatisfied
    val somethingOldSatisfied = dissatisfied exists (_.conditionSatisfied)
    if (nothingNewDissatisfied && somethingOldSatisfied) true
    else if (!nothingNewDissatisfied && !somethingOldSatisfied) false
    else {
      lastTemperature = annealer.temperature(lastTemperature, iter)
      transProb = newState.proposalProb + lastTemperature * newState.modelProb
      if (debug) println("Transition Probability of " + transProb)
      log(random.nextDouble) < transProb
    }
  }

  private def initConstrainedValues() = {
    universe.constrainedElements.foreach(e => currentConstraintValues += (e -> e.constraintValue))
  }

  protected final def mhStep(annealer: Schedule, iter: Int): State = {
    val nextStateUnconstrained = proposeAndUpdate()
    val nextState = State(nextStateUnconstrained.oldValues, nextStateUnconstrained.oldRandomness,
      nextStateUnconstrained.proposalProb, nextStateUnconstrained.modelProb + computeScores, nextStateUnconstrained.dissatisfied)
    if (decideToAccept(nextState, annealer, iter)) {
      accepts += 1
      accept(nextState)
      nextState
    } else {
      rejects += 1
      undo(nextState)
      newState
    }
  }

  /**
   * Produce a single sample.
   */
  final def sample(iter: Int): (Boolean, Double) = {
    val nextState = mhStep(annealSchedule, iter)
    lastIter = iter
    if (dissatisfied.isEmpty) {
      (true, nextState.modelProb)
    } else {
      (false, nextState.modelProb)
    }

  }

  protected override final def doSample() = {
    for { i <- 1 to interval - 1 } { mhStep(annealSchedule, lastIter) }
    super.doSample()
  }

  protected def doInitialize(): Unit = {
    Forward(universe)
    initConstrainedValues()
    for { i <- 1 to burnIn } mhStep(Schedule.schedule, 1)
  }
  /**
   * Test Metropolis-Hastings by repeatedly running a single step from the same initial state.
   * For each of a set of predicates, the fraction of times the predicate is satisfied by the resulting state
   * is returned. By the resulting state, we mean the new state if it is accepted and the original state
   * if not.
   */
  def test(numSamples: Int, predicates: Seq[Predicate[_]], elementsToTrack: Seq[Element[_]]): (Map[Predicate[_], Double], Map[Element[_], Double]) = {
    val successes: Map[Predicate[_], Int] = Map((predicates map (_ -> 0)): _*)
    this.elementsToTrack = Map((elementsToTrack map (_ -> null): _*))
    proposalCounts = Map((elementsToTrack map (_ -> 0)): _*)
    def collectResults() =
      for { predicate <- predicates } {
        if (predicate.test) successes += predicate -> (successes(predicate) + 1)
      }

    for { i <- 1 to numSamples } {
      val newStateUnconstrained = proposeAndUpdate()
      val state1 = State(newStateUnconstrained.oldValues, newStateUnconstrained.oldRandomness,
        newStateUnconstrained.proposalProb, newStateUnconstrained.modelProb + computeScores, newStateUnconstrained.dissatisfied)
      if (decideToAccept(state1, Schedule.schedule, 1)) {
        accepts += 1
        // collect results for the new state and restore the original state
        collectResults()
        undo(state1)
      } else {
        rejects += 1
        // collect results for the original state
        undo(state1)
        collectResults()
      }
    }
    val successResults: Map[Predicate[_], Double] =
      Map((for { (key, count) <- successes.toSeq } yield key -> count.toDouble / numSamples): _*)
    val proposalResults: Map[Element[_], Double] =
      Map((for { (key, count) <- proposalCounts.toSeq } yield key -> count.toDouble / numSamples): _*)
    (successResults, proposalResults)
  }
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
  scheme: ProposalScheme, annealSchedule: Schedule, burnIn: Int, interval: Int,
  targets: Element[_]*)
  extends MetropolisHastingsAnnealer(universe, scheme, annealSchedule, burnIn, interval, targets: _*)
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
  burnIn: Int, interval: Int, targets: Element[_]*)
  extends MetropolisHastingsAnnealer(universe, scheme, annealSchedule, burnIn, interval, targets: _*)
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
    new AnytimeMetropolisHastingsAnnealer(universe, scheme, annealSchedule, 0, 1, List(): _*)

  /**
   * Create a one-time Metropolis-Hastings annealer using the given number of samples, proposal
   * scheme and annealing schedule.
   */
  def apply(numSamples: Int, scheme: ProposalScheme, annealSchedule: Schedule)(implicit universe: Universe) =
    new OneTimeMetropolisHastingsAnnealer(universe, numSamples, scheme, annealSchedule, 0, 1, List(): _*)

  /**
   * Create an anytime Metropolis-Hastings annealer using the given proposal scheme, annealing schedule and number
   * of burn-in samples.
   */
  def apply(scheme: ProposalScheme, annealSchedule: Schedule, burnIn: Int)(implicit universe: Universe) =
    new AnytimeMetropolisHastingsAnnealer(universe, scheme, annealSchedule, burnIn, 1, List(): _*)

  /**
   * Create a one-time Metropolis-Hastings annealer using the given number of samples, proposal scheme,
   *  annealing schedule and number of burn-in samples.
   */
  def apply(numSamples: Int, scheme: ProposalScheme, annealSchedule: Schedule, burnIn: Int)(implicit universe: Universe) =
    new OneTimeMetropolisHastingsAnnealer(universe, numSamples, scheme, annealSchedule, burnIn, 1, List(): _*)

  /**
   * Create an anytime Metropolis-Hastings annealer using the given proposal scheme, annealing schedule, number of burn-in
   * samples, and interval between samples.
   */
  def apply(scheme: ProposalScheme, annealSchedule: Schedule, burnIn: Int, interval: Int)(implicit universe: Universe) =
    new AnytimeMetropolisHastingsAnnealer(universe, scheme, annealSchedule, burnIn, interval, List(): _*)

  /**
   * Create a one-time Metropolis-Hastings annealer using the given number of samples, proposal scheme, annealing schedule,
   * number of burn-in samples, and interval between samples.
   */
  def apply(numSamples: Int, scheme: ProposalScheme, annealSchedule: Schedule,
    burnIn: Int, interval: Int)(implicit universe: Universe) =
    new OneTimeMetropolisHastingsAnnealer(universe, numSamples, scheme, annealSchedule, burnIn, interval: Int, List(): _*)

}
