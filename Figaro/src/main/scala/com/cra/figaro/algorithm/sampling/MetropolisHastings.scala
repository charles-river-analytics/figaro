/*
 * MetropolisHastings.scala
 * Metropolis-Hastings sampler.
 *
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Jan 1, 2009
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
 * Metropolis-Hastings samplers.
 *
 * @param burnIn The number of iterations to run before samples are collected
 * @param interval The number of iterations to perform between collecting samples
 *
 */
// proposalScheme might evaluate to a different step each time it is called; making it by name gets the right effect.
abstract class MetropolisHastings(universe: Universe, proposalScheme: ProposalScheme, burnIn: Int, interval: Int,
  targets: Element[_]*)
  extends UnweightedSampler(universe, targets: _*) {
  import MetropolisHastings._

  // Used for debugging
  private var elementsToTrack: Map[Element[_], Null] = Map()
  private var proposalCounts: Map[Element[_], Int] = Map()
  // Make sure these maps don't cause memory leaks
  universe.register(elementsToTrack)
  universe.register(proposalCounts)

  private var accepts = 0
  private var rejects = 0

  private val currentConstraintValues: Map[Element[_], Double] = Map()
  universe.register(currentConstraintValues)

  /**
   * Get the acceptance ratio for the sampler.
   */
  def acceptRejectRatio = accepts.toDouble / (accepts.toDouble + rejects.toDouble)

  /**
   * Set this flag to true to obtain debugging information.
   */
  var debug = false

  private def newState: State = State(Map(), Map(), 0.0, 0.0, scala.collection.mutable.Set())

  private val fastTargets = targets.toSet

  /*
   * We continually update the values of elements while making a proposal. In order to be able to undo it, we need to
   * store the old value.
   * We keep track of the improvement in the constraints for the new proposal compared to the original value.
   * We keep track of which elements do not have their condition satisfied by the new proposal.
   */
  private def attemptChange[T](state: State, elem: Element[T]): State = {
    val newValue = {
      // Don't generate a new value for an observed element because it won't agree with the observation
      // For a compound element we can't do this because we have to condition the arguments by the
      // probability of generating the correct value.
      if (elem.observation.isEmpty || !elem.isInstanceOf[Atomic[_]]) elem.generateValue(elem.randomness)
      else elem.observation.get
    }
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
      if (randomness == oldRandomness) {
        state
      } else {
        val newOldRandomness = {
          state.oldRandomness + (elem -> elem.randomness)
        }
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
    val updatesNeeded = state1.oldValues.keySet flatMap (elem => universe.usedBy(elem))
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
    val scores = constrainedElements.map { e =>
      e.constraintValue - currentConstraintValues.getOrElseUpdate(e, 0.0)
    }
    scores.sum
  }

  private def accept(state: State): Unit = {
    if (debug) println("Accepting!\n")
    currentConstraintValues.keys.foreach(e => currentConstraintValues += e -> e.constraintValue)
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

    /* Have to call generateValue on chains after a rejection to restore the old resulting
     * element. We can't do this above because we have to ensure the value of parent is restored before we
     * do this.
     */
    for ((elem, value) <- state.oldValues) {
      elem match {
        case c: Chain[_, _] => c.generateValue
        case _ =>
      }
    }
  }

  /*
   * The rule for acceptance is:
   * - if the current status is the InitialStatus always accept; otherwise
   * - if the new set of dissatisfied elements is a proper subset of the old set, always accept
   * - if it is a proper superset, never accept
   * - otherwise, use the acceptance probability
   */
  private def decideToAccept(newState: State): Boolean = {
    val nothingNewDissatisfied = newState.dissatisfied subsetOf dissatisfied
    val somethingOldSatisfied = dissatisfied exists (_.conditionSatisfied)
    if (nothingNewDissatisfied && somethingOldSatisfied) true
    else if (!nothingNewDissatisfied && !somethingOldSatisfied) false
    else {
      log(random.nextDouble) < (newState.proposalProb + newState.modelProb)
    }
  }

  private def initConstrainedValues() = {
    universe.constrainedElements.foreach(e => currentConstraintValues += (e -> e.constraintValue))
  }

  protected final def mhStep(): State = {
    val newStateUnconstrained = proposeAndUpdate()
    val newState = State(newStateUnconstrained.oldValues, newStateUnconstrained.oldRandomness,
      newStateUnconstrained.proposalProb, newStateUnconstrained.modelProb + computeScores, newStateUnconstrained.dissatisfied)
    if (decideToAccept(newState)) {
      accepts += 1
      accept(newState)
    } else {
      rejects += 1
      undo(newState)
    }
    newState
  }

  /**
   * Produce a single sample.
   */
  final def sample(): (Boolean, Sample) = {
    val newState = mhStep()
    if (dissatisfied.isEmpty) {
      val values = newState.oldValues.keys.filter(fastTargets.contains(_)) map (target => target -> target.value)
      (true, Map(values.toSeq: _*))
    } else {
      (false, Map())
    }

  }

  protected override final def doSample() = {
    for { i <- 1 to interval - 1 } { mhStep() }
    super.doSample()
  }

  protected def doInitialize(): Unit = {
    // Need to prime the universe to make sure all elements have a generated value
    Forward(universe)
    initConstrainedValues()
    for { i <- 1 to burnIn } mhStep()
  }
  /**
   * Test Metropolis-Hastings by repeatedly running a single step from the same initial state.
   * For each of a set of predicates, the fraction of times the predicate is satisfied by the resulting state
   * is returned. By the resulting state, we mean the new state if it is accepted and the original state
   * if not.
   */
  def test(numSamples: Int, predicates: Seq[Predicate[_]], elementsToTrack: Seq[Element[_]]): (Double, scala.collection.Map[Predicate[_], Double], scala.collection.Map[Element[_], Double]) = {
    val savedAccepts = accepts
    val savedRejects = rejects
    val savedProposalCounts = proposalCounts
    accepts = 0
    rejects = 0
    proposalCounts = Map((elementsToTrack map (_ -> 0)): _*)
    val successes: Map[Predicate[_], Int] = Map((predicates map (_ -> 0)): _*)
    this.elementsToTrack = Map((elementsToTrack map (_ -> null): _*))
    def collectResults() =
      for { predicate <- predicates } {
        if (predicate.test) successes += predicate -> (successes(predicate) + 1)
      }

    for { i <- 1 to numSamples } {
      val newStateUnconstrained = proposeAndUpdate()
      val state1 = State(newStateUnconstrained.oldValues, newStateUnconstrained.oldRandomness,
        newStateUnconstrained.proposalProb, newStateUnconstrained.modelProb + computeScores, newStateUnconstrained.dissatisfied)
      if (decideToAccept(state1)) {
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
    val successResults: scala.collection.Map[Predicate[_], Double] = successes.mapValues(_.toDouble / numSamples)
    val proposalResults: scala.collection.Map[Element[_], Double] = proposalCounts.mapValues(_.toDouble / numSamples)
    val acceptanceRatio = accepts.toDouble / numSamples
    accepts = savedAccepts
    rejects = savedRejects
    proposalCounts = savedProposalCounts
    (acceptanceRatio, successResults, proposalResults)
  }
}

/**
 * Anytime Metropolis-Hastings sampler.
 * @param burnIn The number of iterations to run before samples are collected
 * @param interval The number of iterations to perform between collecting samples
 *
 */
class AnytimeMetropolisHastings(universe: Universe,
  scheme: ProposalScheme, burnIn: Int, interval: Int,
  targets: Element[_]*)
  extends MetropolisHastings(universe, scheme, burnIn, interval, targets: _*)
  with AnytimeProbQuerySampler {
  /**
   * Initialize the sampler.
   */
  override def initialize(): Unit = {
    super.initialize()
    doInitialize()
  }

  /**
   * Clean up the sampler, freeing memory.
   */
  override def cleanUp(): Unit = {
    universe.clearTemporaries()
    super.cleanUp()
  }
}

/**
 * One-time Metropolis-Hastings sampler.
 *
 * @param burnIn The number of iterations to run before samples are collected
 * @param interval The number of iterations to perform between collecting samples
 *
 */
class OneTimeMetropolisHastings(universe: Universe, myNumSamples: Int, scheme: ProposalScheme,
  burnIn: Int, interval: Int, targets: Element[_]*)
  extends MetropolisHastings(universe, scheme, burnIn, interval, targets: _*)
  with OneTimeProbQuerySampler {

  val numSamples = myNumSamples

  /**
   * Run the algorithm, performing its computation to completion.
   */
  override def run(): Unit = {
    doInitialize()
    super.run()
    update
  }
}

object MetropolisHastings {

  /**
   * Create an anytime Metropolis-Hastings sampler using the given proposal scheme with the given target
   * query elements.
   */
  def apply(scheme: ProposalScheme, targets: Element[_]*)(implicit universe: Universe) =
    new AnytimeMetropolisHastings(universe, scheme, 0, 1, targets: _*)

  /**
   * Create a one-time Metropolis-Hastings sampler using the given number of samples and proposal
   * scheme with the given target query elements.
   */
  def apply(numSamples: Int, scheme: ProposalScheme, targets: Element[_]*)(implicit universe: Universe) =
    new OneTimeMetropolisHastings(universe, numSamples, scheme, 0, 1, targets: _*)

  /**
   * Create an anytime Metropolis-Hastings sampler using the given proposal scheme and number
   * of burn-in samples with the given target query elements.
   */
  def apply(scheme: ProposalScheme, burnIn: Int, targets: Element[_]*)(implicit universe: Universe) =
    new AnytimeMetropolisHastings(universe, scheme, burnIn, 1, targets: _*)

  /**
   * Create a one-time Metropolis-Hastings sampler using the given number of samples, proposal scheme, and
   * number of burn-in samples with the given target query elements.
   */
  def apply(numSamples: Int, scheme: ProposalScheme, burnIn: Int, targets: Element[_]*)(implicit universe: Universe) =
    new OneTimeMetropolisHastings(universe, numSamples, scheme, burnIn, 1, targets: _*)

  /**
   * Create an anytime Metropolis-Hastings sampler using the given proposal scheme, number of burn-in
   * samples, and interval between samples with the given target query elements.
   */
  def apply(scheme: ProposalScheme, burnIn: Int, interval: Int, targets: Element[_]*)(implicit universe: Universe) =
    new AnytimeMetropolisHastings(universe, scheme, burnIn, interval, targets: _*)

  /**
   * Create a one-time Metropolis-Hastings sampler using the given number of samples, proposal scheme,
   * number of burn-in samples, and interval between samples with the given target query elements.
   */
  def apply(numSamples: Int, scheme: ProposalScheme,
    burnIn: Int, interval: Int, targets: Element[_]*)(implicit universe: Universe) =
    new OneTimeMetropolisHastings(universe, numSamples, scheme, burnIn, interval: Int, targets: _*)

  /**
   * Use MH to compute the probability that the given element has the given value.
   */
  def probability[T](target: Element[T], predicate: T => Boolean): Double = {
    val alg = MetropolisHastings(1000000, ProposalScheme.default, target)
    alg.start()
    val result = alg.probability(target, predicate)
    alg.kill()
    result
  }

  /**
   * Use MH to compute the probability that the given element has the given value.
   */
  def probability[T](target: Element[T], value: T): Double =
    probability(target, (t: T) => t == value)

  private[figaro] case class State(oldValues: Map[Element[_], Any],
    oldRandomness: Map[Element[_], Any],
    proposalProb: Double,
    modelProb: Double,
    dissatisfied: scala.collection.mutable.Set[Element[_]])
}
