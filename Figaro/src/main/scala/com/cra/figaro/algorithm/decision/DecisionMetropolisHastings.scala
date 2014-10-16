/*
 * DecisionMetropolisHastings.scala
 * Decision Metropolis-Hastings sampler.
 * 
 * Created By:      Brian Ruttenberg (bruttenberg@cra.com)
 * Creation Date:   Oct 1, 2012
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.algorithm.decision

import com.cra.figaro.algorithm._
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.language._
import com.cra.figaro.library.compound._
import com.cra.figaro.library.decision._
import com.cra.figaro.library.decision.DecisionUtil._
import com.cra.figaro.util._
import scala.collection.mutable.Map
import scala.language.existentials
import scala.math.log
import scala.annotation.tailrec

/**
 * Metropolis-Hastings Decision sampler. Almost the exact same as normal MH except that it keeps
 * track of utilities and probabilities (to compute expected utility) and it implements DecisionAlgorithm trait
 */

abstract class DecisionMetropolisHastings[T, U] private (universe: Universe, proposalScheme: ProposalScheme, burnIn: Int, interval: Int,
  utilityNodes: List[Element[_]], decisionTarget: Decision[T, U], dummyTarget: Element[_])
  extends UnweightedSampler(universe, dummyTarget) with DecisionAlgorithm[T, U] {

  def this(universe: Universe, proposalScheme: ProposalScheme, burnIn: Int, interval: Int,
    utilityNodes: List[Element[_]], decisionTarget: Decision[T, U]) = this(universe, proposalScheme,
    burnIn, interval, utilityNodes, decisionTarget, createDecisionDummy(decisionTarget))

  import MetropolisHastings._

  // Used for debugging
  private var elementsToTrack: Map[Element[_], Null] = Map()
  private var proposalCounts: Map[Element[_], Int] = Map()
  // Make sure these maps don't cause memory leaks
  universe.register(elementsToTrack)
  universe.register(proposalCounts)

  protected type WeightSeen[T] = (Element[T], Map[T, Double])
  protected def newWeightSeen[T](target: Element[T]): WeightSeen[T] = (target, Map())

  /**
   * Contains all of the sample data (decision values, utilities) for a given target decision
   */
  private var allUtilitiesSeen: List[WeightSeen[_]] = _

  private def utilitySum = (0.0 /: utilityNodes)((s: Double, n: Element[_]) => s + n.value.asInstanceOf[Double])

  /**
   * Cleans up the temporary elements created during sampling
   */
  def cleanup() = universe.deactivate(queryTargets)

  /* Overrides DecisionAlgorithm Trait */
  def computeUtility(): scala.collection.immutable.Map[(T, U), DecisionSample] = {
    val TimesSeen = allTimesSeen.find(_._1 == dummyTarget).get._2.asInstanceOf[Map[(T, U), Int]]
    val utilitySeen = allUtilitiesSeen.find(_._1 == dummyTarget).get._2.asInstanceOf[Map[(T, U), Double]]
    (utilitySeen.map(v => (v._1, DecisionSample(v._2, TimesSeen(v._1).toDouble)))).toMap
  }

  // override reset so we can reset the local utilities
  override protected def resetCounts() = {
    allUtilitiesSeen = queryTargets.toList map (newWeightSeen(_))
    super.resetCounts()
  }

  /**
   * Set this flag to true to obtain debugging information
   */
  var debug = false

  private def newState: State = State(Map(), Map(), 0.0, 0.0, scala.collection.mutable.Set())

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
//val newProb =
// state.modelProb * elem.score(elem.value, newValue)
val newProb = state.modelProb
val newDissatisfied =
if (elem.condition(newValue)) state.dissatisfied -= elem; else state.dissatisfied += elem
elem.value = newValue
State(newOldValues, state.oldRandomness, state.proposalProb, newProb, newDissatisfied)
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
      ", new value = " + elem.value +
      ", new prob = " + state1.modelProb)
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

def updateMany[T](state: State, toUpdate: Set[Element[_]]): State = {
    var returnState = state
    var updatesLeft = toUpdate
    while (!updatesLeft.isEmpty){ 
       var argsRemaining = universe.uses(updatesLeft.head).intersect(updatesLeft)
        while (!argsRemaining.isEmpty) { 
        returnState = updateManyHelper(returnState, argsRemaining.toSet) 
        argsRemaining = argsRemaining.tail 
      }
      returnState = updateOne(returnState, updatesLeft.head) 
      updatesLeft = updatesLeft.tail
      }
      returnState
  }
  
  @tailrec
  private def updateManyHelper(state: State, toUpdate: Set[Element[_]]): State = {
	  var returnState = state
	  var updatesLeft = toUpdate
	  var argsRemaining = universe.uses(updatesLeft.head).intersect(updatesLeft)
	  if (argsRemaining.isEmpty){
	        returnState = updateOne(returnState, updatesLeft.head) 
	        returnState } 
	  else { updateManyHelper(returnState, argsRemaining.toSet) }
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

  private def accept(state: State): Unit = {
    if (debug) println("Accepting!\n")
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
  private def decideToAccept(newState: State): Boolean = {
    val nothingNewDissatisfied = newState.dissatisfied subsetOf dissatisfied
    val somethingOldSatisfied = dissatisfied exists (_.conditionSatisfied)
    if (nothingNewDissatisfied && somethingOldSatisfied) true
    else if (!nothingNewDissatisfied && !somethingOldSatisfied) false
    else log(random.nextDouble) < (newState.proposalProb + newState.modelProb)
  }

  private final def mhStep(): Unit = {
    val newState = proposeAndUpdate()
    if (decideToAccept(newState)) accept(newState)
    else undo(newState)
  }

  protected def updateWeightSeenWithValue[T](value: T, weight: Double, weightSeen: WeightSeen[T]): Unit =
    weightSeen._2 += value -> (weightSeen._2.getOrElse(value, 0.0) + weight)

  protected def updateWeightSeenForTarget[T](sample: (Double, Map[Element[_], Any]), weightSeen: WeightSeen[T]): Unit = {
    val (weight, values) = sample
    val value = values(weightSeen._1).asInstanceOf[T]
    updateWeightSeenWithValue(value, weight, weightSeen)
  }

  /**
   * Produce a single sample.
   */
  final def sample(): (Boolean, Sample) = {
    mhStep()
    if (dissatisfied.isEmpty) {
      val values = queryTargets map (target => target -> target.value)
      (true, Map(values: _*))
    } else {
      (false, Map())
    }

  }

  /*
   * In the decision version of importance sampling, we keep track of both the sampled utility
   * and the state probability, so we have to maintain two data structures (allUtilitiesSeen and allTimesSeen)
   */
  protected override final def doSample() = {
    for { i <- 1 to interval - 1 } { mhStep() }
    val s = sample()
    if (sampleCount == 0) {
      initUpdates
    }
    if (s._1) {
      allUtilitiesSeen foreach (updateWeightSeenForTarget((utilitySum, s._2), _))
      sampleCount += 1
      s._2 foreach (t => updateTimesSeenForTarget(t._1.asInstanceOf[Element[t._1.Value]], t._2.asInstanceOf[t._1.Value]))
    }
  }

  protected override def update(): Unit = {
    super.update
    sampleCount += 1
    allUtilitiesSeen foreach (updateWeightSeenForTarget((utilitySum, Map[Element[_], Any](dummyTarget -> dummyTarget.value)), _))    
    sampleCount -= 1
  }

  protected def doInitialize(): Unit = {
    Forward(universe)
    for { i <- 1 to burnIn } mhStep()
  }

  /**
   * Test Metropolis-Hastings Decisions by repeatedly running a single step from the same initial state.
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
      val state1 = proposeAndUpdate()
      if (decideToAccept(state1)) {
        // collect results for the new state and restore the original state
        collectResults()
        undo(state1)
      } else {
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
 * Anytime Decision Metropolis-Hastings sampler.
 */
class AnytimeDecisionMetropolisHastings[T, U](universe: Universe,
  scheme: ProposalScheme, burnIn: Int, interval: Int, utilityNodes: List[Element[_]],
  decisionTarget: Decision[T, U])
  extends DecisionMetropolisHastings(universe, scheme, burnIn, interval, utilityNodes, decisionTarget)
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
 * One-time Decision Metropolis-Hastings sampler.
 */
class OneTimeDecisionMetropolisHastings[T, U](universe: Universe, myNumSamples: Int, scheme: ProposalScheme,
  burnIn: Int, interval: Int, utilityNodes: List[Element[_]],
  decisionTarget: Decision[T, U])
  extends DecisionMetropolisHastings(universe, scheme, burnIn, interval, utilityNodes, decisionTarget)
  with OneTimeProbQuerySampler {
  /**
   * Number of samples to take.
   */
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

object DecisionMetropolisHastings {

  /* Checks conditions of Decision Usage
   * 1. Double utilities
   */
  private def UsageCheck(utilityNodes: List[Element[_]], target: Decision[_, _]) = {
    utilityNodes.foreach { u =>
      u.value match {
        case d: Double => 1
        case _ => {
          println(u.value)
          throw new IllegalArgumentException("Only double utilities are allowed")
        }
      }
    }
  }

  /**
   * Create an Anytime DecisionMetropolis-Hastings sampler using the given proposal scheme with the given
   * decision.
   */
  /*
   *  For decisions, we will create a dummy Element that is a tuple of the decision node and its parents. This will be used 
   *  to track expected utilities during the sampling
   * 
   */
  def apply[T, U](scheme: ProposalScheme, utilityNodes: List[Element[_]], target: Decision[T, U])(implicit universe: Universe) = {
    utilityNodes.foreach(_.generate)
    UsageCheck(utilityNodes, target)
    new AnytimeDecisionMetropolisHastings[T, U](universe, scheme, 0, 1, utilityNodes, target)
  }

  /**
   * Create a OneTime DecisionMetropolis-Hastings sampler using the given number of samples and proposal
   * scheme with the given decision.
   */
  def apply[T, U](numSamples: Int, scheme: ProposalScheme, utilityNodes: List[Element[_]], target: Decision[T, U])(implicit universe: Universe) = {
    utilityNodes.foreach(_.generate)
    UsageCheck(utilityNodes, target)
    new OneTimeDecisionMetropolisHastings[T, U](universe, numSamples, scheme, 0, 1, utilityNodes, target)
  }

  /**
   * Create an Anytime DecisionMetropolis-Hastings sampler using the given proposal scheme and number
   * of burn-in samples with the given decision.
   */
  def apply[T, U](scheme: ProposalScheme, burnIn: Int, utilityNodes: List[Element[_]], target: Decision[T, U])(implicit universe: Universe) = {
    utilityNodes.foreach(_.generate)
    UsageCheck(utilityNodes, target)
    new AnytimeDecisionMetropolisHastings[T, U](universe, scheme, burnIn, 1, utilityNodes, target)
  }

  /**
   * Create a OneTime DecisionMetropolis-Hastings sampler using the given number of samples, proposal scheme, and
   * number of burn-in samples with the given decision.
   */
  def apply[T, U](numSamples: Int, scheme: ProposalScheme, burnIn: Int, utilityNodes: List[Element[_]], target: Decision[T, U])(implicit universe: Universe) = {
    utilityNodes.foreach(_.generate)
    UsageCheck(utilityNodes, target)
    new OneTimeDecisionMetropolisHastings[T, U](universe, numSamples, scheme, burnIn, 1, utilityNodes, target)
  }

  /**
   * Create an Anytime DecisionMetropolis-Hastings sampler using the given proposal scheme, number of burn-in
   * samples, and interval between samples with the given decision.
   */
  def apply[T, U](scheme: ProposalScheme, burnIn: Int, interval: Int, utilityNodes: List[Element[_]], target: Decision[T, U])(implicit universe: Universe) = {
    utilityNodes.foreach(_.generate)
    UsageCheck(utilityNodes, target)
    new AnytimeDecisionMetropolisHastings[T, U](universe, scheme, burnIn, interval, utilityNodes, target)
  }

  /**
   * Create a OneTime DecisionMetropolis-Hastings sampler using the given number of samples, proposal scheme,
   * number of burn-in samples, and interval between samples with the given decision.
   */
  def apply[T, U](numSamples: Int, scheme: ProposalScheme,
    burnIn: Int, interval: Int, utilityNodes: List[Element[_]], target: Decision[T, U])(implicit universe: Universe) = {
    utilityNodes.foreach(_.generate)
    UsageCheck(utilityNodes, target)
    new OneTimeDecisionMetropolisHastings[T, U](universe, numSamples, scheme, burnIn, interval: Int, utilityNodes, target)
  }

  private[figaro] case class State(oldValues: Map[Element[_], Any],
    oldRandomness: Map[Element[_], Any],
    proposalProb: Double,
    modelProb: Double,
    dissatisfied: scala.collection.mutable.Set[Element[_]])
}
