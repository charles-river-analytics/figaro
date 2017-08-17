/*
 * MetropolisHastings.scala
 * Metropolis-Hastings sampler.
 *
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Jan 1, 2009
 *
 * Copyright 2017 Avrom J. Pfeffer and Charles River Analytics, Inc.
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
import com.cra.figaro.library.cache._
import com.cra.figaro.library.collection.Container

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
    extends BaseUnweightedSampler(universe, targets: _*) {
  import MetropolisHastings._

  // Used for debugging
  private var elementsToTrack: Map[Element[_], Null] = Map()
  private var proposalCounts: Map[Element[_], Int] = Map()
  // Make sure these maps don't cause memory leaks
  universe.register(elementsToTrack)
  universe.register(proposalCounts)

  protected var accepts = 0
  protected var rejects = 0

  protected val currentConstraintValues: Map[Element[_], Double] = Map()
  universe.register(currentConstraintValues)

  /**
   * Get the acceptance ratio for the sampler.
   */
  def acceptRejectRatio = accepts.toDouble / (accepts.toDouble + rejects.toDouble)

  /**
   * Set this flag to true to obtain debugging information.
   */
  var debug = false

  /* Stores the topologically sorted list of updates needed for a given set of proposed elements */
  protected var proposedElementsSortedUpdates: Map[Iterable[Element[_]], List[Element[_]]] = Map()

  /* Stores the list of elements used by each proposed element */
  protected var elementsUsedBy: Map[Element[_], Set[Element[_]]] = Map()

  /**
   * Set this flag to true when constraints are bound between 0 and 1
   * to enable early rejection of states with constrained elements.
   */
  var constraintsBound = false

  protected var acceptProbability = 0.0
  protected var constraintsSum = 0.0
  protected var oldPropProb = 0.0
  protected var oldModelProb = 0.0

  private def newState: State = State(Map(), Map(), 0.0, 0.0, scala.collection.mutable.Set(), List())

  protected val fastTargets = targets.toSet

  protected var chainCache: Cache = new MHCache(universe)

  /*
   * We continually update the values of elements while making a proposal. In order to undo it, we must store the old value.
   * We keep track of the improvement in the constraints for the new proposal compared to the original value.
   * We also keep track of which elements do not have their condition satisfied by the new proposal.
   */
  private def attemptChange[T](state: State, elem: Element[T]): State = {
    val newOldValues = state.oldValues + (elem -> elem.value)
    /* Do not generate a new value for an observed element because it will not agree with the observation.
     * For a compound element we cannot do this because we have to condition the arguments by the
     * probability of generating the correct value.
     */
    val newValue = if (elem.observation.isEmpty || !elem.isInstanceOf[Atomic[_]]) {
      chainCache(elem) match {
        case None => elem.generateValue(elem.randomness)
        case Some(result) =>
          if (result.value == null) result.generate
          result.value
      }
    } else elem.observation.get
    elem.value = newValue

    /* Early rejection of states is possible when attempting to change constrained or conditioned elements.
     * If the constraints are bound between 0 and 1 (indicated by the constraintsBound flag), 
     * there were previously no conditions dissatisfied, and there is a constraint on the element, then
     * early rejection occurs (via the throwable RejectState object) if the logical statement is true.
     * If the element has a condition on it, and that condition was newly dissatisfied, we also reject early.
     */
    var newDissatisfied = state.dissatisfied
    if (constraintsBound && state.dissatisfied.isEmpty && !elem.allConstraints.isEmpty) {
      val constraintValue = elem.constraintValue
      if (constraintValue < (acceptProbability + constraintsSum
        + (oldModelProb - state.modelProb)
        + (state.proposalProb - oldPropProb))) {
        val result = State(newOldValues, state.oldRandomness, state.proposalProb, state.modelProb, state.dissatisfied, elem +: state.reverseVisitOrder)
        throw new RejectState(result)
      } else {
        constraintsSum -= constraintValue
      }
    } else if (elem.condition(newValue)) {
      newDissatisfied -= elem
    } else if (!elem.condition(newValue)) {
      newDissatisfied += elem
      if (!(state.dissatisfied contains elem)) {
        val result = State(newOldValues, state.oldRandomness, state.proposalProb, state.modelProb, newDissatisfied, elem +: state.reverseVisitOrder)
        throw new RejectState(result)
      }
    }

    State(newOldValues, state.oldRandomness, state.proposalProb, state.modelProb, newDissatisfied, elem +: state.reverseVisitOrder)
  }

  private def propose[T](state: State, elem: Element[T]): State = {
    oldPropProb = state.proposalProb
    oldModelProb = state.modelProb
    if (elem.intervention.isDefined) {
      state
    } else {
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
          elem.randomness = randomness
          State(state.oldValues, newOldRandomness, state.proposalProb + log(proposalProb), state.modelProb + log(modelProb), state.dissatisfied, state.reverseVisitOrder)
        }
      val result = attemptChange(state1, elem)
      if (debug) println("old randomness = " + oldRandomness +
        ", old value = " + oldValue +
        ", new randomness = " + elem.randomness +
        ", new value = " + elem.value)
      result
    }
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
      State(state.oldValues, newOldRandomness2, state.proposalProb, state.modelProb, state.dissatisfied, state.reverseVisitOrder)
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
      case None       => state
      case Some(step) => runStep(state, step)
    }

  private def runStep(state: State, step: ProposalScheme): State =
    step match {
      case FinalScheme(elem) => propose(state, elem())
      case TypedScheme(first, rest) =>
        val firstElem = first()
        val state1 = propose(state, proposeChainCheck(firstElem))
        continue(state1, rest(firstElem.value))
      case UntypedScheme(first, rest) =>
        val firstElem = first()
        val state1 = propose(state, proposeChainCheck(firstElem))
        continue(state1, rest)
      case ds: DisjointScheme =>
        val (probs, schemes) = ds.choices.toList.unzip
        val choice = sampleMultinomial(normalize(probs) zip schemes)
        runStep(state, choice())
      case SwitchScheme(first, rest) =>
        val (elem1, elem2) = first()
        val state1 = switch(state, proposeChainCheck(elem1), proposeChainCheck(elem2))
        continue(state1, rest)
    }

  private def proposeChainCheck(elem: Element[_]): Element[_] = {
    val e = chainCache(elem)
    if (e.isEmpty) elem else e.get.asInstanceOf[Element[_]]
  }

  protected def runScheme(): State = runStep(newState, proposalScheme)

  /*
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
    if (elem.intervention.isDefined) state
    else {
      if (debug) println("Updating " + elem.name.string)
      val oldValue = elem.value
      val result = attemptChange(state, elem)
      if (debug) println("randomness = " + elem.randomness + ", old value = " + oldValue + ", new value = " + elem.value)
      result
    }
  }

  /*
   * A single step of MetropolisHastings lists of proposing according to the scheme and updating any elements that depend on those
   * changed.
   */
  protected def proposeAndUpdate(): State = {
    /* Before making any proposals, choose the acceptProbability */
    acceptProbability = log(random.nextDouble)

    val state1 = runScheme()

    val proposedElements = state1.oldValues.keySet
    for (elem <- proposedElements) {

      /* elementsUsedBy stores the list of elements used by each proposed element.
       * If an element is being proposed for the first time, it will not appear in elementsUsedBy (or proposedElementsSortedUpdates).
       * Therefore, the element and its usedBy list can simply be added to elementsUsedBy. 
       * This is true for all first time proposals for permanent elements and temporary elements.
       * Temporary elements most often go through this process as they are brand new elements upon creation.
       */
      val usedBy = universe.usedBy(elem).toSet
      if (!elementsUsedBy.contains(elem)) {
        elementsUsedBy += (elem -> usedBy)
      } else if (usedBy != elementsUsedBy(elem)) {
        /*  An element's usedBy list can change in between proposals, so we must compare its latest usedBy list to its previous 
         *  usedBy list (as stored in elementsUsedBy). If the two lists do not match, the element is invalidated and we must remove 
         *  its stored sorted update lists for all proposed element sets that contain the invalidated element. We also overwrite the old
         *  usedBy list in elementsUsedBy with the new list.
         */
        if (debug) println("Invalidating " + elem.toNameString)
        println("Invalidating " + elem.toNameString)
        val invalidatedSets = proposedElementsSortedUpdates.keySet.filter(key => key.toSet.contains(elem))
        proposedElementsSortedUpdates --= invalidatedSets
        elementsUsedBy += (elem -> usedBy)
      }
    }

    /* proposedElementsSortedUpdate stores the topologically list of updates needed for a given set of proposed elements.
     * If proposedElementsSortedUpdate does not contain the set of proposed elements, we must store and store the updates needed.
     */
    if (!proposedElementsSortedUpdates.contains(proposedElements)) {
      val updatesNeeded = proposedElements flatMap (elem => elementsUsedBy.getOrElse(elem, Set()))
      val topologicallySorted = TopSort.topologicallySort(updatesNeeded, universe)
      proposedElementsSortedUpdates += (proposedElements -> topologicallySorted)
    }

    /* We must retrieve the list of updates needed, and update one element at at time. */
    var state = state1
    var updatesNeeded = proposedElementsSortedUpdates(proposedElements)
    if (constraintsBound) {
      constraintsSum = updatesNeeded.map(e => e.constraintValue).sum
    }
    while (updatesNeeded.nonEmpty) {
      state = updateOne(state, updatesNeeded.head)
      updatesNeeded = updatesNeeded.tail
    }
    state
  }

  protected var dissatisfied: Set[Element[_]] = _

  protected def getDissatisfied = dissatisfied // for testing

  /*
   * Computes the scores of the constrained elements from caches scores. If the element
   * is not found in the cache, then it's value is 1.0.
   */
  protected def computeScores(): Double = {
    // A variable with an intervention shouldn't be scored
    val constrainedElements = universe.constrainedElements.filter(_.intervention.isEmpty)
    val scores = constrainedElements.map { e =>
      e.constraintValue - currentConstraintValues.getOrElseUpdate(e, 0.0)
    }
    scores.sum
  }

  protected def accept(state: State): Unit = {
    if (debug) println("Accepting!\n")
    currentConstraintValues.keys.foreach(e => currentConstraintValues += e -> e.constraintValue)
    dissatisfied = (dissatisfied filter (e => !e.conditionSatisfied && e.intervention.isEmpty)) ++ state.dissatisfied
  }

  private def setValue(pair: (Element[_], Any)) = {
    val (elem, value) = pair
    elem.value = value.asInstanceOf[elem.Value]
  }

  private def setRandomness(pair: (Element[_], Any)) = {
    val (elem, randomness) = pair
    elem.randomness = randomness.asInstanceOf[elem.Randomness]
  }

  protected def undo(state: State): Unit = {
    if (debug) println("Rejecting!\n")
    val visitOrder = state.reverseVisitOrder.reverse
    visitOrder.foreach { elem =>
      elem match {
        case c: Chain[_, _] => {
          chainCache(c) match {
            case Some(result) => c.value = result.value.asInstanceOf[c.Value]
            case None         => throw new AlgorithmException
          }
        }
        case _ => {
          if (state.oldRandomness.contains(elem)) elem.randomness = state.oldRandomness(elem).asInstanceOf[elem.Randomness]
          elem.value = state.oldValues(elem).asInstanceOf[elem.Value]
        }
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
  protected def decideToAccept(newState: State): Boolean = {
    val nothingNewDissatisfied = newState.dissatisfied subsetOf dissatisfied
    val somethingOldSatisfied = dissatisfied exists (_.conditionSatisfied)
    if (nothingNewDissatisfied && somethingOldSatisfied) true
    else if (!nothingNewDissatisfied && !somethingOldSatisfied) false
    else {
      acceptProbability < (newState.proposalProb + newState.modelProb)
    }
  }

  protected def initConstrainedValues() = {
    universe.constrainedElements.foreach(e => currentConstraintValues += (e -> e.constraintValue))
  }

  /* TODO
   * 
   */
  protected def mhStep(): State = {
    try {
      val newStateUnconstrained = proposeAndUpdate()
      val newState = State(newStateUnconstrained.oldValues, newStateUnconstrained.oldRandomness,
        newStateUnconstrained.proposalProb, newStateUnconstrained.modelProb + computeScores, newStateUnconstrained.dissatisfied, newStateUnconstrained.reverseVisitOrder)
      if (decideToAccept(newState)) {
        accepts += 1
        accept(newState)
      } else {
        throw new RejectState(newState)
      }
      newState
    } catch {
      case reject: RejectState => {
        rejects += 1
        undo(reject.state)
        reject.state
      }
    }
  }

  /**
   * Produce a single sample.
   */
  def sample(): (Boolean, Sample) = {
    val newState = mhStep()
    if (dissatisfied.isEmpty) {
      val values = newState.oldValues.keys.filter(fastTargets.contains(_)) map (target => target -> target.value)
      (true, Map(values.toSeq: _*))
    } else {
      (false, Map())
    }

  }

  protected override def doSample() = {
    for { i <- 1 to interval - 1 } { mhStep() }
    super.doSample()
  }

  protected def doInitialize(): Unit = {
    // Need to prime the universe to make sure all elements have a generated value
    Forward(universe, chainCache)
    initConstrainedValues()
    dissatisfied = universe.conditionedElements.toSet filter (!_.conditionSatisfied)
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
    dissatisfied = universe.conditionedElements.toSet filter (!_.conditionSatisfied)
    def collectResults() =
      for { predicate <- predicates } {
        if (predicate.test) successes += predicate -> (successes(predicate) + 1)
      }

    for { i <- 1 to numSamples } {
      val newStateUnconstrained = proposeAndUpdate()
      val state1 = State(newStateUnconstrained.oldValues, newStateUnconstrained.oldRandomness,
        newStateUnconstrained.proposalProb, newStateUnconstrained.modelProb + computeScores, newStateUnconstrained.dissatisfied, newStateUnconstrained.reverseVisitOrder)
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

  /**
   * Clean up the sampler, freeing memory.
   */
  override def cleanUp(): Unit = {
    super.cleanUp()
    universe.clearTemporaries()
    chainCache.clear()
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
    with UnweightedSampler with AnytimeProbQuerySampler {
  /**
   * Initialize the sampler.
   */
  override def initialize(): Unit = {
    super.initialize()
    doInitialize()
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
    with UnweightedSampler with OneTimeProbQuerySampler {

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

  /**
   * Use MH to sample the joint posterior distribution of several variables
   */
  def sampleJointPosterior(targets: Element[_]*)(implicit universe: Universe): Stream[List[Any]] = {
    val jointElement = Container(targets: _*).foldLeft(List[Any]())((l: List[Any], i: Any) => l :+ i)
    val alg = MetropolisHastings(1000000, ProposalScheme.default, jointElement)(universe)
    alg.start()
    val posterior = alg.sampleFromPosterior(jointElement)
    alg.kill()
    posterior
  }

  private[figaro] case class State(oldValues: Map[Element[_], Any],
                                   oldRandomness: Map[Element[_], Any],
                                   proposalProb: Double,
                                   modelProb: Double,
                                   dissatisfied: scala.collection.mutable.Set[Element[_]],
                                   reverseVisitOrder: List[Element[_]])

  case class RejectState(state: State) extends Throwable
}