/*
 * DecisionImportance.scala
 * Decision Importance sampler.
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
import scala.annotation.tailrec
import scala.collection.mutable.{ Set, Map }

/*
 *  Importance sampling uses a dummy element to track the sampled parent/decision values and observed utilities
 *  This dummyTarget is just a tuple element of the parent and decision
 */
/**
 * Importance sampling for decisions. Almost the exact same as normal importance sampling except that it keeps
 * track of utilities and probabilities (to compute expected utility) and it implements DecisionAlgorithm trait.
 */

abstract class DecisionImportance[T, U] private (override val universe: Universe, utilityNodes: List[Element[_]], decisionTarget: Decision[T, U],
  dummyTarget: Element[_]) extends WeightedSampler(universe, dummyTarget) with DecisionAlgorithm[T, U] {

  def this(universe: Universe, utilityNodes: List[Element[_]], decisionTarget: Decision[T, U]) =
    this(universe, utilityNodes, decisionTarget, createDecisionDummy(decisionTarget))

  import Importance.State

  private var allUtilitiesSeen: List[WeightSeen[_]] = _

  private def utilitySum = (0.0 /: utilityNodes)((s: Double, n: Element[_]) => s + n.value.asInstanceOf[Double])

  /**
   * Cleans up the temporary elements created during sampling.
   */
  def cleanup() = universe.deactivate(queryTargets)

  /* Overrides DecisionAlgorithm Trait */
  // have to normalize the utilities by the sum of the weights for each (parent, decision) combo
  def computeUtility(): scala.collection.immutable.Map[(T, U), DecisionSample] = {
    val weightSeen = allWeightsSeen.find(_._1 == dummyTarget).get._2.asInstanceOf[Map[(T, U), Double]]
    val utilitySeen = allUtilitiesSeen.find(_._1 == dummyTarget).get._2.asInstanceOf[Map[(T, U), Double]]
    (utilitySeen.map(v => (v._1, DecisionSample(v._2, math.exp(weightSeen(v._1)))))).toMap
  }

  // override reset so we can reset the local utilities
  override protected def resetCounts() = {
    allUtilitiesSeen = queryTargets.toList map (newWeightSeen(_))
    super.resetCounts()
  }

  protected def updateWeightSeenWithValueNoLog[T](value: T, weight: Double, weightSeen: WeightSeen[T]): Unit =
    weightSeen._2 += value -> (weightSeen._2.getOrElse(value, 0.0) + weight)

  protected def updateWeightSeenForTargetNoLog[T](sample: Sample, weightSeen: WeightSeen[T]): Unit = {
    val (weight, values) = sample
    val value = values(weightSeen._1).asInstanceOf[T]
    updateWeightSeenWithValueNoLog(value, weight, weightSeen)
  }

  // override doSample so can update the local utilities
  override protected def doSample(): Unit = {
    val s = sample()
    totalWeight = logSum(s._1, totalWeight)
    allWeightsSeen foreach (updateWeightSeenForTarget(s, _))
    allUtilitiesSeen foreach (updateWeightSeenForTargetNoLog((math.exp(s._1) * utilitySum, s._2), _))
  }

  /**
   * Produce one weighted sample of the given element. weightedSample takes into account conditions and constraints
   * on all elements in the Universe, including those that depend on this element.
   *
   * For decisions, our weight is actually the weight of the sampled state times the sum of the utility nodes. This will be
   * used as the "weight" in the weighted sampler, ie, we are accumulating the expected utility of each state. Note that the weights
   * will not be normalized, but that is ok since strategies are an optimization and everything will be divided by a constant.
   *
   */
  @tailrec final def sample(): Sample = {
    val resultOpt: Option[Sample] =
      try {
        val state = State()
        // We must make a fresh copy of the active elements since sampling can add active elements to the Universe
        val activeElements = universe.activeElements
        activeElements.foreach(e => if (e.active) sampleOne(state, e))
        val bindings = queryTargets map (elem => elem -> elem.value)
        Some((state.weight, Map(bindings: _*)))
      } catch {
        case Importance.Reject => None
      }

    resultOpt match {
      case Some(x) => x
      case None =>
        sample()
    }
  }

  /*
   * Sample the value of an element. If it has already been assigned in the state, the current assignment is
   * used and the state is unchanged. Also, an element in a different universe is not sampled; instead its state is
   * used directly
   *
   * This is made private[figaro] to allow easy testing
   */
  private[figaro] def sampleOne[T](state: State, element: Element[T]): T =
    if (element.universe != universe || (state.assigned contains element)) element.value
    else {
      state.assigned += element
      sampleFresh(state, element)
    }

  /*
   * Sample a fresh value of an element, assuming it has not yet been assigned a value in the state. This sampling
   * takes into account the condition and constraint on the element. If the condition is violated, the entire sampling
   * process is rejected. This function returns the state including the new assignment to the element with the weight
   * of the constraint multiplied in.
   */
  private def sampleFresh[T](state: State, element: Element[T]): T = {
    val value = sampleValue(state, element)
    if (!element.condition(value)) throw Importance.Reject
    state.weight += element.constraint(value)
    value
  }

  /*
   * Sample the value of an element according to its generative model, without considering the condition or constraint.
   * Since sampling the value of this element might also involve sampling the values of related elements, the state
   * must be updated and returned.
   *
   * Most elements can simply be handled by sampling values for the arguments and generating values for this
   * element. Dist is an exception, because not all the outcomes need to be generated, but we only know which one
   * after we have sampled the randomness of the Dist. For this reason, we write special code to handle Dists.
   * For Chain, we also write special code to avoid calling get twice.
   */
  private def sampleValue[T](state: State, element: Element[T]): T =
    element match {
      case d: Dist[_, _] =>
        d match {
          case dc: CompoundDist[_] => dc.probs foreach (sampleOne(state, _))
          case _ => ()
        }
        val rand = d.generateRandomness()
        val index = d.selectIndex(rand)
        sampleOne(state, d.outcomeArray(index))
        d.value = d.finishGeneration(index)
        d.value
      case dec: Decision[_, _] =>
        val parentValue = if (universe.activeElements contains dec.parent) {
          sampleOne(state, dec.parent)
        } else {
          dec.parent.value
        }
        dec.value = sampleOne(state, dec.get(parentValue))
        dec.value
      case c: Chain[_, _] =>
        val parentValue = sampleOne(state, c.parent)
        c.value = sampleOne(state, c.get(parentValue))
        c.value
      case _ =>
        element.args foreach (sampleOne(state, _))
        element.randomness = element.generateRandomness()
        element.value = element.generateValue(element.randomness)
        element.value
    }
}

object DecisionImportance {
  /*
   * An element cannot be assigned more than once during importance sampling. If an element has been assigned, 
   * its assigned value will be held in its value field. A state consists of the set of variables that have 
   * been assigned, together with the accumulated weight so far. */
  case class State(assigned: Set[Element[_]] = Set(), var weight: Double = 0.0)

  object Reject extends RuntimeException

  /* Checks conditions of Decision Usage
   * 1. Double utilities
   */
  private def UsageCheck(utilityNodes: List[Element[_]], target: Decision[_, _]) = {
    utilityNodes.foreach { u =>
      u.value match {
        case d: Double => 1
        case _ => throw new IllegalArgumentException("Only double utilities are allowed")
      }
    }
  }

  /*
   *  For decisions, we will create a dummy Element that is a tuple of the decision node and its parents. This will be used 
   *  to track expected utilities during the sampling
   *
   *  NOTE: This assumes that the initial choices of decisions are uniform over the possible actions. 
   *  If this is not the case, then this class needs to be modified (ie, divide out the prob(decision) in the state weight)
   */
  /**
   * Create an Anytime DecisionImportance sampler with the given decision over the given universe.
   */
  def apply[T, U](utilityNodes: List[Element[_]], target: Decision[T, U])(implicit universe: Universe) = {
    utilityNodes.foreach(_.generate()) // need initial values for the utility nodes before the usage check
    UsageCheck(utilityNodes, target)
    new DecisionImportance[T, U](universe, utilityNodes, target) with AnytimeProbQuerySampler
  }

  /**
   * Create an OneTime DecisionImportance sampler with the given decision over the given universe
   * using the given number of samples.
   */
  def apply[T, U](myNumSamples: Int, utilityNodes: List[Element[_]], target: Decision[T, U])(implicit universe: Universe) = {
    utilityNodes.foreach(_.generate()) // need initial values for the utility nodes before the usage check
    UsageCheck(utilityNodes, target)
    new DecisionImportance[T, U](universe, utilityNodes, target) with OneTimeProbQuerySampler { val numSamples = myNumSamples }
  }

}










