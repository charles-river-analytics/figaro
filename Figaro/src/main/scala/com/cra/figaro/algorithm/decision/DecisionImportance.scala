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
  dummyTarget: Element[_]) extends Importance(universe, dummyTarget) with DecisionAlgorithm[T, U] {

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
    universe.clearTemporaries()
    totalWeight = logSum(s._1, totalWeight)
    allWeightsSeen foreach (updateWeightSeenForTarget(s, _))
    allUtilitiesSeen foreach (updateWeightSeenForTargetNoLog((math.exp(s._1) * utilitySum, s._2), _))
  }

}

object DecisionImportance {

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










