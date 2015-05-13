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
  extends MetropolisHastings(universe, proposalScheme, burnIn, interval, dummyTarget) with DecisionAlgorithm[T, U] {

  def this(universe: Universe, proposalScheme: ProposalScheme, burnIn: Int, interval: Int,
    utilityNodes: List[Element[_]], decisionTarget: Decision[T, U]) = this(universe, proposalScheme,
    burnIn, interval, utilityNodes, decisionTarget, createDecisionDummy(decisionTarget))

  import MetropolisHastings._


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

  protected def updateWeightSeenWithValue[T](value: T, weight: Double, weightSeen: WeightSeen[T]): Unit =
    weightSeen._2 += value -> (weightSeen._2.getOrElse(value, 0.0) + weight)

  protected def updateWeightSeenForTarget[T](sample: (Double, Map[Element[_], Any]), weightSeen: WeightSeen[T]): Unit = {
    val (weight, values) = sample
    val value = values(weightSeen._1).asInstanceOf[T]
    updateWeightSeenWithValue(value, weight, weightSeen)
  }

  /**
   * Produce a single sample. In decision MH, we always update the target (parent and decision) since the utilities mights have changed
   */
  override def sample(): (Boolean, Sample) = {
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
    if (sampleCount == 0) {
      initUpdates
    }
    val s = sample()
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

}

/**
 * Anytime Decision Metropolis-Hastings sampler.
 */
class AnytimeDecisionMetropolisHastings[T, U](universe: Universe,
  scheme: ProposalScheme, burnIn: Int, interval: Int, utilityNodes: List[Element[_]],
  decisionTarget: Decision[T, U])
  extends DecisionMetropolisHastings(universe, scheme, burnIn, interval, utilityNodes, decisionTarget)
  with UnweightedSampler with AnytimeProbQuerySampler {

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
  with UnweightedSampler with OneTimeProbQuerySampler {
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
