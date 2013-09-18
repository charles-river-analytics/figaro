/*
 * MultiDecisionMetropolisHastings.scala
 * Multi-decision MH
 * 
 * Created By:      Brian Ruttenberg (bruttenberg@cra.com)
 * Creation Date:   Oct 1, 2012
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 */

package com.cra.figaro.algorithm.decision

import com.cra.figaro.algorithm._
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.algorithm.decision.MultiDecisionMetropolisHastings._
import com.cra.figaro.language._
import com.cra.figaro.library.decision._
import scala.collection.mutable.Map

/**
 * A OneTime multi-decision algorithm that uses Metropolis-Hastings sampling for each decision. A user
 * must supple an instance of a ProposalMakerType, which indicates how to create a proposal scheme
 * for each decision.
 */
class OneTimeMultiDecisionMetropolisHastings(universe: Universe, myNumSamples: Int, ProposalMaker: ProposalMakerType,
  burnIn: Int, interval: Int, utilityNodes: List[Element[_]], targets: Decision[_, _]*)
  extends MultiDecisionAlgorithm(universe, utilityNodes, targets.toList) {

  protected def createAlg[T, U](decisionTarget: Decision[T, U],
    utilities: List[Element[_]], mv: Universe): OneTimeProbQueryDecision[T, U] = {
    new OneTimeDecisionMetropolisHastings[T, U](mv, myNumSamples, ProposalMaker(mv, decisionTarget), burnIn,
      interval, utilities, decisionTarget) with OneTimeProbQueryDecision[T, U]
  }
}

object MultiDecisionMetropolisHastings {

  /**
   * Type that defines how to create a proposal in a multi-decision MH algorithm. This is simply a function
   * from a universe and element (a decision) to a proposal scheme.
   */
  type ProposalMakerType = (Universe, Element[_]) => ProposalScheme

  /**
   * Create a OneTime multi-decision Metropolis-Hastings sampler using the given number of samples, proposal
   * maker, given target utilities, and target decisions. 
   * 
   * @param proposalMaker A function that specifies how to create a proposal for each decision in the target list
   */
  def apply(numSamples: Int, proposalMaker: ProposalMakerType, utilityNodes: List[Element[_]], targets: Decision[_, _]*)(implicit universe: Universe) = {
    new OneTimeMultiDecisionMetropolisHastings(universe, numSamples, proposalMaker, 0, 1, utilityNodes, targets: _*)
  }

  /**
   * Create a OneTime multi-decision Metropolis-Hastings sampler using the given number of samples, proposal
   * maker, burn in time, given target utilities, and target decisions. 
   * 
   * @param proposalMaker A function that specifies how to create a proposal for each decision in the target list
   */
  def apply(numSamples: Int, proposalMaker: ProposalMakerType, burnIn: Int,
    utilityNodes: List[Element[_]], targets: Decision[_, _]*)(implicit universe: Universe) = {
    new OneTimeMultiDecisionMetropolisHastings(universe, numSamples, proposalMaker, burnIn, 1, utilityNodes, targets: _*)
  }

  /**
   * Create a OneTime multi-decision Metropolis-Hastings sampler using the given number of samples, proposal
   * maker, burn in time, interval, given target utilities, and target decisions. 
   * 
   * @param proposalMaker A function that specifies how to create a proposal for each decision in the target list
   */
  def apply(numSamples: Int, proposalMaker: ProposalMakerType, burnIn: Int, interval: Int, 
      utilityNodes: List[Element[_]], targets: Decision[_, _]*)(implicit universe: Universe) = {
    new OneTimeMultiDecisionMetropolisHastings(universe, numSamples, proposalMaker, burnIn, interval: Int, utilityNodes, targets: _*)
  }

}


