/*
 * MultiDecisionImportance.scala
 * Multi-decisions that use Importance Sampling
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
import com.cra.figaro.library.decision._
import scala.collection.mutable.Map

/**
 * A OneTime multi-decision algorithm that uses Importance sampling for each decision.
 */
class OneTimeMultiDecisionImportance(universe: Universe, myNumSamples: Int, utilityNodes: List[Element[_]], targets: Decision[_, _]*)
  extends MultiDecisionAlgorithm(universe, utilityNodes, targets.toList) {

  protected def createAlg[T, U](decisionTarget: Decision[T, U],
    utilities: List[Element[_]], mv: Universe): OneTimeProbQueryDecision[T, U] = {
    new DecisionImportance[T, U](mv, utilities, decisionTarget) with OneTimeProbQueryDecision[T, U] with OneTimeProbQuerySampler { val numSamples = myNumSamples }
  }

}

object MultiDecisionImportance {

  /**
   * Create a OneTime multi-decision Importance sampler using the given number of samples, 
   * the given target utilities, and target decisions.
   */
  def apply(numSamples: Int, utilityNodes: List[Element[_]], targets: Decision[_, _]*)(implicit universe: Universe) = {
    new OneTimeMultiDecisionImportance(universe, numSamples, utilityNodes, targets: _*)
  }

}


