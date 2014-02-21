/*
 * MultiDecisionVariableElimination.scala
 * Multi-decision Variable Elimination
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
import com.cra.figaro.algorithm.factored._
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.language._
import com.cra.figaro.library.decision._
import scala.collection.mutable.Map

/**
 * A multi-decision algorithm that uses Variable Elimination for each decision.
 */
class MultiDecisionVariableElimination(universe: Universe, utilityNodes: List[Element[_]], targets: Element[_]*)
  extends MultiDecisionAlgorithm(universe, utilityNodes, targets.toList) {

  protected def createAlg[T, U](decisionTarget: Decision[T, U],
    utilities: List[Element[_]], mv: Universe): OneTimeProbQueryDecision[T, U] = {
    utilityNodes.foreach(_.generate()) // need initial values for the utility nodes before the usage check
    DecisionVariableElimination.usageCheck(utilityNodes, decisionTarget)
    val ve = new ProbQueryVariableEliminationDecision[T, U](mv, utilityNodes, decisionTarget)(
      false,
      List(),
      (u: Universe, e: List[NamedEvidence[_]]) => () => ProbEvidenceSampler.computeProbEvidence(10000, e)(u)) with OneTimeProbQueryDecision[T, U]
    ve
  }
}

object MultiDecisionVariableElimination {

  /**
   * Create a OneTime multi-decision Variable Elimination the given target utilities, and target decisions.
   */
  def apply(utilityNodes: List[Element[_]], targets: Element[_]*)(implicit universe: Universe) = {
    new MultiDecisionVariableElimination(universe, utilityNodes, targets: _*)
  }

}


