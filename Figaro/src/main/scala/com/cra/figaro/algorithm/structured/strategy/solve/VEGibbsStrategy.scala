/*
 * VEGibbsStrategy.scala
 * Strategy that chooses to solve a problem with either VE or Gibbs.
 *
 * Created By:      William Kretschmer (kretsch@mit.edu)
 * Creation Date:   August 11, 2015
 *
 * Copyright 2015 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */
package com.cra.figaro.algorithm.structured.strategy.solve

import com.cra.figaro.algorithm.structured.{NestedProblem, Problem, solver}
import com.cra.figaro.algorithm.factored.factors.Factor
import com.cra.figaro.algorithm.factored.factors.Variable
import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.algorithm.factored.gibbs.Gibbs

/**
 * A solving strategy that chooses between VE and Gibbs based on a score of the elminiation order
 */
class VEGibbsStrategy(problem: Problem, raisingCriteria: RaisingCriteria, val scoreThreshold: Double,
                      val numSamples: Int, val burnIn: Int, val interval: Int,  val blockToSampler: Gibbs.BlockSamplerCreator)
  extends RaisingStrategy(problem, raisingCriteria) {

  override def eliminate(toEliminate: Set[Variable[_]], toPreserve: Set[Variable[_]], factors: List[Factor[Double]]): (List[Factor[Double]], Map[Variable[_], Factor[_]]) = {
    val (score, order) = VariableElimination.eliminationOrder(factors, toPreserve)
    if (score > scoreThreshold) {
      solver.marginalGibbs(numSamples, burnIn, interval, blockToSampler)(problem, toEliminate, toPreserve, factors)
    } else {
      solver.marginalVariableElimination(problem, toEliminate, toPreserve, factors)
    }
  }

  override def recurse(subproblem: NestedProblem[_]) = {
    if(subproblem.solved) None
    else Some(new VEGibbsStrategy(subproblem, raisingCriteria, scoreThreshold, numSamples, burnIn, interval, blockToSampler))
  }

}