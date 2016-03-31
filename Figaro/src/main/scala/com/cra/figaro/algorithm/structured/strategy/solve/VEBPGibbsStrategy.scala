/*
 * VEBPGibbsStrategy.scala
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

import com.cra.figaro.algorithm.structured.Problem
import com.cra.figaro.algorithm.factored.factors.Factor
import com.cra.figaro.algorithm.factored.factors.Variable
import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.algorithm.factored.gibbs.Gibbs
import com.cra.figaro.language._
import com.cra.figaro.algorithm.structured.solver

/**
 * A solving strategy that chooses between VE, BP,  and Gibbs based on a score of the elminiation order and determinism
 */
class VEBPGibbsStrategy(val scoreThreshold: Double, val determThreshold: Double, val bpIters: Int, val numSamples: Int, val burnIn: Int, val interval: Int, val blockToSampler: Gibbs.BlockSamplerCreator) extends SolvingStrategy {

  def solve(problem: Problem, toEliminate: Set[Variable[_]], toPreserve: Set[Variable[_]], factors: List[Factor[Double]]): (List[Factor[Double]], Map[Variable[_], Factor[_]]) = {
    val (score, order) = VariableElimination.eliminationOrder(factors, toPreserve)
    if (score <= scoreThreshold) {
      solver.marginalVariableElimination(problem, toEliminate, toPreserve, factors)
    } else {
      /*
       *  Choose between BP and Gibbs.
       *  
       *  Look at the variables in the problem, and find the percent that are deterministic. We label a variable as deterministic if it has any variables that
       *  it must be blocked with for Gibbs sampling to converge properly.
       *  
       *  If the percent of variables in a model is deterministic, it is better to run BP than Gibbs.
       */
      val numDeterministicVars = toEliminate.toList.map(hasDeterminism(problem, _)).count(b => b) + toPreserve.toList.map(hasDeterminism(problem, _)).count(b => b)
      val totalVars = toEliminate.size + toPreserve.size

      if ((numDeterministicVars.toDouble / totalVars) > determThreshold) {        
        solver.marginalBeliefPropagation(bpIters)(problem, toEliminate, toPreserve, factors)
      } else {
        solver.marginalGibbs(numSamples, burnIn, interval, blockToSampler)(problem, toEliminate, toPreserve, factors)
      }
    }
  }

  def hasDeterminism(problem: Problem, v: Variable[_]): Boolean = problem.collection.variableParents(v).nonEmpty
  
}