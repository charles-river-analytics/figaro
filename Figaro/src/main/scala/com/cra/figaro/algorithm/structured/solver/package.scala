/*
 * package.scala
 * Definitions of solvers.
 *
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   March 1, 2015
 *
 * Copyright 2015 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.algorithm.structured

import com.cra.figaro.algorithm.factored.factors.Factor
import com.cra.figaro.algorithm.factored.factors.Variable
import com.cra.figaro.algorithm.factored.gibbs.Gibbs
import com.cra.figaro.algorithm.structured.solver.BPSolver
import com.cra.figaro.algorithm.structured.solver.GibbsSolver
import com.cra.figaro.algorithm.structured.solver.VESolver
import com.cra.figaro.algorithm.factored.factors.SumProductSemiring
import com.cra.figaro.algorithm.factored.factors.MaxProductSemiring

package object solver {
  /**
   * A Solver takes a set of variables to eliminate, a set of variables to preserve, and a list of factors.
   * It returns a list of factors that mention only the preserved variables.
   */
  type Solver = (Problem, Set[Variable[_]], Set[Variable[_]], List[Factor[Double]]) => (List[Factor[Double]], Map[Variable[_], Factor[_]])

  /**
   * Creates a Gibbs sampling solver.
   * @param numSamples number of samples to take
   * @param burnIn number of burn-in samples to throw away
   * @param interval number of samples to throw away between recorded samples
   * @param blockToSampler function for creating Gibbs block samplers
   * @param problem the problem to solve
   * @param toEliminate the variables to be eliminated
   * @param toPreserve the variables to be preserved (not eliminated)
   * @param factors all the factors in the problem
   */
  def marginalGibbs(numSamples: Int, burnIn: Int, interval: Int, blockToSampler: Gibbs.BlockSamplerCreator)(problem: Problem, toEliminate: Set[Variable[_]], toPreserve: Set[Variable[_]], factors: List[Factor[Double]]): (List[Factor[Double]], Map[Variable[_], Factor[_]]) = {
    val gibbs = new GibbsSolver(problem, toEliminate, toPreserve, factors, numSamples, burnIn, interval, blockToSampler)
    (gibbs.go(), Map())
  }

  /**
   * Creates a variable elimination solver.
   * @param problem the problem to solve
   * @param toEliminate the variables to be eliminated
   * @param toPreserve the variables to be preserved (not eliminated)
   * @param factors all the factors in the problem
   */
  def marginalVariableElimination(problem: Problem, toEliminate: Set[Variable[_]], toPreserve: Set[Variable[_]], factors: List[Factor[Double]]): (List[Factor[Double]], Map[Variable[_], Factor[_]]) = {
    val ve = new VESolver(problem, toEliminate, toPreserve, factors, SumProductSemiring())
    ve.go()
  }

  /**
   * Creates an MPE variable elimination solver.
   * @param problem the problem to solve
   * @param toEliminate the variables to be eliminated
   * @param toPreserve the variables to be preserved (not eliminated)
   * @param factors all the factors in the problem
   */
  def mpeVariableElimination(problem: Problem, toEliminate: Set[Variable[_]], toPreserve: Set[Variable[_]], factors: List[Factor[Double]]): (List[Factor[Double]], Map[Variable[_], Factor[_]]) = {
    val ve = new VESolver(problem, toEliminate, toPreserve, factors, MaxProductSemiring())
    ve.go()
  }

  /**
   * Creates a belief propagation solver.
   * @param iterations number of iterations of BP to run
   * @param problem the problem to solve
   * @param toEliminate the variables to be eliminated
   * @param toPreserve the variables to be preserved (not eliminated)
   * @param factors all the factors in the problem
   */
  def marginalBeliefPropagation(iterations: Int = 100)(problem: Problem, toEliminate: Set[Variable[_]],
    toPreserve: Set[Variable[_]], factors: List[Factor[Double]]): (List[Factor[Double]], Map[Variable[_], Factor[_]]) = {
    val bp = new BPSolver(problem, toEliminate, toPreserve, factors, iterations, SumProductSemiring())
    bp.go()
  }
  
    /**
   * Creates an MPE belief propagation solver.
   * @param iterations number of iterations of BP to run
   * @param problem the problem to solve
   * @param toEliminate the variables to be eliminated
   * @param toPreserve the variables to be preserved (not eliminated)
   * @param factors all the factors in the problem
   */
  def mpeBeliefPropagation(iterations: Int = 100)(problem: Problem, toEliminate: Set[Variable[_]],
    toPreserve: Set[Variable[_]], factors: List[Factor[Double]]): (List[Factor[Double]], Map[Variable[_], Factor[_]]) = {
    val bp = new BPSolver(problem, toEliminate, toPreserve, factors, iterations, MaxProductSemiring())
    bp.go()
  }

}
