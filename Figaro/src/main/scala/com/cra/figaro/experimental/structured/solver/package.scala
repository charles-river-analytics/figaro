package com.cra.figaro.experimental.structured

import com.cra.figaro.algorithm.factored.factors.Factor
import com.cra.figaro.algorithm.factored.factors.Variable

package object solver {
  /**
   * A Solver takes a set of variables to eliminate, a set of variables to preserve, and a list of factors.
   * It returns a list of factors that mention only the preserved variables.
   */
  type Solver = (Problem, Set[Variable[_]], Set[Variable[_]], List[Factor[Double]]) => List[Factor[Double]]

  def variableElimination(problem: Problem, toEliminate: Set[Variable[_]], toPreserve: Set[Variable[_]], factors: List[Factor[Double]]): List[Factor[Double]] = {
    val ve = new VESolver(problem, toEliminate, toPreserve, factors)
    ve.go()
  }

  def beliefPropagation(iterations: Int = 100)(problem: Problem, toEliminate: Set[Variable[_]], toPreserve: Set[Variable[_]], factors: List[Factor[Double]]): List[Factor[Double]] = {
    val ve = new BPSolver(problem, toEliminate, toPreserve, factors, iterations)
    ve.go()
  }

}
