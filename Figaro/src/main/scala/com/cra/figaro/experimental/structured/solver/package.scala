package com.cra.figaro.experimental.structured

import com.cra.figaro.algorithm.factored.factors.Factor
import com.cra.figaro.algorithm.factored.factors.Variable

package object solver {
  /**
   * A Solver takes a list of variables to eliminate and a list of factors.
   * It returns a list of non-eliminated variables and factors that mention only those variables.
   */
  type Solver = (List[Variable[_]], List[Factor[Double]]) => (List[Variable[_]], List[Factor[Double]])

  def variableElimination(toEliminate: List[Variable[_]], factors: List[Factor[Double]]): (List[Variable[_]], List[Factor[Double]]) = {
    val ve = new VESolver(toEliminate, factors)
    ve.go()
    (ve.globals, ve.result)
  }
}
