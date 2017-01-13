/*
 * MarginalMAPVEStrategy.scala
 * A class that solves a marginal MAP problem using VE.
 *
 * Created By:      William Kretschmer (kretsch@mit.edu)
 * Creation Date:   July 1, 2015
 *
 * Copyright 2015 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */
package com.cra.figaro.experimental.marginalmap

import com.cra.figaro.algorithm.factored.factors.{Factor, Variable}
import com.cra.figaro.algorithm.structured.{NestedProblem, Problem, solver}
import com.cra.figaro.algorithm.structured.strategy.solve.SolvingStrategy

/**
 * A solving strategy that uses MPE VE to solve non-nested problems, and performs the MAP step at the top level.
 * It is assumed that at the top level, "toPreserve" elements are the MAP elements.
 */
class MarginalMAPVEStrategy extends SolvingStrategy {

  def solve(problem: Problem, toEliminate: Set[Variable[_]], toPreserve: Set[Variable[_]], factors: List[Factor[Double]]):
    (List[Factor[Double]], Map[Variable[_], Factor[_]]) = {
    problem match {
      case _: NestedProblem[_] => {
        // A problem needed for the initial step of summing out the non-MAP variables
        // Use marginal VE for this
        solver.marginalVariableElimination(problem, toEliminate, toPreserve, factors)
      }
      case _ => {
        // Sum over the remaining non-MAP variables (i.e. toEliminate), and MAP the rest (i.e. toPreserve)
        // marginalizedFactors is a set of factors over just the MAP variables
        val (marginalizedFactors, _) = solver.marginalVariableElimination(problem, toEliminate, toPreserve, factors)
        // Now that we have eliminated the sum variables, we effectively just do MPE over the remaining variables
        // For MPE, we eliminate all remaining variables (i.e. toPreserve), and preserve no variables (i.e. Set())
        solver.mpeVariableElimination(problem, toPreserve, Set(), marginalizedFactors)
      }
    }
  }
  
}