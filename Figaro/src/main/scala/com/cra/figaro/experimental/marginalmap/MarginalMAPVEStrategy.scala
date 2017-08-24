/*
 * MarginalMAPVEStrategy.scala
 * A class that solves a marginal MAP problem using VE.
 *
 * Created By:      William Kretschmer (kretsch@mit.edu)
 * Creation Date:   July 1, 2015
 *
 * Copyright 2017 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */
package com.cra.figaro.experimental.marginalmap

import com.cra.figaro.algorithm.factored.factors.{Factor, Variable}
import com.cra.figaro.algorithm.structured.strategy.solve._
import com.cra.figaro.algorithm.structured.{NestedProblem, Problem, solver}

/**
 * A solving strategy that uses MPE VE to solve non-nested problems, and performs the MAP step at the top level.
 * It is assumed that at the top level, "toPreserve" variables are the MAP variables.
 */
class MarginalMAPVEStrategy(problem: Problem, raisingCriteria: RaisingCriteria)
  extends RaisingStrategy(problem, raisingCriteria) {

  def eliminate(toEliminate: Set[Variable[_]], toPreserve: Set[Variable[_]], factors: List[Factor[Double]]):
    (List[Factor[Double]], Map[Variable[_], Factor[_]]) = {
    // Sum over the remaining non-MAP variables (i.e. toEliminate), and MAP the rest (i.e. toPreserve)
    // marginalizedFactors is a set of factors over just the MAP variables
    val (marginalizedFactors, _) = solver.marginalVariableElimination(problem, toEliminate, toPreserve, factors)
    // Now that we have eliminated the sum variables, we effectively just do MPE over the remaining variables
    // For MPE, we eliminate all remaining variables (i.e. toPreserve), and preserve no variables (i.e. Set())
    solver.mpeVariableElimination(problem, toPreserve, Set(), marginalizedFactors)
  }

  override def recurse(subproblem: NestedProblem[_]) = {
    // A problem needed for the initial step of summing out the non-MAP variables; use marginal VE for this
    new ConstantStrategy(subproblem, raisingCriteria, solver.marginalVariableElimination)
  }
  
}