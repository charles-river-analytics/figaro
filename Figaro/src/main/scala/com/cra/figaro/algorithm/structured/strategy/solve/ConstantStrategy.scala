/*
 * ConstantStrategy.scala
 * Solving strategies that use a single solver.
 *
 * Created By:      Brian Ruttenberg (bruttenberg@cra.com)
 * Creation Date:   July 1, 2015
 *
 * Copyright 2015 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */
package com.cra.figaro.algorithm.structured.strategy.solve

import com.cra.figaro.algorithm.factored.factors.{Factor, Variable}
import com.cra.figaro.algorithm.structured.{NestedProblem, Problem, solver}


/**
 * A solving strategy that applies the same solver to every problem, including nested problems.
 * @param problem Problem to solve.
 * @param solverToUse Solver to use for elimination.
 * @param raisingCriteria Function used to decide whether to raise a problem or solve it first and raise its solution.
 * Defaults to a structured raising criteria, which never raises a subproblem without solving it first.
 */
class ConstantStrategy(problem: Problem, solverToUse: solver.Solver,
                       raisingCriteria: (Problem, NestedProblem[_]) => Boolean = ConstantStrategy.structured)
  extends SolvingStrategy(problem)
  with RaisingStrategy {

  override def eliminate(toEliminate: Set[Variable[_]], toPreserve: Set[Variable[_]], factors: List[Factor[Double]]):
    (List[Factor[Double]], Map[Variable[_], Factor[_]]) = {
    solverToUse(problem, toEliminate, toPreserve, factors)
  }

  override def recurse(subproblem: NestedProblem[_]) = {
    if(raisingCriteria(problem, subproblem)) None
    else Some(new ConstantStrategy(problem, solverToUse))
  }
}

object ConstantStrategy {

  /**
   * Decision function for constant raising strategies that raises a subproblem if any of its components are global.
   */
  def raiseIfGlobal(parent: Problem, subproblem: NestedProblem[_]): Boolean = {
    subproblem.components.exists(pc => {
      val a = subproblem.internal(pc.variable)
      val b = subproblem.targets.contains(pc.element)
      !a && !b
    })
  }

  /**
   * Decision function for constant raising strategies that always raises a subproblem.
   */
  def flatten(parent: Problem, subproblem: NestedProblem[_]): Boolean = true

  /**
   * Decision function for constant raising strategies that never raises a subproblem without solving it.
   */
  def structured(parent: Problem, subproblem: NestedProblem[_]): Boolean = false

}
