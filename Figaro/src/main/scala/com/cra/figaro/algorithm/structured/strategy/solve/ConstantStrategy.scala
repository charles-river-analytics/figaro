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
import com.cra.figaro.algorithm.structured.solver.Solver
import com.cra.figaro.algorithm.structured._

/**
 * A solving strategy that applies the same solver and raising criteria to every problem, including nested problems.
 * @param problem Problem to solve.
 * @param raisingCriteria Function used to decide whether to raise a problem or solve it first and raise its solution.
 * @param solverToUse Solver to use for elimination.
 */
class ConstantStrategy(problem: Problem, raisingCriteria: RaisingCriteria, solverToUse: Solver)
  extends RaisingStrategy(problem, raisingCriteria) {

  override def decideToRaise() = raisingCriteria(problem)

  override def eliminate(toEliminate: Set[Variable[_]], toPreserve: Set[Variable[_]], factors: List[Factor[Double]]) = {
    solverToUse(problem, toEliminate, toPreserve, factors)
  }

  override def recurse(subproblem: NestedProblem[_]) = {
    if(subproblem.solved) None
    else Some(new ConstantStrategy(subproblem, raisingCriteria, solverToUse))
  }
}
