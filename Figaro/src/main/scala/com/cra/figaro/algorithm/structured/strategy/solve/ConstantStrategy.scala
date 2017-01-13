/*
 * ConstantStrategy.scala
 * An abstract class that defines how to solve a problem
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

import com.cra.figaro.algorithm.structured.Problem
import com.cra.figaro.algorithm.structured.solver
import com.cra.figaro.algorithm.factored.factors.Factor
import com.cra.figaro.algorithm.factored.factors.Variable

/**
 * A solving strategy that applies the same solver to every problem
 */
class ConstantStrategy(solverToUse: solver.Solver) extends SolvingStrategy {

  def solve(problem: Problem, toEliminate: Set[Variable[_]], toPreserve: Set[Variable[_]], factors: List[Factor[Double]]): (List[Factor[Double]], Map[Variable[_], Factor[_]]) = {
    solverToUse(problem, toEliminate, toPreserve, factors)
  }
  
}