/*
 * SolvingStrategy.scala
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
import com.cra.figaro.algorithm.factored.factors.Factor
import com.cra.figaro.algorithm.factored.factors.Variable

/**
 * An abstract class that defines how to solve a problem
 */
abstract class SolvingStrategy {

  /**
   * Solve the given problem with the indicated preserve and eliminate variables, and return a list of factors representing
   * the joint distribution over the preserved variables.
   */
  def solve(problem: Problem, toEliminate: Set[Variable[_]], toPreserve: Set[Variable[_]], factors: List[Factor[Double]]): List[Factor[Double]]
  
}