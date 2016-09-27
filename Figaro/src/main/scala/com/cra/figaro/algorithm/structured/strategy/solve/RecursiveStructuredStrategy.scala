/*
 * RecursiveStructuredStrategy.scala
 * Solving strategies that recursively solve in a structured manner.
 *
 * Created By:      William Kretschmer (kretsch@mit.edu)
 * Creation Date:   Sep 27, 2016
 *
 * Copyright 2016 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */
package com.cra.figaro.algorithm.structured.strategy.solve

import com.cra.figaro.algorithm.structured.{Bounds, NestedProblem}

/**
 * A strategy that always solves subproblems before raising their solutions, and applies the same strategy to solve all
 * subproblems.
 */
trait RecursiveStructuredStrategy extends RaisingStrategy {
  // Never raise a subproblem without solving it.
  override def raisingCriteria(subproblem: NestedProblem[_]): Boolean = false

  // Call the default solve method.
  override def solveNested(subproblem: NestedProblem[_], bounds: Bounds): Unit = solve(subproblem, bounds)
}
