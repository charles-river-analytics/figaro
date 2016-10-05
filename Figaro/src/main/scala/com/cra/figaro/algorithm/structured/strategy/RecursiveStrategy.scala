/*
 * RecursiveStrategy.scala
 * Strategies that recurse on nested problems.
 *
 * Created By:      William Kretschmer (kretsch@mit.edu)
 * Creation Date:   Sep 28, 2016
 *
 * Copyright 2016 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */
package com.cra.figaro.algorithm.structured.strategy

import com.cra.figaro.algorithm.structured.NestedProblem

/**
 * A strategy that may choose to recursively operate on subproblems.
 */
trait RecursiveStrategy extends ProblemStrategy {
  /**
   * Optionally recurse on a subproblem.
   * @param subproblem Nested problem to consider recursing on.
   * @return A strategy to recurse on the nested problem, or None if it should not recurse further.
   */
  def recurse(subproblem: NestedProblem[_]): Option[RecursiveStrategy]
}
