/*
 * RefiningStrategy.scala
 * Base class for strategies that refine problems.
 *
 * Created By:      William Kretschmer (kretsch@mit.edu)
 * Creation Date:   Sep 16, 2016
 *
 * Copyright 2016 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */
package com.cra.figaro.algorithm.structured.strategy.refine

import com.cra.figaro.algorithm.structured.Problem

/**
 * A refining strategy takes an inference problem over a set of factors and improves it to better reflect the underlying
 * model. The exact notion of "improvement" should be specified in subclasses. The most common use cases include
 * expanding additional subproblems, or sampling additional values for elements with infinite ranges.
 *
 * Refining is separate from solving, but can be thought of as the process of deciding how much of a problem we want to
 * solve.
 */
private[figaro] abstract class RefiningStrategy(val problem: Problem) {
  /**
   * Refine the problem in place using this strategy. This will mark the problem as unsolved if any changes are made to
   * the problem or nested subproblems, since any previous solution may no longer be applicable. This also marks
   * problem components as fully expanded where applicable.
   */
  def execute(): Unit
}
