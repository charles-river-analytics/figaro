/*
 * ProblemStrategy.scala
 * Classes that operate on problems.
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

import com.cra.figaro.algorithm.structured.Problem

/**
 * A strategy operates on a problem and does something useful from the perspective of performing inference.
 */
private[figaro] abstract class ProblemStrategy(val problem: Problem) {
  /**
   * Perform a one time execution of the strategy.
   */
  def execute(): Unit
}
