/*
 * package.scala
 * Definitions of raising criteria.
 *
 * Created By:      William Kretschmer (kretsch@mit.edu)
 * Creation Date:   Oct 13, 2016
 *
 * Copyright 2016 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */
package com.cra.figaro.algorithm.structured.strategy

import com.cra.figaro.algorithm.structured.Problem

package object solve {
  /**
   * A raising criteria is a decision function that chooses whether or not to eliminate variables at the level of this
   * problem, or to raise factors to the next higher problem. Returns true if the factors should be raised, or false if
   * a solver should perform elimination.
   */
  type RaisingCriteria = Problem => Boolean

  /**
   * Raises a problem if any of its components are global.
   */
  def raiseIfGlobal(problem: Problem): Boolean = {
    // Raise if there exists a component that is neither internal nor a target
    problem.components.exists(pc => {
      val a = problem.internal(pc.variable)
      val b = problem.targets.contains(pc.element)
      !a && !b
    })
  }

  /**
   * Always raises a problem, unless it is the specified top-level problem. This has the effect of "flattening" because
   * all factors of problems strictly contained in the top-level problem are raised.
   * @param topLevel The problem that should not be raised. This function raises all other problems.
   */
  def flatRaising(topLevel: Problem)(problem: Problem): Boolean = {
    problem != topLevel
  }

  /**
   * Never raises a problem without solving it.
   */
  def structuredRaising(problem: Problem): Boolean = false
}
