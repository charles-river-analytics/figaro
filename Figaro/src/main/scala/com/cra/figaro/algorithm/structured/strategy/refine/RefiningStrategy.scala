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

import com.cra.figaro.algorithm.structured._
import com.cra.figaro.algorithm.structured.strategy.ProblemStrategy

/**
 * A refining strategy takes an inference problem over a set of factors and improves it to better reflect the underlying
 * model. The exact notion of "improvement" should be specified in subclasses. The most common use cases include
 * generating ranges and factors, expanding additional subproblems, or sampling additional values for elements with
 * infinite ranges.
 *
 * Refining is separate from solving, but can be thought of as the process of deciding how much of a problem we want to
 * solve.
 * @param problem Problem to refine.
 * @param rangeSizer Method to determine the size of the range of components.
 * @param parameterized Indicates whether or not to make parameterized factors.
 */
private[figaro] abstract class RefiningStrategy(problem: Problem, rangeSizer: RangeSizer, parameterized: Boolean)
  extends ProblemStrategy(problem) {
  /**
   * Refine the problem in place using this strategy. This will recursively mark as unsolved any problems whose
   * solutions are no longer applicable as a result of refinement. This also marks problem components as fully
   * enumerated or refined where applicable.
   */
  def execute(): Unit

  /**
   * Process a component by generating its range and factors. This includes non-constraint factors, as well as both
   * lower and upper bound constraint factors. After calling this method, it may be necessary to check if the component
   * is fully enumerated or refined.
   * @param comp Component to process.
   */
  protected def makeRangeAndFactors(comp: ProblemComponent[_]) {
    // No need to regenerate the range for fully enumerated components
    if(!comp.fullyEnumerated) comp.generateRange(rangeSizer(comp))
    // Still need to make factors because the ranges of variables in the factors could have changed
    comp.makeNonConstraintFactors(parameterized)
    comp.makeConstraintFactors(Lower)
    comp.makeConstraintFactors(Upper)
  }
}
