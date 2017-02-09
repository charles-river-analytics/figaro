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

/**
 * A refining strategy takes an inference problem over a set of factors and improves it to better reflect the underlying
 * model. The exact notion of "improvement" should be specified in subclasses. The most common use cases include
 * generating ranges, expanding additional subproblems, or sampling additional values for elements with infinite ranges.
 *
 * Refining is separate from solving, but can be thought of as the process of deciding how much of a problem we want to
 * solve.
 * @param collection Collection of components to refine.
 * @param rangeSizer Method to determine the size of the range of components.
 */
private[figaro] abstract class RefiningStrategy(collection: ComponentCollection, rangeSizer: RangeSizer) {
  /**
   * Refine in place using this strategy. This will recursively mark as unsolved any problems whose solutions are no
   * longer applicable as a result of refinement. This also marks problem components as fully enumerated or refined
   * where applicable.
   */
  def execute(): Unit

  /**
   * Process a component by generating its range, if it is not already fully enumerated. After calling this method, it
   * may be necessary to check if the component is fully enumerated or refined.
   * @param comp Component to process.
   */
  def generateRange(comp: ProblemComponent[_]): Unit = {
    if(!comp.fullyEnumerated) {
      comp.generateRange(rangeSizer(comp))
    }
  }
}
