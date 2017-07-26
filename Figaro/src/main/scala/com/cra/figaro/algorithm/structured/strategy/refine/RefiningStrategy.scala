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
import com.cra.figaro.util

/**
 * A refining strategy takes an inference problem over a set of factors and improves it to better reflect the underlying
 * model. The exact notion of "improvement" should be specified in subclasses. The most common use cases include
 * generating ranges, expanding additional subproblems, or sampling additional values for elements with infinite ranges.
 *
 * Refining is separate from solving, but can be thought of as the process of deciding how much of a problem we want to
 * solve.
 * @param collection Collection of components to refine.
 */
abstract class RefiningStrategy(val collection: ComponentCollection) {
  /**
   * Refine in place using this strategy. This will recursively mark as unsolved any problems whose solutions are no
   * longer applicable as a result of refinement. This also marks problem components as fully enumerated or refined
   * where applicable.
   */
  def execute(): Unit

  /**
   * Recursively marks as unsolved any problem whose solution could have changed as a result of refinement by this
   * strategy or any of its recursively generated strategies.
   */
  protected def markProblemsUnsolved(problems: Set[Problem]): Unit = {
    // From a subproblem, we must include the problems that use it
    def problemGraph(pr: Problem): Set[Problem] = pr match {
      case npr: NestedProblem[_] => collection.expandableComponents(npr).map(_.problem)
      case _ => Set()
    }
    // We have to work our way up the whole problem graph marking problems as unsolved; reachable does this efficiently
    val allUnsolvedProblems = util.reachable(problemGraph, true, problems.toSeq:_*)
    // Mark each reachable problem as unsolved
    for(pr <- allUnsolvedProblems) {
      pr.solved = false
      pr.solution = Nil
    }
  }

  /**
   * Process a component by generating its range, if it is not already fully enumerated. After calling this method, it
   * may be necessary to check if the component is fully enumerated or refined.
   * @param comp Component to process.
   */
  def generateRange(comp: ProblemComponent[_]): Unit = {
    if(!comp.fullyEnumerated) {
      comp.generateRange()
    }
  }
}
