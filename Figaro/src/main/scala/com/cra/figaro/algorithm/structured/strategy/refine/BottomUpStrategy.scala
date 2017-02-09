/*
 * BottomUpStrategy.scala
 * Strategies that decompose in a bottom-up manner.
 *
 * Created By:      William Kretschmer (kretsch@mit.edu)
 * Creation Date:   Nov 28, 2016
 *
 * Copyright 2016 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */
package com.cra.figaro.algorithm.structured.strategy.refine

import com.cra.figaro.algorithm.structured._
import com.cra.figaro.language._

import scala.collection.mutable

/**
 * Refining strategies that start with a list of components belonging to a single problem, working bottom-up to
 * decompose the entire model. The primary purpose of this strategy is as a tool to expand the model in the component
 * collection by adding needed elements to the collection and recursing on subproblems.
 *
 * This will fail on infinitely recursive models.
 * @param problem Problem to refine.
 * @param rangeSizer Method to determine the size of the range of components.
 * @param initialComponents List of components belonging to the problem from which to begin bottom-up decomposition.
 * @param done Problem components that were already processed, which should not be visited again. This is explicitly a
 * mutable set so that nested decomposition strategies can update any enclosing decomposition strategy with the
 * components that were processed. Defaults to the empty set.
 */
class BottomUpStrategy(problem: Problem, rangeSizer: RangeSizer,
                       override val initialComponents: List[ProblemComponent[_]],
                       done: mutable.Set[ProblemComponent[_]] = mutable.Set())
  extends DecompositionStrategy(problem.collection, rangeSizer, done) {

  // Always recurse normally; this could overflow on infinite models
  override def recurse(nestedProblem: NestedProblem[_]): Option[DecompositionStrategy] = {
    Some(new BottomUpStrategy(nestedProblem, rangeSizer, nestedProblem.targetComponents, done))
  }

  // Get the component from the collection, or add it if it does not exist
  override protected def checkArg[T](element: Element[T]): ProblemComponent[T] = {
    if(problem.collection.contains(element)) problem.collection(element)
    else problem.add(element)
  }

  // Refine any component not fully refined
  override protected def shouldRefine(comp: ProblemComponent[_]): Boolean = !comp.fullyRefined
}

/**
 * Like `BottomUpStrategy`, but only partially decomposes a model. This strategy is partial in the sense that it only
 * recurses to some given maximum depth. This makes the strategy applicable for infinitely recursive models.
 * @param depth Maximum recursion depth allowed; must be nonnegative.
 * @param problem Problem to refine.
 * @param rangeSizer Method to determine the size of the range of components.
 * @param initialComponents List of components belonging to the problem from which to begin bottom-up decomposition.
 * @param done Problem components that were already processed, which should not be visited again. This is explicitly a
 * mutable set so that nested decomposition strategies can update any enclosing decomposition strategy with the
 * components that were processed. Defaults to the empty set.
 */
class PartialBottomUpStrategy(depth: Int, problem: Problem, rangeSizer: RangeSizer,
                              initialComponents: List[ProblemComponent[_]],
                              done: mutable.Set[ProblemComponent[_]] = mutable.Set())
  extends BottomUpStrategy(problem, rangeSizer, initialComponents, done) {

  // Only recurse if we haven't reached the maximum recursion depth yet
  override def recurse(nestedProblem: NestedProblem[_]): Option[DecompositionStrategy] = {
    if(depth > 0) {
      Some(new PartialBottomUpStrategy(depth - 1, nestedProblem, rangeSizer, nestedProblem.targetComponents, done))
    }
    else None
  }
}
