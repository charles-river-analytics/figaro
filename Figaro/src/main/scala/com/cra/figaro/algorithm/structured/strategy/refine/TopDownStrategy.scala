/*
 * TopDownStrategy.scala
 * Strategies that decompose in a top-down manner.
 *
 * Created By:      William Kretschmer (kretsch@mit.edu)
 * Creation Date:   Nov 29, 2016
 *
 * Copyright 2016 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */
package com.cra.figaro.algorithm.structured.strategy.refine

import com.cra.figaro.algorithm.structured._
import com.cra.figaro.language._
import com.cra.figaro.util

import scala.collection.mutable

/**
 * Strategies that refine a set of "top-level" components, then update all indirect children of those components to have
 * consistent ranges and factors. The primary purpose of this strategy is as a ranging tool.
 *
 * This only updates children that are already in the problem's component collection. New elements will not be added to
 * the collection, unless they belong to newly expanded subproblems, in which case they will be added without being
 * refined. Another way of saying this is that this strategy will not recursively refine subproblems. This strategy
 * makes the assumption that the collection contains all args of any component that the strategy refines.
 * @param topLevel Top-level components to refine and work down from. Strictly speaking, it is not essential that these
 * components be top-level (i.e. have no args), but most components that are not top-level cannot be refined without
 * first refining their args.
 * @param problem Problem to refine.
 * @param rangeSizer Method to determine the size of the range of components.
 * @param parameterized Indicates if whether or not to make parameterized factors.
 * @param done Problem components that were already processed, which should not be visited again. This is explicitly a
 * mutable set so that nested decomposition strategies can update any enclosing decomposition strategy with the
 * components that were processed. Defaults to the empty set.
 */
class TopDownStrategy(topLevel: List[ProblemComponent[_]], problem: Problem, rangeSizer: RangeSizer, parameterized: Boolean,
                      done: mutable.Set[ProblemComponent[_]] = mutable.Set())
  extends DecompositionStrategy(problem, rangeSizer, parameterized, done) {

  // Never recurse on subproblems because we don't want to modify the factor graph structure
  override def recurse(nestedProblem: NestedProblem[_]): Option[DecompositionStrategy] = None

  // Top-down strategies assume the collection contains args of any element we refine, hence this is unchecked
  override protected def checkArg[T](element: Element[T]): ProblemComponent[T] = problem.collection(element)

  // Finds the direct children of the given component that are both in the component collection and not fully refined.
  // Used in computing the set of components that need updates after refining the top-level components.
  protected def children(comp: ProblemComponent[_]): Traversable[ProblemComponent[_]] = {
    val elem = comp.element
    elem.universe.directlyUsedBy(elem).flatMap{child =>
      // Only returns the child if it is in the collection and not fully refined
      if(problem.collection.contains(child)) {
        val childComp = problem.collection(child)
        if(childComp.fullyRefined) None
        else Some(childComp)
      }
      else None
    }
  }

  // The set of components reachable from topLevel through components not fully refined (includes top-level)
  protected val reachable = util.reachable(children, true, topLevel:_*)

  // Refine a component if it is not fully refined and it is reachable from one of the top-level components
  override protected def shouldRefine(comp: ProblemComponent[_]): Boolean = reachable.contains(comp)

  // Refine all components reachable from the top-level elements, excluding those already fully refined
  override val initialComponents: List[ProblemComponent[_]] = reachable.toList
}
