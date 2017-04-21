/*
 * DecompositionStrategy.scala
 * Base class for strategies that decompose a problem
 *
 * Created By:      Brian Ruttenberg (bruttenberg@cra.com)
 * Creation Date:   July 1, 2015
 *
 * Copyright 2015 Avrom J. Pfeffer and Charles River Analytics, Inc.
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
 * Refining strategies that operate by decomposing components. This involves processing problem components by generating
 * ranges for variables and expanding relevant subproblems. This strategy visits components at most once, so generally
 * it is of no use to call `execute` multiple times.
 *
 * Needed components are discovered lazily by working backwards from the initial components. As a result, the strategy
 * makes no guarantees about visiting components that are not needed.
 *
 * Expanding subproblems proceeds in a depth-first manner. To avoid infinite recursions, the strategy sets a subproblem
 * as open before executing recursively on the subproblem. When refining of the subproblem is complete, the subproblem
 * is once again set as closed. For this to work correctly, this usually means that if a recursive
 * `DecompositionStrategy` is called from anything but the top-level, any subproblems that contain `initialComponents`
 * directly or indirectly must also be marked as open. Thus, it is usually safest to call `DecompositionStrategy` from a
 * set of top-level components.
 * @param collection Collection of components to refine.
 * @param done Problem components that were already processed, which should not be visited again. This is explicitly a
 * mutable set so that nested decomposition strategies can update any enclosing decomposition strategy with the
 * components that were processed.
 */
private[figaro] abstract class DecompositionStrategy(collection: ComponentCollection, done: mutable.Set[ProblemComponent[_]])
  extends RefiningStrategy(collection) {

  /**
   * Optionally decompose a nested problem. The recursing strategy may not refine any components in the set `done`, but
   * can add to this set. This generally means passing `done` directly into the constructor of the recursing strategy.
   * @param nestedProblem Nested problem to decompose.
   * @return A decomposition strategy for the nested problem, or None if it should not be decomposed further.
   */
  def recurse(nestedProblem: NestedProblem[_]): Option[DecompositionStrategy]

  /**
   * Get the problem component associated with an element. This may involve adding the element to the collection if a
   * problem component has not yet been created.
   * @param element Element for which to return a problem component.
   */
  protected def checkArg[T](element: Element[T]): ProblemComponent[T]

  /**
   * Decide if this strategy should refine the given problem component. This is usually based on the refinement status
   * of the component and the components on which it depends.
   * @param comp Component to consider refining.
   * @return True if and only if this strategy should refine the argument. Note that this check is independent of the
   * set `done` (i.e. this method behaves the same whether or not this strategy has already refined the component).
   */
  protected def shouldRefine(comp: ProblemComponent[_]): Boolean

  /**
   * Recursively marks as unsolved any problem whose solution could have changed as a result of refinement by this
   * strategy or any of its recursively generated strategyes.
   */
  protected def markProblemsUnsolved(): Unit = {
    // Start with the problems associated with each visited component
    val initialProblems = done.map(_.problem).toSeq
    // From a subproblem, we must include the problems that use it
    def problemGraph(pr: Problem): Set[Problem] = pr match {
      case npr: NestedProblem[_] => collection.expandableComponents(npr).map(_.problem)
      case _ => Set()
    }
    // We have to work our way up the whole problem graph marking problems as unsolved; reachable does this efficiently
    val allUnsolvedProblems = util.reachable(problemGraph, true, initialProblems:_*)
    // Mark each reachable problem as unsolved
    for(pr <- allUnsolvedProblems) {
      pr.solved = false
      pr.solution = Nil
    }
  }

  /**
   * Initial components to process. Decomposition proceeds lazily in a bottom-up fashion from these components.
   * @return List of components from which to decompose.
   */
  def initialComponents: List[ProblemComponent[_]]

  /**
   * Execute this decomposition strategy, starting with the initial components.
   */
  override def execute(): Unit = {
    initialComponents.foreach(decompose)
    markProblemsUnsolved()
  }

  /**
   * Like `execute`, but doesn't mark problems as unsolved. This is intended as an optimization for recursive
   * strategies. Instead of working our way up the problem graph to mark problems as unsolved each time we finish
   * executing a recursive decomposition, this allows performing this in a single top-level step. This should be only be
   * used in strategies where the set `done` is passed to the recursively constructed strategy at each iteration. As
   * such, this method cannot be public.
   */
  protected def recursiveExecute(): Unit = initialComponents.foreach(decompose)

  /**
   * Decompose a single problem component by recursively decomposing components on which it depends, then generating the
   * range for each component. This guarantees that when ranges are created, all components on which a component depends
   * will have been processed.
   *
   * As a rule, this method never works in the other direction. If component B depends (directly or indirectly) on
   * component A, then calling decompose on A will never recursively call decompose on component B. This is to avoid
   * infinite loops, and to guarantee that the range of A does not unpredictably change in the middle of a call to
   * decompose A.
   *
   * Once the range has been created, the component will be added to the set done, and (if applicable) the component is
   * marked as fully enumerated or refined.
   * @param comp Component to decompose. This strategy never decomposes a component more than once, so if the component
   * is fully refined or in the set done, this method does nothing.
   */
  def decompose(comp: ProblemComponent[_]): Unit = {
    // Only process if the component is neither fully refined nor already visited
    if(!done.contains(comp) && shouldRefine(comp)) {
      comp match {
        case chainComp: ChainComponent[_, _] =>
          processChain(chainComp)
        case maComp: MakeArrayComponent[_] =>
          processMakeArray(maComp)
        case atomicComp: AtomicComponent[_] =>
          processAtomic(atomicComp)
        case _ =>
          process(comp)
      }
      done += comp
    }
  }

  /**
   * Generate the range for the given Chain component. Mark it as fully refined or enumerated if applicable.
   * @param chainComp Chain component to process. This should not be called on a component we previously processed,
   * including fully refined components.
   */
  def processChain(chainComp: ChainComponent[_, _]): Unit = {
    // Decompose the parent to get values for expansion
    val parentComp = checkArg(chainComp.chain.parent)
    decompose(parentComp)
    // Ensure expansions exist for each parent value
    chainComp.expand()
    // Optionally recurse on subproblems. The recursive strategies do not change the range of the parent because
    // parentComp is in the set done. This preserves the current state where we have expanded subproblems for each
    // parent value.
    val subproblems = chainComp.subproblems.values
    for(subproblem <- subproblems ; strategy <- recurse(subproblem)) {
      // Mark subproblem as open to avoid infinite recursion
      subproblem.open = true
      // Use the recursive execute method so we only mark problems as unsolved once
      strategy.recursiveExecute()
      subproblem.open = false
    }
    // Make range based on the refinement of the subproblems
    generateRange(chainComp)
    // The range for this component is complete if the range of the parent is complete (and therefore no further
    // subproblems can be created), and the target for each subproblem has a complete range
    chainComp.fullyEnumerated =
      parentComp.fullyEnumerated && subproblems.forall { sp => collection(sp.target).fullyEnumerated }
    // If all components in the subproblems are fully refined, then the chain component is also fully refined
    chainComp.fullyRefined = chainComp.fullyEnumerated && subproblems.forall(_.fullyRefined)
  }

  /**
   * Generate the range for the given MakeArray component. Mark it as fully refined or enumerated if applicable.
   * @param maComp MakeArray component to process. This should not be called on a component we previously processed,
   * including fully refined components.
   */
  def processMakeArray(maComp: MakeArrayComponent[_]): Unit = {
    // Decompose the number of items component to get the maximum number of expansions
    val numItemsComp = checkArg(maComp.makeArray.numItems)
    decompose(numItemsComp)
    // Ensure expansions exist up to the maximum number of items
    maComp.expand()
    // Decompose each of the items
    val items = maComp.makeArray.items.take(maComp.maxExpanded).toList
    val itemsComps = items.map(checkArg(_))
    itemsComps.foreach(decompose)
    // Make range based on the ranges of the items
    generateRange(maComp)
    // The range for this component is complete if the number of items and each item have complete ranges
    // This also implies that the component is fully refined because there are no subproblems
    maComp.fullyEnumerated = numItemsComp.fullyEnumerated && itemsComps.forall(_.fullyEnumerated)
    maComp.fullyRefined = maComp.fullyEnumerated
  }

  /**
   * Generate the range for the given atomic component. Mark it as fully refined or enumerated if applicable.
   * @param atomicComp Atomic component to process. This should not be called on a component we previously processed,
   * including fully refined components.
   */
  def processAtomic(atomicComp: AtomicComponent[_]): Unit = {
    // An atomic element has no args; simply generate its range
    generateRange(atomicComp)
    // Decide if the component is fully enumerated/refined based on the ranging strategy used
    atomicComp.fullyRefined = atomicComp.fullyRefinable()
    atomicComp.fullyEnumerated = atomicComp.fullyRefined
  }

  /**
   * Generate the range for the given component. Mark it as fully refined or enumerated if applicable.
   * @param comp Component to process. This should not be called on a component we previously processed, including fully
   * refined components.
   */
  def process(comp: ProblemComponent[_]): Unit = {
    // Decompose the args of this component
    val argComps = comp.element.args.map(checkArg(_))
    argComps.foreach(decompose)
    // Make range based on the ranges of the args
    generateRange(comp)
    // We need to know if all args are fully enumerated to determine the enumeration and refinement status
    val argsFullyEnumerated = argComps.forall(_.fullyEnumerated)
    // If the range doesn't have *, the enumeration status breaks into several cases
    comp.fullyEnumerated = !comp.range.hasStar && {
      comp.element match {
        // Ranges of compound/parameterized Flip and Select don't depend on parents
        case _: Flip | _: Select[_, _] => true
        // CompoundDist range does not depend on the probabilities; only the outcomes need to be enumerated
        case d: CompoundDist[_] => d.outcomes.forall(o => checkArg(o).fullyEnumerated)
        // Otherwise, we assume the range is generated deterministically from the element's args, which means this is
        // enumerable if and only if all of the args are fully enumerated
        case _ => argsFullyEnumerated
      }
    }
    // We assume the factors depend only on the ranges of this component and its arguments. When all of these components
    // are fully enumerated, the factors of this component can't change any further, and therefore it is fully refined.
    comp.fullyRefined = comp.fullyEnumerated && argsFullyEnumerated
  }
}
