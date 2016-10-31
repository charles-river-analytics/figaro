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
import com.cra.figaro.algorithm.structured.strategy.RecursiveStrategy
import com.cra.figaro.language.Element

import scala.collection.mutable

/**
 * Refining strategies that operate by decomposing subproblems. This does not solve the problem or any nested
 * subproblems, but involves processing problem components by generating ranges for variables, expanding relevant
 * subproblems, and creating factors. The strategy visits each needed component at most once. Needed components are
 * discovered lazily by working backwards from the target components. As a result, the strategy makes no guarantees
 * about visiting components that are not needed.
 * @param problem The problem to decompose
 * @param rangeSizer The method to determine the range of components
 * @param parameterized Indicates if this problem parameterized
 * @param done Problem components that were already processed, which should not be visited again. This is explicitly a
 * mutable set so that nested decomposition strategies can update any enclosing decomposition strategy with the
 * components that were processed. Defaults to the empty set.
 */
private[figaro] abstract class DecompositionStrategy(problem: Problem, val rangeSizer: RangeSizer,
  val parameterized: Boolean, done: scala.collection.mutable.Set[ProblemComponent[_]] = mutable.Set())
  extends RefiningStrategy(problem) with RecursiveStrategy {

  /**
   * Optionally decompose a nested problem. The recusing strategy may not refine any components in the set `done`, but
   * can add to this set. This generally means passing `done` directly into the constructor of the recursing strategy.
   * @param nestedProblem Nested problem to decompose.
   * @return A decomposition strategy for the nested problem, or None if it should not be decomposed further.
   */
  override def recurse(nestedProblem: NestedProblem[_]): Option[DecompositionStrategy]

  override def execute(): Unit = execute(problem.targets.map(problem.collection(_)))

  /**
   * Refine the problem by processing the given components.
   * @param components Initial components to decompose. Decomposition proceeds lazily from these components, so any
   * components that are neither in this list nor needed to process components in this list may not be processed.
   */
  def execute(components: List[ProblemComponent[_]]) = {
    val refinable = components.filterNot(_.fullyRefined)
    // Only proceed if there is something to refine
    if(refinable.nonEmpty) {
      // TODO take a closer look at criteria for removing problem solutions
      // In particular, there could be problems with failing to propagate that the solution changed to superproblems.
      // What happens if one of the problems along the way used a raising strategy which didn't solve it?
      problem.solved = false
      problem.solution = List()
      refinable.foreach(decompose)
    }
  }

  /**
   * Get the problem component associated with an element, generating it first if it hasn't been created.
   */
  protected def checkArg[T](element: Element[T]): ProblemComponent[T] = {
    if (problem.collection.contains(element)) problem.collection(element)
    else problem.add(element)
  }

  /**
   * Process a component by generating its range and factors. This includes non-constraint factors, as well as both
   * lower and upper bound constraint factors.
   * @param comp Component to process. This should not be called on a component we previously processed, including fully
   * refined components.
   */
  protected def makeRangeAndFactors(comp: ProblemComponent[_]) {
    comp.generateRange(rangeSizer(comp))
    comp.makeNonConstraintFactors(parameterized)
    comp.makeConstraintFactors(Lower)
    comp.makeConstraintFactors(Upper)
  }

  /**
   * Decompose a single problem component by recursively decomposing components on which it depends, then generating the
   * range and factors. This guarantees that when range and factors are created, all components on which a component
   * depends will have been processed.
   *
   * As a rule, this method never works in the other direction. If component B depends (directly or indirectly) on
   * component A, then calling decompose on A will never recursively call decompose on component B. This is to avoid
   * infinite loops, and to guarantee that the range and factors of A do not unpredictably change in the middle of a
   * call to decompose A.
   *
   * Once range and factors have been created, the component will be added to the set done, and (if applicable) the
   * component is marked as fully enumerated or expanded.
   * @param comp Component to decompose. This strategy never decomposes a component more than once, so if the component
   * is fully refined or in the set done, this method does nothing.
   */
  def decompose(comp: ProblemComponent[_]): Unit = {
    // Only process if the component is neither fully refined nor already visited
    if(!(comp.fullyRefined || done.contains(comp))) {
      comp match {
        case chainComp: ChainComponent[_, _] =>
          processChain(chainComp)
        case maComp: MakeArrayComponent[_] =>
          processMakeArray(maComp)
        case _ =>
          process(comp)
      }
      done += comp
    }
  }

  /**
   * Generate the range and factors for the given Chain component. Mark it as fully refined or enumerated if applicable.
   * @param chainComp Chain component to process. This should not be called on a component we previously processed,
   * including fully refined components.
   */
  def processChain(chainComp: ChainComponent[_, _]) = {
    // Decompose the parent to get values for expansion
    val parentComp = checkArg(chainComp.chain.parent)
    decompose(parentComp)
    // Ensure expansions exist for each parent value
    chainComp.expand()
    // Optionally recurse on subproblems. The recursive strategies do not change the range of the parent because
    // parentComp is in the set done. This preserves the current state where we have expanded subproblems for each
    // parent value.
    val subproblems = chainComp.subproblems.values
    subproblems.flatMap(recurse).foreach(_.execute())
    // Make range and factors based on the refinement of the subproblems
    makeRangeAndFactors(chainComp)
    // The range for this component is complete if the range of the parent is complete (and therefore no further
    // subproblems can be created), and the target for each subproblem has a complete range
    if(parentComp.fullyEnumerated && subproblems.forall { sp => sp.collection(sp.target).fullyEnumerated }) {
      chainComp.fullyEnumerated = true
      // If all components in the subproblems are fully refined, then the chain component is also fully refined.
      if(subproblems.forall(_.fullyRefined)) {
        chainComp.fullyRefined = true
      }

    }
  }

  /**
   * Generate the range and factors for the given MakeArray component. Mark it as fully refined or enumerated if
   * applicable.
   * @param maComp MakeArray component to process. This should not be called on a component we previously processed,
   * including fully refined components.
   */
  def processMakeArray(maComp: MakeArrayComponent[_]) = {
    // Decompose the number of items component to get the maximum number of expansions
    val numItemsComp = checkArg(maComp.makeArray.numItems)
    decompose(numItemsComp)
    // Ensure expansions exist up to the maximum number of items
    maComp.expand()
    // Decompose each of the items
    val items = maComp.makeArray.items.take(maComp.maxExpanded).toList
    val itemComponents = items.map(checkArg(_))
    itemComponents.foreach(decompose)
    // Make range and factors based on the ranges of the items
    makeRangeAndFactors(maComp)
    // The range for this component is complete if the number of items and each item have complete ranges. This also
    // implies that the component is fully refined because there are no subproblems.
    if(numItemsComp.fullyEnumerated && itemComponents.forall(_.fullyEnumerated)) {
      maComp.fullyEnumerated = true
      maComp.fullyRefined = true
    }
  }

  /**
   * Generate the range and factors for the given component. Mark it as fully expanded if applicable.
   * @param comp Component to process. This should not be called on a component we previously processed, including fully
   * refined components.
   */
  def process(comp: ProblemComponent[_]) = {
    // Decompose the args of this component
    val argComponents = comp.element.args.map(checkArg(_))
    argComponents.foreach(decompose)
    // Make range and factors based on the ranges of the args
    makeRangeAndFactors(comp)
    // The range and factors for this component are complete when the args all have complete ranges, since this is the
    // only information used in producing the factors for this component.
    if(argComponents.forall(_.fullyEnumerated)) {
      // TODO a check for elements with infinite support
      comp.fullyEnumerated = true
      comp.fullyRefined = true
    }
  }
}

/**
 * A full decomposition strategy completely decomposes the model, starting from the needed components in the problem.
 * This will not terminate on infinitely recursive models.
 * @param problem The problem to decompose
 * @param rangeSizer The method to determine the range of components
 * @param parameterized Indicates if this problem parameterized
 * @param done Problem components that were already processed, which should not be visited again. This is explicitly a
 * mutable set so that nested decomposition strategies can update any enclosing decomposition strategy with the
 * components that were processed. Defaults to the empty set.
 */
class FullDecompositionStrategy(problem: Problem, rangeSizer: RangeSizer, parameterized: Boolean, done: mutable.Set[ProblemComponent[_]] = mutable.Set())
  extends DecompositionStrategy(problem, rangeSizer, parameterized, done) {

  override def recurse(nestedProblem: NestedProblem[_]): Option[DecompositionStrategy] = {
    Some(new FullDecompositionStrategy(nestedProblem, rangeSizer, parameterized, done))
  }
}

/**
 * A lazy decomposition strategy decomposes to a finite depth, starting from the current problem.
 * @param maxDepth Absolute maximum depth to which subproblems may be expanded. This must be greater than or equal to the
 * depth of the given problem. So, if `maxDepth = problem.depth`, this corresponds to not recursing on any subproblems
 * of the given problem. Note that the existence of global components may allow expansion of subproblems that are at a
 * lower depth than the current problem.
 * @param problem The problem to decompose
 * @param rangeSizer The method to determine the range of components
 * @param parameterized Indicates if this problem parameterized
 * @param done Problem components that were already processed, which should not be visited again. This is explicitly a
 * mutable set so that nested decomposition strategies can update any enclosing decomposition strategy with the
 * components that were processed. Defaults to the empty set.
 */
class LazyDecompositionStrategy(maxDepth: Int, problem: Problem, rangeSizer: RangeSizer, parameterized: Boolean,
                                done: mutable.Set[ProblemComponent[_]] = mutable.Set())
  extends DecompositionStrategy(problem, rangeSizer, parameterized, done) {

  override def recurse(nestedProblem: NestedProblem[_]): Option[DecompositionStrategy] = {
    if(nestedProblem.depth <= maxDepth) {
      Some(new LazyDecompositionStrategy(maxDepth, nestedProblem, rangeSizer, parameterized, done))
    }
    else None
  }
}

