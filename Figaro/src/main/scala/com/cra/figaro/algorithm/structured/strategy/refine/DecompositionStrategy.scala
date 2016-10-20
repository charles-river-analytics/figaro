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
 * subproblems, and creating factors. The strategy visits each needed component exactly once. Needed components are
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
   * Optionally decompose a nested problem.
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
    // Only proceed if there is something to refine, i.e. a component not fully expanded.
    if(components.exists(!_.fullyExpanded)) {
      // TODO take a closer look at criteria for removing problem solutions
      // In particular, there could be problems with failing to propagate that the solution changed to superproblems.
      // What happens if one of the problems along the way used a raising strategy which didn't solve it?
      problem.solved = false
      problem.solution = List()
      backwardChain(components)
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
   * Process a component by generating its range and factors.
   * @param comp Component to process. This should not be called on a component we previously processed, including fully
   * expanded components.
   */
  protected def makeRangeAndFactors(comp: ProblemComponent[_]) {
    comp.generateRange(rangeSizer(comp))
    comp.makeNonConstraintFactors(parameterized)
    comp.makeConstraintFactors(Lower)
    comp.makeConstraintFactors(Upper)
  }

  /*
   * backwardChain takes a list of items to process.
   * When the first item is taken off the list, it checks whether it has already been done.
   * When an item is taken off the list, if it has not been done, all the items it depends on
   * are added to the list to do. This guarantees that when an item is finally processed,
   * all the items it depends on have already been processed. Also, we do not process
   * any items more than once.
   */
  protected[figaro] def backwardChain(toDo: List[ProblemComponent[_]]): Unit = {
    toDo match {
      case first :: rest =>
        // Don't process any components that we previously visited, or that are known to need no further expansion.
        if (done.contains(first) || first.fullyExpanded) backwardChain(rest)
        else {
          val argComponents = first.element.args.map(checkArg(_))
          if (argComponents.nonEmpty) backwardChain(argComponents)
          val argsFullyExpanded = argComponents.forall(_.fullyExpanded)
          first match {
            case chainComp: ChainComponent[_, _] =>
              processChain(chainComp, argsFullyExpanded)
            case maComp: MakeArrayComponent[_] =>
              processMakeArray(maComp, argsFullyExpanded)
            case _ =>
              process(first, argsFullyExpanded)
          }
          done += first
          backwardChain(rest)
        }
      case _ =>
    }
  }

  /**
   * Generate the range and factors for the given Chain component. Mark it as fully expanded if applicable.
   * @param chainComp Chain component to process. This should not be called on a component we previously processed,
   * including fully expanded components.
   * @param argsFullyExpanded True if and only if all parents of the Chain are fully expanded.
   */
  def processChain(chainComp: ChainComponent[_, _], argsFullyExpanded: Boolean) = {
    chainComp.expand()
    val subStrategies = chainComp.subproblems.values.flatMap(recurse)
    subStrategies.foreach(_.execute())
    makeRangeAndFactors(chainComp)
    if(argsFullyExpanded && chainComp.subproblems.forall(_._2.fullyExpanded)) chainComp.fullyExpanded = true
  }

  /**
   * Generate the range and factors for the given MakeArray component. Mark it as fully expanded if applicable.
   * @param maComp MakeArray component to process. This should not be called on a component we previously processed,
   * including fully expanded components.
   * @param argsFullyExpanded True if and only if all parents of the MakeArray are fully expanded.
   */
  def processMakeArray(maComp: MakeArrayComponent[_], argsFullyExpanded: Boolean) = {
    maComp.expand()
    val items = maComp.makeArray.items.take(maComp.maxExpanded).toList
    val itemComponents = items.map(checkArg(_))
    backwardChain(itemComponents)
    makeRangeAndFactors(maComp)
    if(argsFullyExpanded && itemComponents.forall(_.fullyExpanded)) maComp.fullyExpanded = true
  }

  /**
   * Generate the range and factors for the given component. Mark it as fully expanded if applicable.
   * @param comp Component to process. This should not be called on a component we previously processed, including fully
   * expanded components.
   * @param argsFullyExpanded True if and only if all parents of the component are fully expanded.
   */
  def process(comp: ProblemComponent[_], argsFullyExpanded: Boolean) = {
    makeRangeAndFactors(comp)
    // TODO this should also check if the range is completely enumerated
    if(argsFullyExpanded) comp.fullyExpanded = true
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

