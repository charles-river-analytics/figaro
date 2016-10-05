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
 * Refining strategies that operate by decomposing subproblems. This involves processing problem components by
 * generating ranges for variables, expanding relevant subproblems, and creating factors. The strategy visits each
 * relevant component exactly once. This does not solve the problem or any nested subproblems.
 * @param problem The problem to decompose
 * @param rangeSizer The method to determine the range of components
 * @param bounds Lower or Upper bounds
 * @param parameterized Indicates if this problem parameterized
 * @param done Problem components that were already processed, which should not be visited again. This is explicitly a
 * mutable set so that nested decomposition strategies can update any enclosing decomposition strategy with the
 * components that were processed. Defaults to the empty set.
 */
private[figaro] abstract class DecompositionStrategy(problem: Problem, val rangeSizer: RangeSizer, val bounds: Bounds,
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
   */
  def execute(components: List[ProblemComponent[_]]) = {
    // Only proceed if there is something to refine, i.e. a component not fully expanded.
    if(components.exists(!_.fullyExpanded)) {
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

  /*
   * Process a component by generating its range and factors. This should not be called on a component that was
   * previously processed in this way, which includes fully expanded components.
   */
  protected def makeRangeAndFactors(comp: ProblemComponent[_]) {
    comp.generateRange(rangeSizer(comp))
    comp.makeNonConstraintFactors(parameterized)
    comp.makeConstraintFactors(bounds)
  }

  /*
   * backwardChain takes a list of items to process.
   * When the first item is taken off the list, it checks whether it has already been done.
   * When an item is taken off the list, if it has not been done, all the items it depends on
   * are added to the list to do. This guarantees that when an item is finally processed,
   * all the items it depends on have already been processed. Also, we do not process
   * any items more than once.
   *
   * TODO comment on fullyExpanded
   */
  protected[figaro] def backwardChain(toDo: List[ProblemComponent[_]]): Unit = {
    toDo match {
      case first :: rest =>
        // Don't process any components that we previously visited, or that are known to need no further expansion.
        if (done.contains(first) || first.fullyExpanded) backwardChain(rest)
        else {
          val argComponents = first.element.args.map(checkArg(_))
          val argsFullyExpanded = argComponents.forall(_.fullyExpanded)
          if (argComponents.nonEmpty) backwardChain(argComponents)
          first match {
            case chainComp: ChainComponent[_, _] =>
              processChain(chainComp, argsFullyExpanded)
            case maComp: MakeArrayComponent[_] =>
              processMakeArray(maComp, argsFullyExpanded)
            case _ =>
              // TODO do we allow decomposables?
              process(first, argsFullyExpanded)
          }
          done += first
          backwardChain(rest)
        }
      case _ =>
    }
  }

  // TODO comment these
  def processChain(chainComp: ChainComponent[_, _], argsFullyExpanded: Boolean) = {
    chainComp.expand()
    val subStrategies = chainComp.subproblems.values.flatMap(recurse)
    subStrategies.foreach(_.execute())
    makeRangeAndFactors(chainComp)
    if(argsFullyExpanded && chainComp.subproblems.forall(_._2.fullyExpanded)) chainComp.fullyExpanded = true
  }

  def processMakeArray(maComp: MakeArrayComponent[_], argsFullyExpanded: Boolean) = {
    maComp.expand()
    val items = maComp.makeArray.items.take(maComp.maxExpanded).toList
    val itemComponents = items.map(checkArg(_))
    backwardChain(itemComponents)
    makeRangeAndFactors(maComp)
    if(argsFullyExpanded && itemComponents.forall(_.fullyExpanded)) maComp.fullyExpanded = true
  }

  def process(comp: ProblemComponent[_], argsFullyExpanded: Boolean) = {
    makeRangeAndFactors(comp)
    if(argsFullyExpanded) comp.fullyExpanded = true
  }
}
