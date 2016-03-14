/*
 * StructuredStrategy.scala
 * A strategy that fully expands a problem and solves the subproblems in a structured manner.
 *
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   March 1, 2015
 *
 * Copyright 2015 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */
package com.cra.figaro.algorithm.structured.strategy.decompose

import com.cra.figaro.algorithm.structured.strategy.solve.SolvingStrategy
import com.cra.figaro.algorithm.structured._

/**
 * A structured strategy that decomposes Chains and MakeArrays according to a given recursive strategy
 * @param problem The problem to decompose
 * @param solvingStrategy The solving strategy used to solve this problem
 * @param recursingStrategy A function that returns a new Decomposition strategy given a nested problem
 * @param rangeSizer The method to determine the range of components
 * @param bounds Lower or Upper bounds
 * @param parameterized Indicates if this problem parameterized
 */
private[figaro] class StructuredStrategy(problem: Problem, solvingStrategy: SolvingStrategy,
  recursingStrategy: Problem => DecompositionStrategy, rangeSizer: RangeSizer,
  bounds: Bounds, parameterized: Boolean)
  extends DecompositionStrategy(problem, solvingStrategy, rangeSizer, bounds, parameterized) with BackwardChain {

  def execute(components: List[ProblemComponent[_]]) {
    backwardChain(components, Set[ProblemComponent[_]]())
    if (problem.componentsToVisit.nonEmpty) backwardChain(problem.componentsToVisit.toList, Set[ProblemComponent[_]]())
    problem.solve(solvingStrategy, bounds)
  }
  
  /**
   * For the structured strategy, if the problem is not solved the recursing strategy is invoked
   */
  def decompose(nestedProblem: Problem, done: Set[ProblemComponent[_]]): Option[DecompositionStrategy] = {
    // This check is important and is what enables us to perform dynamic programming
    if (!nestedProblem.solved) Some(recursingStrategy(nestedProblem)) else None
  }

  def processChain(first: ProblemComponent[_], rest: List[ProblemComponent[_]], done: Set[ProblemComponent[_]], chainComp: ChainComponent[_, _]): Set[ProblemComponent[_]] = {
    chainComp.expand()    
    val subStrategies = chainComp.subproblems.values.map(decompose(_, done))
    subStrategies.foreach(ds => if (ds.nonEmpty) ds.get.execute())    
    process(chainComp)
    backwardChain(rest, done + chainComp)
  }

  def processMakeArray(first: ProblemComponent[_], rest: List[ProblemComponent[_]], done: Set[ProblemComponent[_]], maComp: MakeArrayComponent[_]): Set[ProblemComponent[_]] = {
    maComp.expand()
    val items = maComp.makeArray.items.take(maComp.maxExpanded).toList
    val itemComponents = items.map(checkArg(_))
    val done2 = backwardChain(itemComponents, done)
    process(maComp)
    backwardChain(rest, done2 + maComp)
  }
}
