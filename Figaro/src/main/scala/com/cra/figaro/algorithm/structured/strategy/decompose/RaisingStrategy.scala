/*
 * RaisingStrategy.scala
 * A strategy that chooses to raise or solve a sub-problem based on a decision function
 *
 * Created By:      Brian Ruttenberg (bruttenberg@cra.com)
 * Creation Date:  Oct 1, 2015
 *
 * Copyright 2015 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */
package com.cra.figaro.algorithm.structured.strategy.decompose

import com.cra.figaro.language._
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

private[figaro] class RaisingStrategy(problem: Problem, solvingStrategy: SolvingStrategy,
  recursingStrategy: Problem => DecompositionStrategy, raisingCriteria: (Problem, Problem) => Boolean, rangeSizer: RangeSizer,
  bounds: Bounds, parameterized: Boolean)
  extends StructuredStrategy(problem, solvingStrategy, recursingStrategy, rangeSizer, bounds, parameterized) with BackwardChain {

  override def execute(components: List[ProblemComponent[_]]) {
    backwardChain(components, Set[ProblemComponent[_]]())
    if (problem.componentsToVisit.nonEmpty) backwardChain(problem.componentsToVisit.toList, Set[ProblemComponent[_]]())
  }
  
  def typedProcessChain[ParentValue, Value](first: ProblemComponent[_], rest: List[ProblemComponent[_]], done: Set[ProblemComponent[_]], chainComp: ChainComponent[ParentValue, Value]): Set[ProblemComponent[_]] = {
    chainComp.expand()
    val subStrategies = chainComp.subproblems.map(v => (v._1, v._2, decompose(v._2, done)))
    subStrategies.foreach(ds => if (ds._3.nonEmpty) ds._3.get.execute())
    process(chainComp)
    subStrategies.foreach(ds => {
      val (parentValue, subproblem, strategy) = ds
      if (strategy.nonEmpty && raisingCriteria(problem, subproblem)) {
        chainComp.raise(parentValue, bounds)
      } else {
        if (strategy.nonEmpty) subproblem.solve(strategy.get.solvingStrategy, strategy.get.bounds)
        // if we decide to solve this subproblem, we need to bring up the subproblem solution factors since we've already
        // called process (and that normally does it)
        chainComp.raiseSubproblemSolution(parentValue, subproblem)
      }
    })
    backwardChain(rest, done + chainComp)
  }
  
  override def processChain(first: ProblemComponent[_], rest: List[ProblemComponent[_]], done: Set[ProblemComponent[_]], chainComp: ChainComponent[_, _]): Set[ProblemComponent[_]] = {
    typedProcessChain(first, rest, done, chainComp)
  }

}

object RaisingStrategy {
  
  def raiseIfGlobal(parent: Problem, subproblem: Problem): Boolean = {
    subproblem.components.exists(pc => {
      val a = subproblem.internal(pc.variable) 
      val b = subproblem.targets.contains(pc.element)  
      !a && !b
    })
  }
  
  
}


