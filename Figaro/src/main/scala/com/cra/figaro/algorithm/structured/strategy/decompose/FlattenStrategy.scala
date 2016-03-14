/*
 * FlattenStrategy.scala
 * A strategy that flattens each problem and solves the problem in a flat manner
 *
 * Created By:      Brian Ruttenberg (bruttenberg@cra.com)
 * Creation Date:   July 1, 2015
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
 * A strategy that flattens the model and uses no structure to solve the problem
 */
private[figaro] class FlattenStrategy(problem: Problem, solvingStrategy: SolvingStrategy, recursingStrategy: Problem => DecompositionStrategy, rangeSizer: RangeSizer,
  bounds: Bounds, parameterized: Boolean)
  extends StructuredStrategy(problem, solvingStrategy, recursingStrategy, rangeSizer, bounds, parameterized) with BackwardChain {

  override def execute(components: List[ProblemComponent[_]]) {
    backwardChain(components, Set[ProblemComponent[_]]())
    if (problem.componentsToVisit.nonEmpty) backwardChain(problem.componentsToVisit.toList, Set[ProblemComponent[_]]())
  }
  
  override def processChain(first: ProblemComponent[_], rest: List[ProblemComponent[_]], done: Set[ProblemComponent[_]], chainComp: ChainComponent[_, _]): Set[ProblemComponent[_]] = {
    chainComp.expand()
    val subStrategies = chainComp.subproblems.values.map(decompose(_, done))
    subStrategies.foreach(ds => if (ds.nonEmpty) ds.get.execute())
    process(chainComp)
    chainComp.raise(bounds)
    backwardChain(rest, done + chainComp)
  }

}
