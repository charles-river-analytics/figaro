/*
 * FlatStrategy.scala
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
package com.cra.figaro.experimental.structured.strategy.decompose

import com.cra.figaro.experimental.structured._
import com.cra.figaro.experimental.structured.solver.Solver
import com.cra.figaro.language.Element
import com.cra.figaro.experimental.structured.factory.Factory
import com.cra.figaro.algorithm.factored.factors.Factor
import com.cra.figaro.experimental.structured.strategy.solve.SolvingStrategy

/**
 * A strategy that flattens the model and uses no structure to solve the problem
 */
private[figaro] class FlatStrategy(problem: Problem, solvingStrategy: SolvingStrategy, rangeSizer: RangeSizer,
  bounds: Bounds, parameterized: Boolean)
  extends DecompositionStrategy(problem, solvingStrategy, rangeSizer, bounds, parameterized) with BackwardChain {

  def execute() {
    backwardChain(problem.components, Set[ProblemComponent[_]]())
    problem.solve(solvingStrategy)
  }

  def decompose(nestedProblem: Problem, done: Set[ProblemComponent[_]]): Unit = {}

  def processChain(first: ProblemComponent[_], rest: List[ProblemComponent[_]], done: Set[ProblemComponent[_]], chainComp: ChainComponent[_, _]): Set[ProblemComponent[_]] = {
    chainComp.expand()
    val results = chainComp.subproblems.values.map(_.target)
    val resultComponents = results.map(checkArg(_))
    // backward chain on the subproblems if some have not been solved
    val done2 = if (resultComponents.exists(p => !done.contains(p))) {
      backwardChain(resultComponents.toList, done)
    } else {
      done
    }
    // Process this chain component (make the factors)
    process(chainComp)
    // Once the factors have been created, we raise the factors from all subproblems into this problem
    chainComp.raise(bounds)
    backwardChain(rest, done2 + chainComp)
  }


  def processMakeArray(first: ProblemComponent[_], rest: List[ProblemComponent[_]], done: Set[ProblemComponent[_]], maComp: MakeArrayComponent[_]): Set[ProblemComponent[_]] = {
    maComp.expand()
    val items = maComp.makeArray.items.take(maComp.maxExpanded).toList
    val itemComponents = items.map(checkArg(_))
    val done2 = backwardChain(itemComponents ::: List(first) ::: rest, done)
    process(maComp)
    backwardChain(rest, done2 + maComp)
  }
}
