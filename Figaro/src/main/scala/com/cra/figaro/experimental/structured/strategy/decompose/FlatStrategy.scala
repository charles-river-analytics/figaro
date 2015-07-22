/*
 * StructuredSolver.scala
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
package com.cra.figaro.experimental.structured.strategy.decompose

import com.cra.figaro.experimental.structured._
import com.cra.figaro.experimental.structured.solver.Solver
import com.cra.figaro.language.Element
import com.cra.figaro.experimental.structured.factory.Factory
import com.cra.figaro.algorithm.factored.factors.Factor

private[figaro] class FlatStrategy(problem: Problem, solver: Solver,
  recursingStrategy: Strategy, rangeSizer: RangeSizer,
  bounds: Bounds, parameterized: Boolean)
  extends DecompositionStrategy(problem, solver, recursingStrategy, rangeSizer, bounds, parameterized) with BackwardChain {

  def execute() {
    backwardChain(problem.components, Set[ProblemComponent[_]]())
    problem.solve(solver)
  }

  private def recurse(nestedProblem: NestedProblem[_], done: Set[ProblemComponent[_]]) {
    // This check is important and is what enables us to perform dynamic programming
    if (!nestedProblem.solved) recursingStrategy(nestedProblem)
  }

  def processChain(first: ProblemComponent[_], rest: List[ProblemComponent[_]], done: Set[ProblemComponent[_]], chainComp: ChainComponent[_, _]): Set[ProblemComponent[_]] = {
    chainComp.expand()
    val results = chainComp.subproblems.values.map(_.target)
    val resultComponents = results.map(checkArg(_))
    val done2 = if (resultComponents.exists(p => !done.contains(p))) {
      backwardChain(resultComponents.toList, done)
    } else {
      done
    }
    process(chainComp)
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
