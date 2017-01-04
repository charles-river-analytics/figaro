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
package com.cra.figaro.algorithm.structured.strategy.solve

import com.cra.figaro.algorithm.structured._
import com.cra.figaro.algorithm.structured.strategy.RecursiveStrategy

/**
 * A solving strategy that recursively raises subproblems, or solves them and raises their solutions.
 */
abstract class RaisingStrategy(problem: Problem, raisingCriteria: RaisingCriteria) extends SolvingStrategy(problem)
  with RecursiveStrategy {

  override def decideToRaise(): Boolean = raisingCriteria(problem)

  /**
   * Returns a strategy to solve the nested problem, or None if e.g. the subproblem is already solved.
   * @param subproblem Nested problem to consider recursing on.
   * @return A strategy to recurse on the nested problem, or None if it should not recurse further.
   */
  override def recurse(subproblem: NestedProblem[_]): Option[RaisingStrategy]

  override def raiseSubproblems[ParentValue, Value](chainComp: ChainComponent[ParentValue, Value]): Unit = {
    // If the chain's range is just {*}, then no factors were created and none of the subproblems are relevant
    if(!chainComp.range.hasStar || chainComp.range.regularValues.nonEmpty) {
      for((parentValue, subproblem) <- chainComp.subproblems) {
        val recursingStrategy = recurse(subproblem)
        if(recursingStrategy.nonEmpty) recursingStrategy.get.execute()

        if(subproblem.solved) chainComp.raiseSubproblemSolution(parentValue, subproblem)
        else chainComp.raise(parentValue)
      }
    }
  }

  /**
   * Execute on a nested problem. Because nested problems never have evidence variables, it doesn't matter which bounds
   * we use.
   */
  override def execute(): Unit = execute(Lower)
}
