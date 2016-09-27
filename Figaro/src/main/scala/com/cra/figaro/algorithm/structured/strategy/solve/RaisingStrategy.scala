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

import com.cra.figaro.algorithm.factored.factors.Factor
import com.cra.figaro.algorithm.structured._

/**
 * A solving strategy that raises subproblems, or solves them and raises their solutions. Classes that implement this
 * trait must specify when to raise subproblems, and how to solve subproblems.
 */
trait RaisingStrategy extends SolvingStrategy {

  /**
   * Decide whether to raise the given subproblem.
   * @param subproblem A nested problem to consider raising.
   * @return True if the subproblem should be raised, false if it should be solved and have its solution raised.
   */
  def raisingCriteria(subproblem: NestedProblem[_]): Boolean

  /**
   * Solve the nested subproblem. This can be done recursively by calling `solve`, or a new strategy can be created.
   * @param subproblem Subproblem to solve.
   * @param bounds Bounds for constraint factors.
   */
  def solveNested(subproblem: NestedProblem[_], bounds: Bounds): Unit

  /**
   * Process the subproblems for the given component by either choosing to raise them without solving, or choosing to
   * solve them and raise their solutions.
   * @param chainComp Component to process.
   * @param bounds Bounds for constraint factors.
   */
  def raiseSubproblems[ParentValue, Value](chainComp: ChainComponent[ParentValue, Value], bounds: Bounds): Unit = {
    for((parentValue, subproblem) <- chainComp.subproblems) {
      if(raisingCriteria(subproblem)){
        // Raise the subproblem without solving it
        chainComp.raise(parentValue, bounds)
      }
      else {
        solveNested(subproblem, bounds)
        chainComp.raiseSubproblemSolution(parentValue, subproblem)
      }
    }
  }

  override def getFactors(component: ProblemComponent[_], bounds: Bounds): List[Factor[Double]] = {
    component match {
      case chainComp: ChainComponent[_, _] =>
        raiseSubproblems(chainComp, bounds)
      case _ =>
    }
    component.nonConstraintFactors ::: component.constraintFactors(bounds)
  }
}
