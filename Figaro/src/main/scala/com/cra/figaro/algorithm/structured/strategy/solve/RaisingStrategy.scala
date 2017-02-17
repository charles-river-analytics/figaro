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
import com.cra.figaro.algorithm.factored.factors.factory.{ChainFactory, Factory}
import com.cra.figaro.algorithm.structured._

/**
 * A solving strategy that handles subproblems. It either recursively raises subproblems, or solves them and raises
 * their solutions.
 * @param problem Problem to solve.
 * @param raisingCriteria Decision function to decide whether to solve a subproblem or raise without elimination.
 */
abstract class RaisingStrategy(problem: Problem, raisingCriteria: RaisingCriteria) extends SolvingStrategy(problem) {

  /**
   * Returns a strategy that could be used to solve the nested problem.
   * @param subproblem Unsolved nested problem to recurse on.
   * @return A strategy to solve the nested problem.
   */
  def recurse(subproblem: NestedProblem[_]): RaisingStrategy

  /**
   * Get all of the non-constraint factors needed for solving. This includes subproblem factors.
   * @return Non-constraint factors for solving.
   */
  override def nonConstraintFactors(): List[Factor[Double]] = problem.components.flatMap {
    case chainComp: ChainComponent[_, _] =>
      chainNonConstraintFactors(chainComp)
    case comp =>
      comp.nonConstraintFactors()
  }

  /**
   * Get the non-constraint factors associated with all subproblems of a Chain component. This returns the existing
   * solution if there is one. Otherwise, it chooses to solve or raise the subproblem based on the raising criteria.
   * @param chainComp Chain component whose subproblems are to be processed.
   * @return All factors associated with subproblems that are needed for solving, grouped by parent value.
   */
  def subproblemNonConstraintFactors[ParentValue, Value](chainComp: ChainComponent[ParentValue, Value]): Map[ParentValue, List[Factor[Double]]] = {
    for((parentValue, subproblem) <- chainComp.subproblems) yield {
      val subproblemFactors = if(subproblem.solved) {
        // If the subproblem has a solution, return it
        subproblem.solution
      }
      else {
        // Otherwise, recurse on the subproblem
        val recursingStrategy = recurse(subproblem)

        if(raisingCriteria(subproblem)) {
          // If we decide to raise without solving, copy the factors from the recursing strategy
          recursingStrategy.nonConstraintFactors()
        }
        else {
          // Otherwise, compute the solution
          // Because this is a nested problem, it doesn't matter which bounds we use
          recursingStrategy.execute()
          subproblem.solution
        }
      }
      parentValue -> subproblemFactors
    }
  }

  /**
   * Get non-constraint factors associated with a single Chain component.
   * @param chainComp Chain component to process.
   * @return All factors associated with the chain component that are needed for solving. This includes (possibly
   * eliminated) subproblem factors.
   */
  def chainNonConstraintFactors[ParentValue, Value](chainComp: ChainComponent[ParentValue, Value]): List[Factor[Double]] = {
    // If the range is {*}, there is no need to raise subproblems because they will all be uniquely *
    if(chainComp.range.regularValues.isEmpty) return chainComp.nonConstraintFactors()
    // Process each subproblem and collect the corresponding factors by parent value
    val collectedSubproblemFactors = subproblemNonConstraintFactors(chainComp)

    if(problem.collection.useSingleChainFactor && chainComp.allSubproblemsEliminatedCompletely) {
      // Use a single condensed factor
      ChainFactory.makeSingleFactor(problem.collection, chainComp.chain)
    }
    else {
      // Before returning the subproblem factors, we need to replace the variable in the raised factors with the actual
      // variable used in the Chain. This requires first calling nonConstraintFactors() to get actualSubproblemVariables
      // for each subproblem.
      val chainFactors = chainComp.nonConstraintFactors()
      val allSubproblemFactors = chainComp.subproblems.flatMap { case (parentValue, subproblem) =>
        // Replace the variable in the factors for a particular subproblem
        val subproblemFactors = collectedSubproblemFactors(parentValue)
        // Variable associated with the target in the subproblem and chain factors, respectively
        val compVar = subproblem.collection(subproblem.target).variable
        val actualVar = chainComp.actualSubproblemVariables(parentValue)
        subproblemFactors.map(Factory.replaceVariable(_, compVar, actualVar))
      }
      // Return all subproblem factors, and the chain factors that connect them
      chainFactors ++ allSubproblemFactors
    }
  }
}
