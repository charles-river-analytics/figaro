/*
 * SolvingStrategy.scala
 * An abstract class that defines how to solve a problem
 *
 * Created By:      Brian Ruttenberg (bruttenberg@cra.com)
 * Creation Date:   July 1, 2015
 *
 * Copyright 2015 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */
package com.cra.figaro.algorithm.structured.strategy.solve

import com.cra.figaro.algorithm.structured._
import com.cra.figaro.algorithm.factored.factors.{Factor, Variable}
import com.cra.figaro.algorithm.structured.strategy.ProblemStrategy

/**
 * A solving strategy solves an inference problem after a series of refinements have been made. This involves solving or
 * raising subproblems first, then eliminating variables that do not belong in the solution.
 */
private[figaro] abstract class SolvingStrategy(problem: Problem) extends ProblemStrategy(problem) {
  /**
   * Get the constraint and non-constraint factors associated with a component. If the component has subproblems, this
   * includes solving the subproblems and raising their solutions, or raising the subproblem factors.
   * @param component Component for which to get factors.
   * @param bounds Bounds for constraint factors.
   * @return All factors needed for this component to solve the problem that contains it.
   */
  def getFactors(component: ProblemComponent[_], bounds: Bounds): List[Factor[Double]]

  /**
   * Solve the problem by eliminating variables, leaving only the ones that belong in the solution.
   * @param toEliminate Variables to eliminate.
   * @param toPreserve Variables to preserve.
   * @param factors Factors over which to perform elimination.
   * @return A list of factors over the variables to preserve representing their joint distribution, and a map of
   * recording factors for MPE.
   */
  def eliminate(toEliminate: Set[Variable[_]], toPreserve: Set[Variable[_]], factors: List[Factor[Double]]):
    (List[Factor[Double]], Map[Variable[_], Factor[_]])

  /**
   * Solve the problem defined by all the components' current factors. This involves solving and incorporating
   * subproblems as well. This will set the globals accordingly. All components in this problem and contained
   * subproblems should be eliminated in the solution. This does nothing if the problem is already solved.
   * @param bounds Bounds for constraint factors.
   */
  def execute(bounds: Bounds): Unit = {
    // TODO should this be a final def?
    if(!problem.solved) {
      val collection = problem.collection
      val allFactors = problem.components.flatMap(getFactors(_, bounds))
      val allVariables = (Set[Variable[_]]() /: allFactors)(_ ++ _.variables)
      val (toEliminate, toPreserve) = allVariables.partition(problem.internal)
      problem.globals = toPreserve.map(collection.variableToComponent(_))
      val (solution, recordingFactors) = eliminate(toEliminate, toPreserve, allFactors)
      problem.solution = solution
      problem.recordingFactors = recordingFactors
      problem.solved = true
      toEliminate.foreach((v: Variable[_]) => {
        // TODO might this cause bugs?
        if (collection.intermediates.contains(v)) collection.intermediates -= v
      })
    }
  }

  override def execute(): Unit = execute(Lower)
}