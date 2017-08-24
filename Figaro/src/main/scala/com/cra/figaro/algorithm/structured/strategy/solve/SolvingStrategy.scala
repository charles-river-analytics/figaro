/*
 * SolvingStrategy.scala
 * An abstract class that defines how to solve a problem
 *
 * Created By:      Brian Ruttenberg (bruttenberg@cra.com)
 * Creation Date:   July 1, 2015
 *
 * Copyright 2017 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */
package com.cra.figaro.algorithm.structured.strategy.solve

import com.cra.figaro.algorithm.structured._
import com.cra.figaro.algorithm.factored.factors.{Factor, Variable}

/**
 * A solving strategy solves an inference problem after a series of refinements have been made. This can involve solving
 * subproblems and collecting their factors, but this class does not account for subproblems by default. Executing the
 * strategy then eliminates variables that do not belong in the solution.
 *
 * By default, solving strategies do not use parameterized factors, but subclasses can override this.
 * @param problem Problem to solve.
 */
abstract class SolvingStrategy(problem: Problem) {

  /**
   * Get all of the non-constraint factors needed for solving.
   * @return Non-constraint factors for solving.
   */
  def nonConstraintFactors(): List[Factor[Double]] = {
    problem.components.flatMap(_.nonConstraintFactors())
  }

  /**
   * Get all of the constraint factors needed for solving.
   * @param bounds Bounds for the returned constraint factors.
   * @return Constraint factors for solving.
   */
  def constraintFactors(bounds: Bounds): List[Factor[Double]] = {
    // Nested problems can't have evidence
    if(problem.isInstanceOf[NestedProblem[_]]) List()
    else problem.components.flatMap(_.constraintFactors(bounds))
  }

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
   * subproblems should be eliminated in the solution.
   * @param bounds Bounds for constraint factors. Defaults to `Lower`. This default is intended for the cases where it
   * does not matter which bounds should be used because both upper and lower bounds would be the same.
   */
  def execute(bounds: Bounds = Lower): Unit = {
    val allFactors = constraintFactors(bounds) ::: nonConstraintFactors()
    val allVariables = (Set[Variable[_]]() /: allFactors)(_ ++ _.variables)
    val (toEliminate, toPreserve) = allVariables.partition(problem.internal)
    val collection = problem.collection
    problem.globals = toPreserve.map(collection.variableToComponent(_))
    val (solution, recordingFactors) = eliminate(toEliminate, toPreserve, allFactors)
    problem.solution = solution
    problem.recordingFactors = recordingFactors
    problem.solved = true
    toEliminate.foreach((v: Variable[_]) => {
      if (collection.intermediates.contains(v)) collection.intermediates -= v
    })
  }
}