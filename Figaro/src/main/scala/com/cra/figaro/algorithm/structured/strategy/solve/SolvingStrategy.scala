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

/**
 * A solving strategy solves an inference problem after a series of refinements have been made. This involves solving
 * subproblems and collecting their factors. The solving strategy may then choose to eliminate variables that do not
 * belong in the solution, or defer elimination to a higher-level problem.
 */
private[figaro] abstract class SolvingStrategy(problem: Problem) {

  /**
   * Process all subproblems for the given component by raising their factors or solutions.
   * @param chainComp Component to process.
   * @param bounds Bounds for constraint factors.
   */
  def raiseSubproblems[ParentValue, Value](chainComp: ChainComponent[ParentValue, Value], bounds: Bounds): Unit

  /**
   * Decide whether to raise factors, or perform elimination on factors at this level.
   * @return False if variables should be eliminated, true if factors should be raised up a level without elimination.
   * In general, this should return false for a top-level problem, since otherwise no elimination will be applied.
   */
  def decideToRaise(): Boolean

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
   * @param bounds Bounds for constraint factors.
   */
  def execute(bounds: Bounds): Unit = {
    // Raise factors from all subproblems before deciding to eliminate and collecting factors.
    problem.components.foreach {
      case chainComp: ChainComponent[_, _] => raiseSubproblems(chainComp, bounds)
      case _ =>
    }

    if(!decideToRaise()) {
      val allFactors = problem.components.flatMap(c => c.constraintFactors(bounds) ::: c.nonConstraintFactors)
      val allVariables = (Set[Variable[_]]() /: allFactors)(_ ++ _.variables)
      val (toEliminate, toPreserve) = allVariables.partition(problem.internal)
      val collection = problem.collection
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
}