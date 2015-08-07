/*
 * DecompositionStrategy.scala
 * Base class for strategies that decompose a problem
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
import com.cra.figaro.experimental.structured.strategy.solve.SolvingStrategy

/**
 * An abstract class that defines how to decompose problems into nested problems.
 * @param problem The problem to decompose
 * @param solvingStrategy The solving strategy used to solve this problem
 * @param rangeSizer The method to determine the range of components
 * @param bounds Lower or Upper bounds
 * @param parameterized Indicates if this problem parameterized
 */
private[figaro] abstract class DecompositionStrategy(val problem: Problem, val solvingStrategy: SolvingStrategy, val rangeSizer: RangeSizer,
  val bounds: Bounds, val parameterized: Boolean) {

  /**
   * Executes (i.e., solves) the problem
   */
  def execute(): Unit
  
  /**
   * Decomposes a nested problem
   */
  def decompose(nestedProblem: Problem, done: Set[ProblemComponent[_]]): Unit

  protected def checkArg[T](element: Element[T]): ProblemComponent[T] = {
    if (problem.collection.contains(element)) problem.collection(element)
    else problem.add(element)
  }

  /*
   * Default process for an element. Generate the range and make the factors
   */
  protected def process(comp: ProblemComponent[_]) {
    comp.generateRange(rangeSizer(comp))
    comp.makeNonConstraintFactors(parameterized)
    comp.makeConstraintFactors(bounds)
  }

}