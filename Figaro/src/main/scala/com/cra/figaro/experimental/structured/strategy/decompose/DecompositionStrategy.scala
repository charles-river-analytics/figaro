package com.cra.figaro.experimental.structured.strategy.decompose

import com.cra.figaro.experimental.structured._
import com.cra.figaro.experimental.structured.solver.Solver
import com.cra.figaro.language.Element

private[figaro] abstract class DecompositionStrategy(val problem: Problem, val solver: Solver, val recursingStrategy: Strategy, val rangeSizer: RangeSizer,
  val bounds: Bounds, val parameterized: Boolean) {

  def execute(): Unit

  private def checkArg[T](element: Element[T]): ProblemComponent[T] = {
    if (problem.collection.contains(element)) problem.collection(element)
    else problem.add(element)
  }

  private def process(comp: ProblemComponent[_]) {
    comp.generateRange(rangeSizer(comp))
    comp.makeNonConstraintFactors(parameterized)
    comp.makeConstraintFactors(bounds)
  }

}