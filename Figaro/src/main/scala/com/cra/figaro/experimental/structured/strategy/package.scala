package com.cra.figaro.experimental.structured

import com.cra.figaro.experimental.structured.solver.Solver

package object strategy {
  /**
   * A strategy takes a problem and does something useful, such as computing a solution.
   * Type-wise, a strategy simply does something with the problem.
   */
  type Strategy = Problem => Unit

  def structuredSolver(solver: Solver, recursingStrategy: Strategy,
                       rangeSizer: RangeSizer = defaultRangeSizer, bounds: Bounds = Lower, parameterized: Boolean = false): Strategy =
    (problem: Problem) => {
      (new StructuredSolver(problem, solver, recursingStrategy, rangeSizer, bounds, parameterized)).execute()
    }

  def recursiveSolver(solver: Solver, rangeSizer: RangeSizer = defaultRangeSizer, bounds: Bounds = Lower, parameterized: Boolean = false): Strategy =
    (problem: Problem) => {
      def recurse(problem: Problem) = recursiveSolver(solver, rangeSizer, bounds, parameterized)(problem)
        (new StructuredSolver(problem, solver, recurse, rangeSizer, bounds, parameterized)).execute()
    }

  /**
   * A range sizer chooses a size of range for components corresponding to atomic elements.
   */
  type RangeSizer = ProblemComponent[_] => Int

  def defaultRangeSizer(pc: ProblemComponent[_]) = Int.MaxValue
}
