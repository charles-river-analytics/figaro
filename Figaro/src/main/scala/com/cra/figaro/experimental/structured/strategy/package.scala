/*
 * package.scala
 * Definitions of expansion and solution strategies.
 *
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   March 1, 2015
 *
 * Copyright 2015 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */
package com.cra.figaro.experimental.structured

import com.cra.figaro.experimental.structured.solver.Solver

package object strategy {
  /**
   * A strategy takes a problem and does something useful, such as computing a solution.
   * Type-wise, a strategy simply does something with the problem.
   */
  type Strategy = Problem => Unit

  /**
   * Create a structured solution strategy. This strategy will recursively solve the subproblems before solving the top level problem
   * with a given solver.
   * @param solver the solver to use for the top level problem
   * @param recursingStrategy strategy for recursively solving the subproblems
   * @param rangeSizer strategy for choosing the size of the range of a component associated with an atomic element
   * @param bounds lower or upper bounds
   * @param parameterized flag indicating whether parameterized elements should use their MAP value or be treated as ordinary elements
   */
  def structuredSolver(solver: Solver, recursingStrategy: Strategy,
                       rangeSizer: RangeSizer = defaultRangeSizer, bounds: Bounds = Lower, parameterized: Boolean = false): Strategy =
    (problem: Problem) => {
      (new StructuredSolver(problem, solver, recursingStrategy, rangeSizer, bounds, parameterized)).execute()
    }

  /**
   * Create a structured solution strategy in which the selfsame strategy is used to recursively solve subproblems
   * @param solver the solver to use for every subproblem
   * @param rangeSizer strategy for choosing the size of the range of a component associated with an atomic element
   * @param bounds lower or upper bounds
   * @param parameterized flag indicating whether parameterized elements should use their MAP value or be treated as ordinary elements
   */
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
