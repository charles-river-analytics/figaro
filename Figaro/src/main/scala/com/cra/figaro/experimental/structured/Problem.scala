package com.cra.figaro.experimental.structured

import com.cra.figaro.algorithm.factored.factors.Factor
import com.cra.figaro.language.Element

/*
 * A Problem defines an inference problem to be solved.
 * It includes a set of components directly contained in the problem.
 * These components might have nested subproblems.
 * It also refers to global components that are outside of this problem.
 */
class Problem(val collection: ComponentCollection, targets: List[Element[_]] = List()) {
  // Components outside of this problem that appear in the solution to this problem.
  var globals: List[ProblemComponent[_]] = List()

  // Components directly defined in this problem.
  var components: List[ProblemComponent[_]] = List()

  // Factors over globals
  var solution: List[Factor[Double]] = List()

  targets.foreach(add(_))

  // Add a component for the given element to this problem.
  def add[T](element: Element[T]): ProblemComponent[T] = collection.add(element, this)

  // Solve the problem defined by the component's current factors.
  // This will also set the globals accordingly.
  // All components in this problem and nested subproblems should be eliminated in the solution.
  def solve(algorithm: solver.Solver, bounds: Bounds = Lower) {
    val targetComponents = targets.map(collection(_))
    val toEliminate = components.filter(!targetComponents.contains(_)).map(_.variable)
    val allFactors = components.flatMap(c => c.nonConstraintFactors ::: c.constraintFactors(bounds))
    val result = algorithm(toEliminate, allFactors)
    globals = result._1.map(collection.variableToComponent(_))
    solution = result._2
  }

  // Produce a single weighted sample of all the elements in this problem.
  def sample(): (Map[Element[_], _], Double) = {
    (Map(), 1.0)
  }
}

class NestedProblem[T](collection: ComponentCollection, val target: Element[T]) extends Problem(collection, List(target))
