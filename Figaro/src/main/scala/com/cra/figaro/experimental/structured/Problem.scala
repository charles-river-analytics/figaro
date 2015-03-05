package com.cra.figaro.experimental.structured

import com.cra.figaro.algorithm.factored.factors.Factor
import com.cra.figaro.language.Element

/*
 * A Problem defines an inference problem to be solved.
 * It includes a set of components directly contained in the problem.
 * These components might have nested subproblems.
 * It also refers to global components that are outside of this problem.
 */
class Problem(val collection: ComponentCollection) {
  // Components outside of this problem that appear in the solution to this problem.
  var globals: List[ProblemComponent[_]] = List()

  // Components directly defined in this problem.
  var components: List[ProblemComponent[_]] = List()

  // Factors over globals
  var solution: List[Factor[Double]] = List()

  // Add a component for the given element to this problem.
  def add[T](element: Element[T]): ProblemComponent[T] = collection.add(element, this)

  // Solve the problem defined by the component's current factors.
  // This will also set the globals accordingly.
  // All components in this problem and nested subproblems should be eliminated in the solution.
  def solve() {

  }

  // Produce a single weighted sample of all the elements in this problem.
  def sample(): (Map[Element[_], _], Double) = {
    (Map(), 1.0)
  }
}

/*
 * A problem defined inside a chain. The target is the element created by the chain function and represents
 * the value of the chain in this subproblem.
 */
class NestedProblem[T](collection: ComponentCollection, val target: Element[T]) extends Problem(collection) {
  add(target)
}
