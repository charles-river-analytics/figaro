package com.cra.figaro.experimental.structured

import com.cra.figaro.algorithm.factored.factors.Factor
import com.cra.figaro.language.Element
import com.cra.figaro.algorithm.factored.factors.Variable

/*
 * A Problem defines an inference problem to be solved.
 * It includes a set of components directly contained in the problem.
 * These components might have nested subproblems.
 * It also refers to global components that are outside of this problem.
 * The targets are elements that appear in this problem that are visible outside.
 * They might be newly defined in this problem or they might be defined previously, but either way, they should not be eliminated.
 */
class Problem(val collection: ComponentCollection, targets: List[Element[_]] = List()) {
  // Components outside of this problem that appear in the solution to this problem.
  var globals: Set[ProblemComponent[_]] = Set()

  // Components directly defined in this problem.
  var components: List[ProblemComponent[_]] = List()

  // Factors over globals
  var solution: List[Factor[Double]] = List()

  targets.foreach(target => if (!collection.contains(target)) add(target))

  // Add a component for the given element to this problem.
  def add[T](element: Element[T]): ProblemComponent[T] = collection.add(element, this)

  // Determines if a variable is internal to this problem and should be eliminated
  def internal(variable: Variable[_]): Boolean = {
    collection.intermediates.contains(variable) || {
      val component = collection.variableToComponent(variable)
      contains(component.problem) & !targets.contains(component.element)
    }
  }

  // Determines if this problem contains the given problem.
  // Any variables in the contained problem should also be eliminated when this problem is solved.
  def contains(otherProblem: Problem): Boolean = {
    def componentContains(component: ProblemComponent[_]): Boolean = {
      component match {
        case c: ChainComponent[_,_] => c.subproblems.values.exists(_.contains(otherProblem))
        case _ => false
      }
    }
    otherProblem == this || components.exists(componentContains(_))
  }


  // Solve the problem defined by the component's current factors.
  // This will also set the globals accordingly.
  // All components in this problem and nested subproblems should be eliminated in the solution.
  def solve(algorithm: solver.Solver, bounds: Bounds = Lower) {
    val targetComponents = targets.map(collection(_))
    val allFactors = components.flatMap(c => c.nonConstraintFactors ::: c.constraintFactors(bounds))
    val allVariables = (Set[Variable[_]]() /: allFactors)(_ ++ _.variables)
    val (toEliminate, toPreserve) = allVariables.partition(internal(_))
    globals = toPreserve.map(collection.variableToComponent(_))
    solution = algorithm(toEliminate, toPreserve, allFactors)
    toEliminate.foreach((v: Variable[_]) => {
      if (collection.intermediates.contains(v)) collection.intermediates -= v
      else collection.components -= collection.variableToComponent(v).element
    })
  }

  // Produce a single weighted sample of all the elements in this problem.
  def sample(): (Map[Element[_], _], Double) = {
    (Map(), 1.0)
  }
}

class NestedProblem[T](collection: ComponentCollection, val target: Element[T]) extends Problem(collection, List(target))
