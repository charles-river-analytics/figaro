package com.cra.figaro.experimental.structured

import com.cra.figaro.language.{Element, Chain}
import com.cra.figaro.algorithm.lazyfactored.ValueSet
import com.cra.figaro.algorithm.factored.factors.{Factor, Factory}
import com.cra.figaro.algorithm.factored.factors.factory.StarFactory
import com.cra.figaro.algorithm.factored.factors.Variable
import com.cra.figaro.library.collection.MakeArray
import com.cra.figaro.library.collection.FixedSizeArray

/*
 * A component of a problem, created for a specific element.
 */
class ProblemComponent[Value](val problem: Problem, val element: Element[Value]) {
  // The current range of the element. May grow or change over time.
  var range: ValueSet[Value] = ValueSet.withStar(Set())

  // The current variable representing this component in factors.
  // This is set automatically when the range is updated.
  var variable: Variable[Value] = new Variable(range)

  // Factors resulting from conditions and constraints on this element.
  // These should be updated when the range changes but otherwise should be left alone.
  var constraintLower: List[Factor[Double]] = List()
  var constraintUpper: List[Factor[Double]] = List()

  // All other factors resulting from the definition of this element. For many element classes,
  // these factors will be generated directly in the usual way.
  // For chains, they will include the solutions of subproblems.
  var nonConstraintFactors: List[Factor[Double]] = List()

  // The current belief about this component, used for belief propagation.
  var belief: Factor[Double] = _ //StarFactory.makeStarFactor(element)(0)

  // Components that share a factor with this component. Used for belief propagation.
  var neighbors: List[ProblemComponent[_]] = List()

  // Messages queued from the neighbors to be set to the incoming messages on synchronization.
  var queuedMessages: Map[ProblemComponent[_], List[Factor[Double]]] = Map()

  // Messages received from the neighbors.
  var incomingMessages: Map[ProblemComponent[_], List[Factor[Double]]] = Map()

  // Generate a range of values for this component. Also sets the variable for this component.
  // The optional argument is the number of values to include in the range.
  // This argument is only used for atomic elements.
  // If an argument is not in the component collection, we do not generate the argument, but instead assume its only value is *.
  // This doesn't change the range of any other element or expand any subproblems.
  // The range will include * based on argument ranges including * or any subproblem not being expanded.
  def generateRange(numValues: Int = 10) {
    range = Range(this, numValues)
    variable = new Variable(range)
  }

  // Generate the constraint factors based on the current range.
  // Bounds specifies whether these should be created for computing lower or upper bounds
  def makeConstraintFactors(bounds: Bounds) {

  }

  // For most elements, this just generates the factors in the usual way.
  // For a chain, this takes the current solution to the subproblems, which are lists of factors over this and other components.
  def makeNonConstraintFactors() {
    nonConstraintFactors = factory.Factory.makeFactors(problem.collection, element)
  }

  // Compute current beliefs about this component based on the queued messages and factors of this component.
  // Also sets the incoming messages to the queued messages.
  def computeBeliefs() {

  }

  // Send messages to the neighbors based on current beliefs and incoming messages.
  def sendMessages() {

  }
}

abstract class ExpandableComponent[ParentValue, Value](problem: Problem, parent: Element[ParentValue], element: Element[Value])
extends ProblemComponent(problem, element)
{
  // Creates subproblems for each of the result elements resulting from different parent values,
  // based on the current ranges of the parent values.
  def expand() {
    if (problem.collection.contains(parent)) {
      for { parentValue <- problem.collection(parent).range.regularValues } { expand (parentValue) }
    }
  }

  def expand(parentValue: ParentValue): Unit
}

class ChainComponent[ParentValue, Value](problem: Problem, val chain: Chain[ParentValue, Value])
extends ExpandableComponent[ParentValue, Value](problem, chain.parent, chain) {
  // The subproblems represent nested problems from chains.
  // They are mapped from parent values.
  var subproblems: Map[ParentValue, NestedProblem[Value]] = Map()

  // Create a subproblem for a particular parent value.
  // Memoized.
  def expand(parentValue: ParentValue) {
    val subproblem = problem.collection.expansion(chain.chainFunction, parentValue)
    subproblems += parentValue -> subproblem
  }

  // Raise the given subproblem into this problem
  def raise(subproblem: NestedProblem[Value]) {

  }

  // Raise all subproblems into this problem
  def raise() { subproblems.values.foreach(raise(_)) }
}

class MakeArrayComponent[Value](problem: Problem, val makeArray: MakeArray[Value])
extends ExpandableComponent[Int, FixedSizeArray[Value]](problem, makeArray.numItems, makeArray) {
  var maxExpanded = 0

  def expand(n: Int) {
    // Make sure the first n items are added to the component collection.
    // Any newly added items will be added to this problem.
    for { i <- maxExpanded until n } {
      val item = makeArray.items(i)
      if (!problem.collection.contains(item)) problem.add(item)
    }
    maxExpanded = maxExpanded.max(n)
  }

  override def expand {
    if (problem.collection.contains(makeArray.numItems)) {
      expand(problem.collection(makeArray.numItems).range.regularValues.max)
    }
  }
}
