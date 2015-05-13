/*
 * ProblemComponent.scala
 * One component of a problem to be solved, corresponding to a single element.
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

import com.cra.figaro.language.{Element, Chain}
import com.cra.figaro.algorithm.lazyfactored.ValueSet
import com.cra.figaro.algorithm.factored.factors.Factor
import com.cra.figaro.algorithm.factored.factors.Variable
import com.cra.figaro.library.collection.MakeArray
import com.cra.figaro.library.collection.FixedSizeArray
import com.cra.figaro.algorithm.factored.ParticleGenerator
import com.cra.figaro.experimental.structured.factory.{Factory, ConstraintFactory}

/*
/** A component of a problem, created for a specific element. */
 */
class ProblemComponent[Value](val problem: Problem, val element: Element[Value]) {
  /** The current range of the element. May grow or change over time. */
  var range: ValueSet[Value] = ValueSet.withStar(Set())

  private var _variable: Variable[Value] = _

  /**
   *  The current variable representing this component in factors.
   *  This is set automatically when the range is updated.
   */
  def variable = _variable

  /**
   *  Set the variable associated with this component to the given variable.
   */
  def setVariable(v: Variable[Value]) {
    _variable = v
    problem.collection.variableToComponent += v -> this
  }

  setVariable(new Variable(range))

  /**
   *  Lower bound factors resulting from conditions and constraints on this element.
   *  These should be updated when the range changes but otherwise should be left alone.
   */
  var constraintLower: List[Factor[Double]] = List()
  /**
   *  Upper bound factors resulting from conditions and constraints on this element.
   *  These should be updated when the range changes but otherwise should be left alone.
   */
  var constraintUpper: List[Factor[Double]] = List()

  /**
   * Gets the constraint factors for this component. Returns the lower bound factors unless an Upper argument is provided.
   */
  def constraintFactors(bounds: Bounds = Lower) = if (bounds == Upper) constraintUpper else constraintLower

  /**
   *  All non-constraint factors resulting from the definition of this element. For many element classes,
   *  these factors will be generated directly in the usual way.
   *  For chains, they will include the solutions of subproblems.
   */
  var nonConstraintFactors: List[Factor[Double]] = List()

/*
  // The current belief about this component, used for belief propagation.
  var belief: Factor[Double] = _ //StarFactory.makeStarFactor(element)(0)

  // Components that share a factor with this component. Used for belief propagation.
  var neighbors: List[ProblemComponent[_]] = List()

  // Messages queued from the neighbors to be set to the incoming messages on synchronization.
  var queuedMessages: Map[ProblemComponent[_], List[Factor[Double]]] = Map()

  // Messages received from the neighbors.
  var incomingMessages: Map[ProblemComponent[_], List[Factor[Double]]] = Map()
*/

  /**
   *  Generate a range of values for this component. Also sets the variable for this component.
   * The optional argument is the number of values to include in the range.
   * This argument is only used for atomic elements.
   * If an argument is not in the component collection, we do not generate the argument, but instead assume its only value is *.
   * This doesn't change the range of any other element or expand any subproblems.
   * The range will include * based on argument ranges including * or any subproblem not being expanded.\
   *
   */
  def generateRange(numValues: Int = ParticleGenerator.defaultTotalSamples) {
    range = Range(this, numValues)
    setVariable(new Variable(range))
  }

  /**
   *  Generate the constraint factors based on the current range.
   *  Bounds specifies whether these should be created for computing lower or upper bounds.
   */
  def makeConstraintFactors(bounds: Bounds = Lower) {
    if (bounds == Upper) constraintUpper = ConstraintFactory.makeFactors(problem.collection, element, true)
    else constraintLower = ConstraintFactory.makeFactors(problem.collection, element, false)
  }

  /**
   *  Generate the non-constraint factors based on the current range.
   *  For most elements, this just generates the factors in the usual way.
   *  For a chain, this takes the current solution to the subproblems, which are lists of factors over this and other components.
   *  The parameterized flag indicates whether parameterized elements should have special factors created that use the MAP values of their arguments.
   */
  def makeNonConstraintFactors(parameterized: Boolean = false) {
    nonConstraintFactors = factory.Factory.makeFactors(problem.collection, element, parameterized).map(_.deDuplicate)
  }

  /*
  // Compute current beliefs about this component based on the queued messages and factors of this component.
  // Also sets the incoming messages to the queued messages.
  def computeBeliefs() {

  }

  // Send messages to the neighbors based on current beliefs and incoming messages.
  def sendMessages() {

  }
  *
  */
}

/**
 * A problem component that provides an expand method.
 * @param parent the element according to whose values this component should be expanded
 * @param element the element to which this component corresponds
 */
abstract class ExpandableComponent[ParentValue, Value](problem: Problem, parent: Element[ParentValue], element: Element[Value])
extends ProblemComponent(problem, element)
{
  /**
   * Expand for all values of the parent, based on the current range of the parent.
   */
  def expand() {
    if (problem.collection.contains(parent)) {
      for { parentValue <- problem.collection(parent).range.regularValues } { expand (parentValue) }
    }
  }

  /** Expand for a particular parent value. */
  def expand(parentValue: ParentValue): Unit
}

/**
 * A problem component created for a chain element.
 */
class ChainComponent[ParentValue, Value](problem: Problem, val chain: Chain[ParentValue, Value])
extends ExpandableComponent[ParentValue, Value](problem, chain.parent, chain) {
  /**
   *  The subproblems represent nested problems from chains.
   *  They are created for particular parent values.
   */
  var subproblems: Map[ParentValue, NestedProblem[Value]] = Map()

  /**
   *  The subproblems are defined in terms of formal variables.
   *  We need to create actual variables for each of the subproblems to replace the formal variables with in their solutions.
   */
  var actualSubproblemVariables: Map[ParentValue, Variable[Value]] = Map()

  /**
   *  Create a subproblem for a particular parent value.
   *  Memoized.
   */
  def expand(parentValue: ParentValue) {
    val subproblem = problem.collection.expansion(chain.chainFunction, parentValue)
    subproblems += parentValue -> subproblem
  }

  /**
   * Make the non-constraint factors for this component by using the solutions to the subproblems.
   */
  override def makeNonConstraintFactors(parameterized: Boolean = false) {
    super.makeNonConstraintFactors(parameterized)
    val subproblemFactors =
      for {
        (parentValue, subproblem) <- subproblems
        factor <- subproblem.solution
      } yield Factory.replaceVariable(factor, problem.collection(subproblem.target).variable, actualSubproblemVariables(parentValue))
    nonConstraintFactors = subproblemFactors.toList ::: nonConstraintFactors
  }

  /*
  // Raise the given subproblem into this problem
  def raise(subproblem: NestedProblem[Value]) {

  }

  // Raise all subproblems into this problem
  def raise() { subproblems.values.foreach(raise(_)) }
  *
  */
}

/**
 * A problem component for a MakeArray element.
 */
class MakeArrayComponent[Value](problem: Problem, val makeArray: MakeArray[Value])
extends ExpandableComponent[Int, FixedSizeArray[Value]](problem, makeArray.numItems, makeArray) {
  /** The maximum number of items expanded so far. */
  var maxExpanded = 0

  /**
   * Ensure that the given number of items is expanded.
   */
  def expand(n: Int) {
    // Make sure the first n items are added to the component collection.
    // Any newly added items will be added to this problem.
    for { i <- maxExpanded until n } {
      val item = makeArray.items(i)
      if (!problem.collection.contains(item)) problem.add(item)
    }
    maxExpanded = maxExpanded.max(n)
  }

  /**
   * Expand all the potential items according to the maximum value in the range of the MakeArray's number of items.
   */
  override def expand {
    if (problem.collection.contains(makeArray.numItems)) {
      expand(problem.collection(makeArray.numItems).range.regularValues.max)
    }
  }
}
