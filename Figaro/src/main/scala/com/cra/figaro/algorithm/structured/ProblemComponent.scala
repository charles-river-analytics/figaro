/*
 * ProblemComponent.scala
 * One component of a problem to be solved, corresponding to a single element.
 *
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   March 1, 2015
 *
 * Copyright 2017 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.algorithm.structured

import com.cra.figaro.language._
import com.cra.figaro.algorithm.lazyfactored.ValueSet
import com.cra.figaro.algorithm.factored.factors._
import com.cra.figaro.algorithm.factored.factors.factory._

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
   * A problem component is fully enumerated if its range is complete. This also means that its range cannot contain
   * star. This is always false for components associated with elements that have infinite support.
   */
  var fullyEnumerated = false

  /**
   * A problem component is fully refined if any additional refinement cannot change its range or factors. One necessary
   * condition is to be fully enumerated. Additionally, expandable components must be fully expanded (i.e. have created
   * a subproblem for each parent value), and each subproblem must be fully refined. These conditions are necessary but
   * not always sufficient to be fully refined.
   */
  var fullyRefined = false

  /**
   *  Set the variable associated with this component to the given variable.
   */
  def setVariable(v: Variable[Value]) {
    _variable = v
    problem.collection.variableToComponent += v -> this
  }

  setVariable(new Variable(range))

  /**
   * Gets the constraint factors for this component. Returns the lower bound factors unless an Upper argument is provided.
   */
  def constraintFactors(bounds: Bounds = Lower): List[Factor[Double]] = {
    val upper = bounds == Upper
    ConstraintFactory.makeFactors(problem.collection, element, upper)
  }

  /**
   *  Generate the non-constraint factors based on the current range. For most elements, this just generates the factors
   *  in the usual way. For a chain, this does not include subproblem factors. The parameterized flag indicates whether
   *  parameterized elements should have special factors created that use the MAP values of their arguments. This
   *  defaults to false.
   */
  def nonConstraintFactors(parameterized: Boolean = false): List[Factor[Double]] = {
    Factory.makeFactors(problem.collection, element, parameterized).map(_.deDuplicate)
  }

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
   * If an argument is not in the component collection, we do not generate the argument, but instead assume its only value is *.
   * This doesn't change the range of any other element or expand any subproblems.
   * The range will include * based on argument ranges including * or any subproblem not being expanded.\
   *
   */
  def generateRange() {
    val newRange = Range(this)
    if ((newRange.hasStar ^ range.hasStar) || (newRange.regularValues != range.regularValues)) {
      range = newRange
      setVariable(new Variable(range))
    }
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

class ApplyComponent[Value](problem: Problem, val apply: Apply[Value]) extends ProblemComponent(problem, apply) {
  private var applyMap = scala.collection.mutable.Map[Any, Value]()
  def getMap() = applyMap
  def setMap(m: scala.collection.mutable.Map[Any, Value]) = applyMap = m

  override def generateRange() = {
    // Figaro semantics allow creation of elements in the function of an Apply. All such elements created must be added
    // to the same problem as the Apply.
    apply.universe.pushContext(apply)
    super.generateRange()
    apply.universe.popContext(apply)
    val contextElements = apply.directContextContents
    contextElements.foreach(problem.add(_))
    contextElements.clear()
  }
}
