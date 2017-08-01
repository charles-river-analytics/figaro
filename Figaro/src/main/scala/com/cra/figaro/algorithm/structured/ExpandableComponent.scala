/*
 * ExpandableComponent.scala
 * Problem components that expand into subproblems.
 *
 * Created By:      William Kretschmer (kretsch@mit.edu)
 * Creation Date:   Mar 17, 2017
 *
 * Copyright 2017 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */
package com.cra.figaro.algorithm.structured

import com.cra.figaro.algorithm.factored.factors.Variable
import com.cra.figaro.language.{Chain, Constant, Element}
import com.cra.figaro.library.collection.{FixedSizeArray, MakeArray}

/**
 * A problem component that provides an expand method.
 *
 * @param parent the element according to whose values this component should be expanded
 * @param element the element to which this component corresponds
 */
abstract class ExpandableComponent[ParentValue, Value](problem: Problem, parent: Element[ParentValue], element: Element[Value])
  extends ProblemComponent(problem, element) {
  /**
   * Expand for all values of the parent that were not previously expanded, based on the current range of the parent.
   */
  def expand() {
    if (problem.collection.contains(parent)) {
      val parentValues = problem.collection(parent).range.regularValues
      for (parentValue <- parentValues if !subproblems.contains(parentValue)) expand(parentValue)
    }
  }

  /** Expand for a particular parent value. */
  def expand(parentValue: ParentValue): Unit

  val expandFunction: (ParentValue) => Element[Value]

  /**
   *  The subproblems nested inside this expandable component.
   *  They are created for particular parent values.
   */
  var subproblems: Map[ParentValue, NestedProblem[Value]] = Map()
}

/**
 * A problem component created for a chain element.
 */
class ChainComponent[ParentValue, Value](problem: Problem, val chain: Chain[ParentValue, Value])
  extends ExpandableComponent[ParentValue, Value](problem, chain.parent, chain) {

  val elementsCreated: scala.collection.mutable.Set[Element[_]] = scala.collection.mutable.Set() ++ chain.universe.contextContents(chain)

  val expandFunction = chain.get _

  /**
   *  The subproblems are defined in terms of formal variables. We need to create actual variables for each of the
   *  subproblems to replace the formal variables with in their solutions. This stores the variables used the last time
   *  factors were created for this component.
   */
  var actualSubproblemVariables: Map[ParentValue, Variable[Value]] = Map()

  /**
   *  Create a subproblem for a particular parent value.
   *  Memoized.
   */
  def expand(parentValue: ParentValue) {
    val subproblem = problem.collection.expansion(this, chain.chainFunction, parentValue)
    val chainContextElements = chain.universe.contextContents(chain)
    val remainingElements = chainContextElements.filter(!elementsCreated.contains(_))
    remainingElements.foreach(subproblem.add(_))
    elementsCreated ++= remainingElements
    subproblems += parentValue -> subproblem
  }

  /*
   * Tests if we can use the optimized Chain factor. If all the subproblems have been eliminated completely and use no
   * globals, we can use the new chain method.
   */
  private[figaro] def allSubproblemsEliminatedCompletely: Boolean = {
    // Every subproblem must be completely eliminated
    subproblems.values.forall { subproblem =>
      // The subproblem must be solved and have no globals in its solution
      subproblem.solved && {
        val factors = subproblem.solution
        val vars = factors.flatMap(_.variables)
        val comps = vars.map(problem.collection.variableToComponent(_))
        // The first part checks that the subproblem target is not global
        // The second part checks that the factors contain no additional global variables
        problem.collection(subproblem.target).problem == subproblem && comps.forall(subproblem.components.contains)
      }
    }
  }

  /*
  def raiseSubproblemSolution(parentValue: ParentValue, subproblem: NestedProblem[Value]): Unit = {
    val factors = for {
      factor <- subproblem.solution
    } yield Factory.replaceVariable(factor, problem.collection(subproblem.target).variable, actualSubproblemVariables(parentValue))
    nonConstraintFactors = factors ::: nonConstraintFactors
  }

  // Raise the given subproblem into this problem. Note that factors for the chain must have been created already
  // This probably needs some more thought!
  // This assumes that components in nested problems do not have evidence on them, and therefore we don't need to raise
  // the constraint factors.
  def raise(parentValue: ParentValue): Unit = {

    if (subproblems.contains(parentValue) && !subproblems(parentValue).solved) {

      val subproblem = subproblems(parentValue)
      val comp = subproblem.collection(subproblem.target)
      val NCF = subproblem.components.flatMap(c => c.nonConstraintFactors.map(Factory.replaceVariable(_, comp.variable, actualSubproblemVariables(parentValue))))

      nonConstraintFactors = nonConstraintFactors ::: NCF
    }
  }

  // Raise all subproblems into this problem
  def raise() { subproblems.foreach(sp => raise(sp._1)) }
  */

}

/**
 * A problem component for a MakeArray element.
 */
class MakeArrayComponent[Value](problem: Problem, val makeArray: MakeArray[Value])
  extends ExpandableComponent[Int, FixedSizeArray[Value]](problem, makeArray.numItems, makeArray) {
  /** The maximum number of items expanded so far. */
  var maxExpanded = 0

  // This isn't really needed in MakeArray since the main expand function won't call this.
  val expandFunction = (i: Int) => Constant(makeArray.makeValues(i).regularValues.head)

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
  override def expand() {
    if (problem.collection.contains(makeArray.numItems)) {
      expand(problem.collection(makeArray.numItems).range.regularValues.max)
    }
  }
}
