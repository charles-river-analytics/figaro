/*
 * ComponentCollection.scala
 * A data structure that holds all the problem components used in a top-level problem and its subproblems.
 *
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   March 1, 2015
 *
 * Copyright 2015 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.algorithm.structured

import com.cra.figaro.language._
import com.cra.figaro.library.collection.MakeArray
import scala.collection.mutable.Map
import com.cra.figaro.algorithm.factored.factors._


/**
 * A collection of problem components. This data structure manages all the components being used in the solution of a top-level
 * problem and its nested subproblems.
 */

/*
 * Every element exists in at most one component.
 * To create a new component for an element, you need to say what problem it belongs to.
 */
class ComponentCollection {
  /**
   * Maps a variable to the parents needed for creating blocks using Gibbs sampling.
   * TODO: test if this variable causes memory leaks.
   */
  val variableParents: Map[Variable[_], Set[Variable[_]]] = Map().withDefaultValue(Set())

  /** All the components in the collection, each associated with an element. */
  val components: Map[Element[_], ProblemComponent[_]] = Map()

  /**
   *  Intermediate variables defined during the construction of factors.
   *  These are not associated with any element or component and are to be eliminated wherever they appear.
   */
  var intermediates: Set[Variable[_]] = Set()

  /**
   * A map from a function and parent value to the associated subproblem.
   */
  val expansions: Map[(Function1[_, Element[_]], _), NestedProblem[_]] = Map()

  /**
   *  Get the nested subproblem associated with a particular function and parent value.
   *  Checks in the cache if an expansion exists and creates one if necessary.
   */
  private[structured] def expansion[P, V](component: ExpandableComponent[P, V], function: Function1[P, Element[V]], parentValue: P): NestedProblem[V] = {
    expansions.get((function, parentValue)) match {
      case Some(p) =>
        p.asInstanceOf[NestedProblem[V]]
      case None =>
        val result = new NestedProblem(this, component.expandFunction(parentValue))
        expansions += (function, parentValue) -> result
        result
    }
  }

  /**
   *  Returns the problem component associated with a particular variable.
   *  Not valid for intermediate variables.
   */
  val variableToComponent: Map[Variable[_], ProblemComponent[_]] = Map()

  /** Does the element have a component in this collection? */
  def contains[T](element: Element[T]): Boolean =
    components.contains(element)

  /**
   *  Get the component associated with this element in this collection.
   *  Throws an exception if the element is not associated with any component.
   */
  def apply[T](element: Element[T]): ProblemComponent[T] = components(element).asInstanceOf[ProblemComponent[T]]
  /**
   *  Get the component associated with this element in this collection.
   *  Throws an exception if the element is not associated with any component.
   */
  def apply[P, T](chain: Chain[P, T]): ChainComponent[P, T] = components(chain).asInstanceOf[ChainComponent[P, T]]
  
    /**
   *  Get the component associated with this element in this collection.
   *  Throws an exception if the element is not associated with any component.
   */
  def apply[T](apply: Apply[T]): ApplyComponent[T] = components(apply).asInstanceOf[ApplyComponent[T]]
  
  /**
   *  Get the component associated with this element in this collection.
   *  Throws an exception if the element is not associated with any component.
   */
  def apply[T](makeArray: MakeArray[T]): MakeArrayComponent[T] = components(makeArray).asInstanceOf[MakeArrayComponent[T]]

  /**
   * Add a component for the given element in the given problem to the component collection and return the component.
   */
  private[structured] def add[T](element: Element[T], problem: Problem): ProblemComponent[T] = {
    if (problem.collection.contains(element)) {
      val component = problem.collection(element)
      if (component.problem != problem) throw new RuntimeException("Trying to add a component to a different problem")
      component
    }
    else {
      val component: ProblemComponent[T] =
        element match {
          case chain: Chain[_, T] => new ChainComponent(problem, chain)
          case makeArray: MakeArray[_] => new MakeArrayComponent(problem, makeArray)
          case apply: Apply[_] => new ApplyComponent(problem, apply)
          case _ => new ProblemComponent(problem, element)
        }
      components += element -> component
      problem.components ::= component
      component
    }
  }

  private[structured] def remove[T](element: Element[T]) {
    components -= element
  }
}
