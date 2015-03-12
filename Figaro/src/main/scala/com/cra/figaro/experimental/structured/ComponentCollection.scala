package com.cra.figaro.experimental.structured

import com.cra.figaro.language.{Element, Chain}
import com.cra.figaro.util.memo

/*
 * Every element exists in at most one component.
 * To create a new component for an element, you need to say what problem it belongs to.
 */
class ComponentCollection {
  private var components: Map[Element[_], ProblemComponent[_]] = Map()

  private var expansions: Map[(Function1[_, Element[_]], _), NestedProblem[_]] = Map()

  // Checks in the cache if an expansion exists and creates one if necessary
  private[structured] def expansion[P,V](function: Function1[P, Element[V]], parentValue: P): NestedProblem[V] = {
    expansions.get((function, parentValue)) match {
      case Some(p) => p.asInstanceOf[NestedProblem[V]]
      case None =>
        val result = new NestedProblem(this, function(parentValue))
        expansions += (function, parentValue) -> result
        result
    }
  }

  // Does the element have a component in this collection?
  def contains[T](element: Element[T]): Boolean =
    components.contains(element)

  // Get the component associated with this element in this collection.
  // Throws an exception if the element is not associated with any component.
  def apply[T](element: Element[T]): ProblemComponent[T] = components(element).asInstanceOf[ProblemComponent[T]]
  def apply[P,T](chain: Chain[P,T]): ChainComponent[P,T] = components(chain).asInstanceOf[ChainComponent[P,T]]

  private[structured] def add[T](element: Element[T], problem: Problem): ProblemComponent[T] = {
    if (contains(element)) throw new RuntimeException("Element already has a component")
    val component =
      element match {
      case chain: Chain[_,T] => new ChainComponent(problem, chain)
      case _ => new ProblemComponent(problem, element)
    }
    components += element -> component
    problem.components ::= component
    component
  }

}
