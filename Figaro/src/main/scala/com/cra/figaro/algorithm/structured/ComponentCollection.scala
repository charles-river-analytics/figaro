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

import com.cra.figaro.algorithm.factored.ParticleGenerator
import com.cra.figaro.language._
import com.cra.figaro.library.collection.MakeArray
import com.cra.figaro.util

import scala.collection.mutable.Map
import com.cra.figaro.algorithm.factored.factors._
import com.cra.figaro.algorithm.structured.strategy.range.RangingStrategy

import scala.annotation.tailrec
import scala.collection.mutable.HashMap
/**
* To speed up factor creation time, it's necessary to override the hashcode of component collections.
*/
object ComponentHash {
  var hashCodeState = 10
  def nextCode: Int = {
    hashCodeState += 1
    hashCodeState
  }
}

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
   * Ranging strategy for atomic components. Initially uses the default non-lazy method that samples infinite atomics.
   */
  var rangingStrategy: RangingStrategy = RangingStrategy.default(ParticleGenerator.defaultNumSamplesFromAtomics)

 /** Indicates whether to create chain factors by decomposing the chain into several factors or a single factor
  * This defaults to false since all the existing code a decomposition
  */
  var useSingleChainFactor = false

  /**
   * Maps a variable to the parents needed for creating blocks using Gibbs sampling.
   * TODO: test if this variable causes memory leaks.
   */
  val variableParents: Map[Variable[_], Set[Variable[_]]] = Map().withDefaultValue(Set())

  /** All the components in the collection, each associated with an element. */
  val components: Map[Element[_], ProblemComponent[_]] = new HashMap[Element[_], ProblemComponent[_]]() {
    override val hashCode = ComponentHash.nextCode
  }

  /**
   *  Intermediate variables defined during the construction of factors.
   *  These are not associated with any element or component and are to be eliminated wherever they appear.
   */
  var intermediates: Set[Variable[_]] = Set()

  /**
   * A map from a function and parent value to the sequence of associated subproblems. A function and parent value are
   * only expanded more than once if this particular expansion calls itself recursively, indirectly or directly. The
   * sequence of subproblems is sorted by depth of expansion. So, the head of the sequence is the first subproblem to
   * have been expanded. Furthermore, this gives rise to an invariant that a subproblem in the sequence uses all later
   * subproblems in the sequence.
   */
  val expansions: Map[(Function1[_, Element[_]], _), IndexedSeq[NestedProblem[_]]] = Map()

  /**
   * A map from a subproblem to the set of expandable components that use it.
   */
  val expandableComponents: Map[NestedProblem[_], Set[ExpandableComponent[_, _]]] = Map()

  /**
   * Tests if the adding the subproblem to the given component would create a cycle in the subproblem graph.
   */
  private def createsCycle(nestedProblem: NestedProblem[_], component: ExpandableComponent[_, _]): Boolean = {
    // TODO consider using a dedicated incremental cycle detection data structure and algorithm for improved efficiency
    // TODO consider a different version of ComponentCollection that avoids this computation for non-lazy algorithms
    // For now, the current implementation just does a breadth first search from the component problem to see if there
    // exists a path from nestedProblem to component via the problem graph
    component.problem match {
      case componentProblem: NestedProblem[_] =>
        // Does nestedProblem ever use component.problem?
        // Test by searching backwards from component.problem through the expandable components that use it
        @tailrec def bfs(problems: Set[NestedProblem[_]]): Boolean = {
          if(problems.contains(nestedProblem)) true
          else if(problems.isEmpty) false
          else {
            // Map current problems to the set of problems that use them
            val next = problems.flatMap(expandableComponents(_)).map(_.problem).collect{ case np: NestedProblem[_] => np }
            bfs(next)
          }
        }

        bfs(Set(componentProblem))
      case _ => false
    }
  }

  /**
   *  Get the nested subproblem associated with a particular function and parent value. Checks in the cache if there is
   *  an expansion that does not create a cyclic dependency. If such an expansion exists, it is returned. Otherwise, a
   *  new expansion is created and stored in the collection.
   */
  private[structured] def expansion[P, V](component: ExpandableComponent[P, V], function: Function1[P, Element[V]], parentValue: P): NestedProblem[V] = {
    val seq = expansions.getOrElse((function, parentValue), Vector())
    // Look for the first copy of the appropriate subproblem that does not produce a cycle
    // Note: it is unclear if/when this greedy solution is optimal. However, it guarantees that the number of copies of
    // a subproblem is at most linear in the depth of expansion (as opposed to the possible exponential growth that
    // results from using no memoization at all).
    val resultOption = util.binarySearch(seq, (np: NestedProblem[_]) => !createsCycle(np, component))
    resultOption match {
      case Some(result) =>
        // There exists a subproblem such that expanding to it does not create a cycle; return it
        expandableComponents(result) += component
        result.asInstanceOf[NestedProblem[V]]
      case None =>
        // All previously expanded copies of this subproblem create a cycle, or the subproblem has not yet been expanded
        // at all (i.e. the sequence is empty)
        // Make a new nested problem and add it to the end of the sequence before returning it
        val result = new NestedProblem(this, component.expandFunction(parentValue))
        expandableComponents += result -> Set(component)
        expansions((function, parentValue)) = seq :+ result
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
   *  Get the component associated with this element in this collection.
   *  Throws an exception if the element is not associated with any component.
   */
  def apply[T](atomic: Atomic[T]): AtomicComponent[T] = components(atomic).asInstanceOf[AtomicComponent[T]]

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
          case apply: Apply[T] => new ApplyComponent(problem, apply)
          case atomic: Atomic[T] => new AtomicComponent(problem, atomic, rangingStrategy(atomic))
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
