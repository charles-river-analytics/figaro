/*
 * ComponentCollection.scala
 * A data structure that holds all the problem components used in a top-level problem and its subproblems.
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
   * An expansion is defined by a generative process (a function) that produces an element, and a parent value used as
   * the argument to the function.
   */
  type Expansion = (_ => Element[_], _)

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
   * Maps an expansion to the set of expansions that it uses without incrementing recursion depth. This is populated
   * greedily, in the sense that an expansion will always try to use another expansion at the same depth if it does not
   * create a cycle.
   */
  private[figaro] val expansionToLevel: Map[Expansion, Set[Expansion]] = Map()

  /**
   * The complement of `expansionToLevel`. This maps an expansion `x` to the set of other expansions `y` with the
   * property that expanding a subproblem for `y` from a subproblem for `x` must increment the recursion depth.
   */
  private[figaro] val expansionToDeeper: Map[Expansion, Set[Expansion]] = Map()

  /**
   * Tests if there exists a path from `from` to `to` in the directed graph induced by `expansionToLevel`. This is
   * generally used to test if adding an edge from `to` to `from` would create a cycle.
   */
  private[figaro] def levelPathExists(from: Expansion, to: Expansion): Boolean = {
    // TODO consider using a dedicated incremental cycle detection data structure and algorithm for improved efficiency
    // TODO consider a different version of ComponentCollection that avoids this computation for non-lazy algorithms
    @tailrec
    def bfs(expansions: Set[Expansion]): Boolean = {
      if(expansions.isEmpty) false
      else if(expansions.contains(to)) true
      else bfs(expansions.flatMap(expansionToLevel.getOrElseUpdate(_, Set())))
    }
    bfs(Set(from))
  }

  /**
   * Bijectively maps an expansion and a recursion depth to a corresponding problem. Intuitively, the recurison depth
   * corresponds to the number of times an expansion has recursively called itself. The inverse map is
   * `problemToExpansion`.
   */
  private[figaro] val expansionToProblem: Map[(Expansion, Int), NestedProblem[_]] = Map()

  /**
   * Bijectively maps a subproblem to a corresponding expansion and a recursion depth. The inverse map is
   * `expansionToProblem`.
   */
  private[figaro] val problemToExpansion: Map[NestedProblem[_], (Expansion, Int)] = Map()

  /**
   * Maps a nested problem to the set of problems that use it through an expandable component.
   */
  private[figaro] val expandsFrom: Map[NestedProblem[_], Set[Problem]] = Map()

  /**
   * Get the nested subproblem associated with a particular function and parent value. The recursion depth of the
   * returned problem is either equal to the recursion depth of the component's problem, or is incremented by one,
   * depending on whether the expansion belongs to `expansionToLevel` or `expansionToDeeper`. The returned subproblem is
   * cached by recursion depth.
   */
  private[figaro] def expansion[P, V](component: ExpandableComponent[P, V], function: P => Element[V], parentValue: P): NestedProblem[V] = {
    val newExpansion = (function, parentValue)
    val recursionDepth = component.problem match {
      case np: NestedProblem[_] =>
        // Get the expansion that produced this nested problem
        val (npExpansion, npDepth) = problemToExpansion(np)
        // Look to see if we previously decided to increment the depth or not when making this expansion
        val level = expansionToLevel.getOrElseUpdate(npExpansion, Set())
        val deeper = expansionToDeeper.getOrElseUpdate(npExpansion, Set())
        val incrementDepth =
          if(level.contains(newExpansion)) false
          else if(deeper.contains(newExpansion)) true
          else {
            // See if keeping the same depth when moving from the nested problem's expansion to this expansion would
            // create a cycle, by testing if there is a path in the other direction
            val levelPath = levelPathExists(newExpansion, npExpansion)
            // Cache this edge in the expansion graph
            if(levelPath) expansionToDeeper(npExpansion) = deeper + newExpansion
            else expansionToLevel(npExpansion) = level + newExpansion
            levelPath
          }
        if(incrementDepth) npDepth + 1 else npDepth
      case _ => 0
    }
    // Get the subproblem cached by function, parent value, and recursion depth
    val result = expansionToProblem.getOrElseUpdate((newExpansion, recursionDepth),
                                                    new NestedProblem(this, component.expandFunction(parentValue)))
    // Mark the subproblem as used by the component's problem
    expandsFrom(result) = expandsFrom.getOrElse(result, Set()) + component.problem
    // Record the expansion and recursion depth associated with the returned problem
    problemToExpansion(result) = (newExpansion, recursionDepth)
    result.asInstanceOf[NestedProblem[V]]
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
