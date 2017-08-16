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
 * A collection of problem components. This data structure manages all the components being used in the solution of a
 * top-level problem and its nested subproblems. Every element exists in at most one component. To create a new
 * component for an element, you need to say what problem it belongs to.
 *
 * This class is intended for models that do not use chain function memoization recursively; for this purpose, use
 * `RecursiveComponentCollection`.
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
   * Get the recursion depth associated with adding the given expansion to the given expandable component. Intuitively,
   * the recursion depth is meant to correspond to the number of times an expansion has recursively called itself;
   * however, this quantity is free to be defined by the particular implementation of this collection. This quantity is
   * used to create recursive expansions of a subproblem for different depths. It ensures that only one subproblem is
   * created for each depth.
   *
   * By default, this method always returns 0, which is the default for non-recursive expansion.
   */
  private[figaro] def getRecursionDepth(value: ExpandableComponent[_, _], newExpansion: Expansion): Int = 0

  /**
   * Bijectively maps an expansion and a recursion depth to a corresponding problem. The inverse map is
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
    val recursionDepth = getRecursionDepth(component, newExpansion)
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

/**
 * Component collections for models that recursively use chain function memoization. This works by incrementing depth
 * at every expansion. Thus, if the maximum depth of expansion is d, then there can be up to d copies of each expansion.
 */
class IncrementingCollection extends ComponentCollection {

  /**
   * Get the recursion depth for expansion by incrementing the depth associated with the component's problem.
   */
  override private[figaro] def getRecursionDepth(component: ExpandableComponent[_, _], newExpansion: Expansion): Int = {
    component.problem match {
      case np: NestedProblem[_] =>
        // Get the expansion that produced this nested problem and increment it
        val (_, npDepth) = problemToExpansion(np)
        npDepth + 1
      // Base case: start with 0 when expanding from a top-level problem
      case _ => 0
    }
  }
}

/**
 * Component collections for models that recursively use chain function memoization. This works by incrementing depth at
 * as few edges as possible in the graph of expansions. The collection maintains a graph of expansions that can use each
 * other without incrementing the depth of recurison. Expansions are greedily added to this graph until one creates a
 * cycle. Such an edge is instead recorded as an edge along which we must increment the depth when expanding.
 */
class SelectiveIncrementingCollection extends ComponentCollection {
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
    // See e.g. Bender, Fineman, Gilbert, Tarjan (2011)
    @tailrec
    def bfs(expansions: Set[Expansion]): Boolean = {
      if(expansions.isEmpty) false
      else if(expansions.contains(to)) true
      else bfs(expansions.flatMap(expansionToLevel.getOrElseUpdate(_, Set())))
    }
    bfs(Set(from))
  }

  /**
   * Get the recursion depth for an expansion by checking to see if it is possible to use the same recurison depth as
   * that associated with the given component. Otherwise, this increments the recursion depth by 1. This involves
   * performing a search to see if the new expansion ever uses the expansion associated with the component's problem.
   * Because this search can be expensive (in general, it may take linear time in the number of expansions), it is
   * memoized.
   */
  override private[figaro] def getRecursionDepth(component: ExpandableComponent[_, _], newExpansion: Expansion): Int = {
    component.problem match {
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
  }
}

/**
 * Component collections for models that recursively use chain function memoization. When expanding a subproblem, this
 * works by computing the least depth that does not create a cycle in the graph of subproblems.
 *
 * This maintains the invariant that if an expansion exists at depth d > 0, then an expansion also exists at depth
 * d - 1. Furthermore, the expansion at depth d - 1 directly or indirectly uses the expansion at depth d. This invariant
 * makes it possible to find the least depth that does not create a cycle by performing exponential search.
 */
class MinimalIncrementingCollection extends ComponentCollection {

  /**
   * Tests if the adding the subproblem to the given component would create a cycle in the subproblem graph.
   */
  private[figaro] def createsCycle(nestedProblem: NestedProblem[_], component: ExpandableComponent[_, _]): Boolean = {
    // TODO consider using a dedicated incremental cycle detection data structure and algorithm for improved efficiency
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
            val next = problems.flatMap(expandsFrom(_)).collect{ case np: NestedProblem[_] => np }
            bfs(next)
          }
        }

        bfs(Set(componentProblem))
      case _ => false
    }
  }

  /**
   * Get the recursion depth for an expansion by looking for the least recursion depth that does not create a cycle in
   * the subproblem graph.
   */
  override private[figaro] def getRecursionDepth(component: ExpandableComponent[_, _], newExpansion: Expansion): Int = {
    // Tests if expanding into the copy of this new expansion at this particular depth would create a cycle
    def depthCreatesCycle(depth: Int): Boolean = {
      expansionToProblem.get((newExpansion, depth)).exists(createsCycle(_, component))
    }

    // Look for the least depth that does NOT create a cycle by exponential search
    // lo: current lower bound; i.e. all depths strictly less than lo create a cycle
    // hi: current upper bound for the exponential search, but not yet known to be a depth that does not produce a cycle
    @tailrec
    def exponentialSearch(lo: Int, hi: Int): Int = {
      // If (hi - 1) depth creates a cycle, then so do all other depths in the semi-open interval [lo, hi)
      // So, we repeat the search, this time with an inclusive lower bound of hi
      if(depthCreatesCycle(hi - 1)) exponentialSearch(hi, hi * 2)
      // Otherwise, the least depth that does not create a cycle must be in [lo, hi)
      else binarySearch(lo, hi)
    }

    // Look for the least depth within the bounds that does NOT create a cycle by binary search
    // Assumes that the least depth that does not create a cycle is in [lo, hi)
    @tailrec
    def binarySearch(lo: Int, hi: Int): Int = {
      if(hi == lo + 1) lo
      else {
        val mid = lo + (hi - lo - 1) / 2
        if(depthCreatesCycle(mid)) binarySearch(mid + 1, hi)
        else binarySearch(lo, mid + 1)
      }
    }

    // This successively searches the intervals [0,1); [1,2); [2,4); [4,8); etc.
    // Note: 0 is the "base case"; i.e. a expansion will be created at depth 0 first if it hasn't been expanded yet
    exponentialSearch(0, 1)
  }
}
