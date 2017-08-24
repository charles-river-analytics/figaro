/*
 * DepthFirstStrategy.scala
 * Strategies that refine in a depth-first manner from an initial set.
 *
 * Created By:      William Kretschmer (kretsch@mit.edu)
 * Creation Date:   Aug 07, 2017
 *
 * Copyright 2017 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */
package com.cra.figaro.algorithm.structured.strategy.refine

import com.cra.figaro.algorithm.structured._
import com.cra.figaro.language._

import scala.collection.mutable

/**
 * Strategies that recursively refine depth-first through arguments and subproblems of problem components. These
 * strategies never visit a component more than once; thus they are not reusable.
 * @param collection Collection of components to refine.
 */
abstract class DepthFirstStrategy(collection: ComponentCollection) extends RefiningStrategy(collection) {

  /**
   * Set of components that have been refined by this strategy.
   */
  private[figaro] val done: mutable.Set[ProblemComponent[_]] = mutable.Set()

  /**
   * Get the problem component associated with an element. This may involve adding the element to the collection if a
   * problem component has not yet been created.
   * @param element Element for which to return a problem component.
   */
  protected def checkArg[T](element: Element[T]): ProblemComponent[T]

  /**
   * Decide if this strategy should refine the given problem component. This is usually based on the refinement status
   * of the component and the components on which it depends. This check should not depend on the set `done`; such a
   * check is performed separately.
   * @param comp Component to consider refining.
   * @return True if and only if this strategy should refine the argument.
   */
  protected def shouldRefine(comp: ProblemComponent[_]): Boolean

  /**
   * Initial components to process. Decomposition proceeds in a depth-first fashion from these components.
   * @return Traversable of components from which to refine initially.
   */
  def initialComponents: Traversable[ProblemComponent[_]]

  override def execute(): Unit = {
    initialComponents.foreach(refine)
    markProblemsUnsolved(done.map(_.problem).toSet)
  }

  /**
   * Refine a single problem component by recursively refining components on which it depends, then generating the range
   * for each component. This guarantees that when ranges are created, all components on which a component depends will
   * have been processed.
   *
   * As a rule, this method never works in the other direction. If component B depends (directly or indirectly) on
   * component A, then calling decompose on A will never recursively call decompose on component B. This is to avoid
   * infinite loops, and to guarantee that the range of A does not unpredictably change in the middle of a call to
   * decompose A.
   *
   * Once the range has been created, the component will be added to the set done, and (if applicable) the component is
   * marked as fully enumerated or refined.
   * @param comp Component to refine. This strategy never refines a component more than once, so if the component is
   * fully refined or in the set done, this method does nothing.
   */
  def refine(comp: ProblemComponent[_]): Unit = {
    // Only process if the component is neither fully refined nor already visited
    if(!done.contains(comp) && shouldRefine(comp)) {
      comp match {
        case chainComp: ChainComponent[_, _] =>
          processChain(chainComp)
        case maComp: MakeArrayComponent[_] =>
          processMakeArray(maComp)
        case atomicComp: AtomicComponent[_] =>
          processAtomic(atomicComp)
        case _ =>
          process(comp)
      }
      done += comp
    }
  }

  /**
   * Generate the range for the given Chain component. Mark it as fully refined or enumerated if applicable.
   * @param chainComp Chain component to process. This should not be called on a component we previously processed,
   * including fully refined components.
   */
  def processChain[P, V](chainComp: ChainComponent[P, V]): Unit

  /**
   * Generate the range for the given MakeArray component. Mark it as fully refined or enumerated if applicable.
   * @param maComp MakeArray component to process. This should not be called on a component we previously processed,
   * including fully refined components.
   */
  def processMakeArray[V](maComp: MakeArrayComponent[V]): Unit

  /**
   * Generate the range for the given atomic component. Mark it as fully refined or enumerated if applicable.
   * @param atomicComp Atomic component to process. This should not be called on a component we previously processed,
   * including fully refined components.
   */
  def processAtomic[V](atomicComp: AtomicComponent[V]): Unit = {
    // An atomic element has no args; simply generate its range
    generateRange(atomicComp)
    // Decide if the component is fully enumerated/refined based on the ranging strategy used
    atomicComp.fullyRefined = atomicComp.ranger.fullyRefinable()
    atomicComp.fullyEnumerated = atomicComp.fullyRefined
  }

  /**
   * Generate the range for the given component. Mark it as fully refined or enumerated if applicable.
   * @param comp Component to process. This should not be called on a component we previously processed, including fully
   * refined components.
   */
  def process[V](comp: ProblemComponent[V]): Unit = {
    // Decompose the args of this component
    val argComps = comp.element.args.map(checkArg(_))
    argComps.foreach(refine)
    // Make range based on the ranges of the args
    generateRange(comp)
    // We need to know if all args are fully enumerated to determine the enumeration and refinement status
    val argsFullyEnumerated = argComps.forall(_.fullyEnumerated)
    // If the range doesn't have *, the enumeration status breaks into several cases
    comp.fullyEnumerated = !comp.range.hasStar && {
      comp.element match {
        // Ranges of compound/parameterized Flip and Select don't depend on parents
        case _: Flip | _: Select[_, _] => true
        // CompoundDist range does not depend on the probabilities; only the outcomes need to be enumerated
        // It is safe to call collection(o) because the collection contains all of the outcomes (through argComps)
        case d: CompoundDist[_] => d.outcomes.forall(o => collection(o).fullyEnumerated)
        // Otherwise, we assume the range is generated deterministically from the element's args, which means this is
        // enumerable if and only if all of the args are fully enumerated
        case _ => argsFullyEnumerated
      }
    }
    // We assume the factors depend only on the ranges of this component and its arguments. When all of these components
    // are fully enumerated, the factors of this component can't change any further, and therefore it is fully refined.
    comp.fullyRefined = comp.fullyEnumerated && argsFullyEnumerated
  }
}
