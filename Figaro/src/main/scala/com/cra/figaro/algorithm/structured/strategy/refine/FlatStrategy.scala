/*
 * FlatStrategy.scala
 * Strategies that refine components without recursing.
 *
 * Created By:      William Kretschmer (kretsch@mit.edu)
 * Creation Date:   Jul 25, 2017
 *
 * Copyright 2017 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */
package com.cra.figaro.algorithm.structured.strategy.refine

import com.cra.figaro.algorithm.structured._
import com.cra.figaro.language._
import com.cra.figaro.util

import scala.collection.mutable

/**
 * Strategies that refine a fixed set of components without recursing or visiting other components. The primary purpose
 * of this strategy is as a ranging tool. New elements will not be added to the collection, unless they belong to newly
 * expanded subproblems, in which case they will be added without being refined. Another way of saying this is that this
 * strategy will not recursively refine subproblems.
 * @param collection Collection of components to refine.
 * @param updates Components to refine. Often, this is a set of atomic components their dependencies. Note that this
 * strategy does not recursively update any components not in this set. This, it is up to the caller of this constructor
 * to ensure that such a call maintains consistency across component ranges.
 */
class FlatStrategy(collection: ComponentCollection, updates: Set[ProblemComponent[_]])
  extends RefiningStrategy(collection) {

  // TODO consider shared inheritance with ExpansionStrategy
  // TODO see if Chain function memoization still breaks

  /**
   * Components that have been visited
   */
  private[figaro] val done = mutable.Set[ProblemComponent[_]]()

  /**
   * Execute this decomposition strategy, starting with the initial components. This should only be called once.
   */
  override def execute(): Unit = {
    updates.foreach(decompose)
    markProblemsUnsolved(done.map(_.problem).toSet)
  }

  /**
   * Decompose a single problem component by recursively decomposing components on which it depends, then generating the
   * range for each component. This guarantees that when ranges are created, all components on which a component depends
   * will have been processed.
   *
   * As a rule, this method never works in the other direction. If component B depends (directly or indirectly) on
   * component A, then calling decompose on A will never recursively call decompose on component B. This is to avoid
   * infinite loops, and to guarantee that the range of A does not unpredictably change in the middle of a call to
   * decompose A.
   *
   * Once the range has been created, the component will be added to the set done, and (if applicable) the component is
   * marked as fully enumerated or refined.
   * @param comp Component to decompose. This strategy never decomposes a component more than once, so if the component
   * is fully refined or in the set done, this method does nothing. This method also does nothing if the component does
   * not belong to the `updates` set.
   */
  def decompose(comp: ProblemComponent[_]): Unit = {
    // Only process if the component is neither fully refined nor already visited
    if(updates.contains(comp) && !done.contains(comp) && !comp.fullyRefined) {
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
  def processChain(chainComp: ChainComponent[_, _]): Unit = {
    // Decompose the parent to get values for expansion
    val parentComp = collection(chainComp.chain.parent)
    decompose(parentComp)
    // Only visit the existing subproblems
    val existingSubproblems = chainComp.subproblems.values
    // Ensure expansions exist for each parent value, but don't visit the new subproblems
    chainComp.expand()
    // Decompose the target of each existing subproblem
    for(subproblem <- existingSubproblems) {
      decompose(collection(subproblem.target))
    }
    // Make range based on the refinement of the subproblems
    generateRange(chainComp)
    // All subproblems, previously or newly expanded
    val subproblems = chainComp.subproblems.values
    // The range for this component is complete if the range of the parent is complete (and therefore no further
    // subproblems can be created), and the target for each subproblem has a complete range
    chainComp.fullyEnumerated =
      parentComp.fullyEnumerated && subproblems.forall { sp => collection(sp.target).fullyEnumerated }
    // If all components in the subproblems are fully refined, then the chain component is also fully refined
    chainComp.fullyRefined = chainComp.fullyEnumerated && subproblems.forall(_.fullyRefined)
  }

  /**
   * Generate the range for the given MakeArray component. Mark it as fully refined or enumerated if applicable.
   * @param maComp MakeArray component to process. This should not be called on a component we previously processed,
   * including fully refined components.
   */
  def processMakeArray(maComp: MakeArrayComponent[_]): Unit = {
    // Decompose the number of items component to get the maximum number of expansions
    val numItemsComp = collection(maComp.makeArray.numItems)
    decompose(numItemsComp)
    // Ensure expansions exist up to the maximum number of items
    maComp.expand()
    // Decompose each of the items
    val items = maComp.makeArray.items.take(maComp.maxExpanded).toList
    val itemsComps = items.map(collection(_))
    itemsComps.foreach(decompose)
    // Make range based on the ranges of the items
    generateRange(maComp)
    // The range for this component is complete if the number of items and each item have complete ranges
    // This also implies that the component is fully refined because there are no subproblems
    maComp.fullyEnumerated = numItemsComp.fullyEnumerated && itemsComps.forall(_.fullyEnumerated)
    maComp.fullyRefined = maComp.fullyEnumerated
  }

  /**
   * Generate the range for the given atomic component. Mark it as fully refined or enumerated if applicable.
   * @param atomicComp Atomic component to process. This should not be called on a component we previously processed,
   * including fully refined components.
   */
  def processAtomic(atomicComp: AtomicComponent[_]): Unit = {
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
  def process(comp: ProblemComponent[_]): Unit = {
    // Decompose the args of this component
    val argComps = comp.element.args.map(collection(_))
    argComps.foreach(decompose)
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

object FlatStrategy {
  /**
   * A flat refining strategy that refines in a top-down manner from the given components.
   * @param collection Collection of components to refine.
   * @param topLevel Top-level components to refine and work down from. Strictly speaking, it is not essential that
   * these components be top-level (i.e. have no args), but most components that are not top-level cannot be refined
   * without first refining their args.
   * @return A strategy that uses the given components to find all (directly or indirectly) dependent
   */
  def topDown(collection: ComponentCollection, topLevel: ProblemComponent[_]*): FlatStrategy = {
    // Finds the direct children of the given component that are both in the component collection and not fully refined.
    // Used in computing the set of components that need updates after refining the top-level components.
    def children(comp: ProblemComponent[_]): Traversable[ProblemComponent[_]] = {
      val elem = comp.element
      // Returns the component associated with the element if it is in the collection and not fully refined
      // This isn't an anonymous function only because the Scala compiler can't figure out the types
      def componentOption(child: Element[_]): Option[ProblemComponent[_]] = {
        if(collection.contains(child)) {
          val childComp = collection(child)
          if(childComp.fullyRefined) None
          else Some(childComp)
        }
        else None
      }
      elem.universe.directlyUsedBy(elem).flatMap(componentOption)
    }

    val updates = util.reachable(children, true, topLevel:_*)
    new FlatStrategy(collection, updates)
  }
}
