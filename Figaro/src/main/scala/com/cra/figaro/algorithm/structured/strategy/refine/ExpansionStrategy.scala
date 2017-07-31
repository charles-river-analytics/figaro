/*
 * ExpansionStrategy.scala
 * Strategies that refine by expanding recursively.
 *
 * Created By:      William Kretschmer (kretsch@mit.edu)
 * Creation Date:   Jul 24, 2017
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
 * Refining strategies that start with a list of components belonging to a single problem, working bottom-up to
 * expand the model lazily by recursing through subproblems.
 * @param problem Problem to refine.
 * @param initialComponents List of components belonging to the problem from which to begin bottom-up decomposition.
 * @param maxDepth Maximum depth of expansion from the initial components given. Depth is decremented each time the
 * algorithm recurses into a subproblem through an expandable component. When the depth reaches -1, the algorithm
 * returns * for the range of a component. Thus, setting the maximum depth to 0 corresponds to not recursing at all.
 * Defaults to `Int.MaxValue` for expansion of the entire model (does not terminate on infinite models).
 */
class ExpansionStrategy(problem: Problem, initialComponents: List[ProblemComponent[_]],
                        maxDepth: Int = Int.MaxValue) extends RefiningStrategy(problem.collection) {

  /**
   * Map from problem components to the greatest depth at which that component has been visited. This also implicitly
   * stores the set of components that have been refined by this strategy.
   */
  private[figaro] val depths = mutable.Map[ProblemComponent[_], Int]()
  /**
   * Map from problem components to the set of visited components that directly depend on it. This map is used for
   * backtracking: it is possible that due to lazy expansion, a component gets visited more than once at increasing
   * depth. When this happens, we need to backtrack and update any dependendent components to have consistent ranges.
   */
  private[figaro] val directUpdates = mutable.Map[ProblemComponent[_], Set[ProblemComponent[_]]]().withDefaultValue(Set())

  /**
   * Get the problem component associated with an element. This may involve adding the element to the collection if a
   * problem component has not yet been created.
   * @param element Element for which to return a problem component.
   */
  protected def checkArg[T](element: Element[T]): ProblemComponent[T] = {
    if(collection.contains(element)) collection(element)
    else problem.add(element)
  }

  /**
   * Execute this decomposition strategy, starting with the initial components. This should only be called once.
   */
  override def execute(): Unit = {
    for(ic <- initialComponents) {
      decompose(ic, maxDepth)
    }
    markProblemsUnsolved(depths.map(_._1.problem).toSet)
  }

  /**
   * Decompose a single problem component by recursively decomposing components on which it depends, then generating the
   * range for each component. This guarantees that when ranges are created, all components on which a component depends
   * (at that depth) will have been processed.
   *
   * As a rule, this method never works in the other direction. If component B depends (directly or indirectly) on
   * component A, then calling decompose on A will never recursively call decompose on component B. This is to avoid
   * infinite loops, and to guarantee that the range of A does not unpredictably change in the middle of a call to
   * decompose A.
   *
   * Once the range has been created, the `depths` map is updated. If any previously visited components depend on
   * `comp`, this method updates those components in a top-down manner.
   * @param comp Component to decompose. This strategy never decomposes a component more than once, so if the component
   * is fully refined or has already been expanded to the desired depth, this method does nothing.
   * @param depth Depth of expansion with respect to this component.
   */
  def decompose(comp: ProblemComponent[_], depth: Int): Unit = {
    if (depth > depths.getOrElse(comp, -1) && !comp.fullyRefined) {
      comp match {
        case chainComp: ChainComponent[_, _] =>
          processChain(chainComp, depth)
        case maComp: MakeArrayComponent[_] =>
          processMakeArray(maComp, depth)
        case atomicComp: AtomicComponent[_] =>
          processAtomic(atomicComp)
        case _ =>
          process(comp, depth)
      }
      // Also used to track which comps/problems have been modified
      depths(comp) = depth
      // Update any dependencies in a top-down recursive manner
      val updatesNeeded = util.reachable((pc: ProblemComponent[_]) => directUpdates(pc), false, comp)
      if(updatesNeeded.nonEmpty) new FlatStrategy(collection, updatesNeeded).execute()
    }
  }

  /**
   * Generate the range for the given Chain component. Mark it as fully refined or enumerated if applicable.
   * @param chainComp Chain component to process.
   * @param depth Depth of expansion with respect to this component.
   */
  def processChain(chainComp: ChainComponent[_, _], depth: Int): Unit = {
    // Decompose the parent to get values for expansion
    val parentComp = checkArg(chainComp.chain.parent)
    decompose(parentComp, depth)
    directUpdates(parentComp) += chainComp
    // Ensure expansions exist for each parent value
    chainComp.expand()
    // Recurse on subproblems
    val subproblems = chainComp.subproblems.values
    for(subproblem <- subproblems) {
      val target = checkArg(subproblem.target)
      decompose(target, depth - 1)
      directUpdates(target) += chainComp
    }
    // Make range based on the refinement of the subproblems
    generateRange(chainComp)
    // The range for this component is complete if the range of the parent is complete (and therefore no further
    // subproblems can be created), and the target for each subproblem has a complete range
    chainComp.fullyEnumerated =
      parentComp.fullyEnumerated && subproblems.forall { sp => collection(sp.target).fullyEnumerated }
    // If all components in the subproblems are fully refined, then the chain component is also fully refined
    chainComp.fullyRefined = chainComp.fullyEnumerated && subproblems.forall(_.fullyRefined)
  }

  /**
   * Generate the range for the given MakeArray component. Mark it as fully refined or enumerated if applicable.
   * @param maComp MakeArray component to process.
   * @param depth Depth of expansion.
   */
  def processMakeArray(maComp: MakeArrayComponent[_], depth: Int): Unit = {
    // Decompose the number of items component to get the maximum number of expansions
    val numItemsComp = checkArg(maComp.makeArray.numItems)
    decompose(numItemsComp, depth)
    directUpdates(numItemsComp) += maComp
    // Ensure expansions exist up to the maximum number of items
    maComp.expand()
    // Decompose each of the items
    val items = maComp.makeArray.items.take(maComp.maxExpanded).toList
    val itemsComps = items.map(checkArg(_))
    for(ic <- itemsComps) {
      decompose(ic, depth - 1)
      directUpdates(ic) += maComp
    }
    // Make range based on the ranges of the items
    generateRange(maComp)
    // The range for this component is complete if the number of items and each item have complete ranges
    // This also implies that the component is fully refined because there are no subproblems
    maComp.fullyEnumerated = numItemsComp.fullyEnumerated && itemsComps.forall(_.fullyEnumerated)
    maComp.fullyRefined = maComp.fullyEnumerated
  }

  /**
   * Generate the range for the given atomic component. Mark it as fully refined or enumerated if applicable.
   * @param atomicComp Atomic component to process.
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
   * @param comp Component to process.
   * @param depth Depth of expansion.
   */
  def process(comp: ProblemComponent[_], depth: Int): Unit = {
    // Decompose the args of this component
    val argComps = comp.element.args.map(checkArg(_))
    for(ac <- argComps) {
      // TODO should we decrement the depth when processing an Apply component?
      decompose(ac, depth)
      directUpdates(ac) += comp
    }
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
