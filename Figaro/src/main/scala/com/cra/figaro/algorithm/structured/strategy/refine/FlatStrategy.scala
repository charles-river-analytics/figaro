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

/**
 * Strategies that refine a fixed set of components without recursing or visiting other components. The primary purpose
 * of this strategy is as a ranging tool. New elements will not be added to the collection, unless they belong to newly
 * expanded subproblems, in which case they will be added without being refined. Another way of saying this is that this
 * strategy will not recursively refine subproblems.
 * @param collection Collection of components to refine.
 * @param initialComponents Components to refine. Often, this is a set of atomic components and their dependencies. Note
 * that this strategy does not recursively add or update any components not in this set. Thus, it is up to the caller of
 * this constructor to ensure that (1) the components in this set and their arguments already belong to the collection,
 * and (2) that such a call maintains consistency across component ranges.
 */
class FlatStrategy(collection: ComponentCollection, override val initialComponents: Set[ProblemComponent[_]])
  extends DepthFirstStrategy(collection) {

  // Only refine components in the given set
  override def shouldRefine(comp: ProblemComponent[_]): Boolean = initialComponents.contains(comp)

  // Assume that all arguments already belong to the collection
  override def checkArg[T](element: Element[T]): ProblemComponent[T] = collection(element)

  override def processChain[P, V](chainComp: ChainComponent[P, V]): Unit = {
    // Visit the parent to get values for expansion
    val parentComp = checkArg(chainComp.chain.parent)
    refine(parentComp)
    // Only visit the subproblems that already exist for the current set of parent values
    val existingSubproblems = parentComp.range.regularValues.flatMap(chainComp.subproblems.get(_))
    // Ensure expansions exist for each parent value, but don't visit the new subproblems
    chainComp.expand()
    // Decompose the target of each existing subproblem
    for(subproblem <- existingSubproblems) {
      refine(checkArg(subproblem.target))
    }
    // Make range based on the refinement of the subproblems
    generateRange(chainComp)

    // All subproblems, previously or newly expanded
    val subproblems = for(parentValue <- parentComp.range.regularValues) yield chainComp.subproblems(parentValue)
    // The range for this component is complete if the range of the parent is complete (and therefore no further
    // subproblems can be created), and the target for each subproblem has a complete range
    chainComp.fullyEnumerated =
      parentComp.fullyEnumerated && subproblems.forall { sp => checkArg(sp.target).fullyEnumerated }
    // If all components in the subproblems are fully refined, then the chain component is also fully refined
    chainComp.fullyRefined = chainComp.fullyEnumerated && subproblems.forall(_.fullyRefined)
  }

  override def processMakeArray[V](maComp: MakeArrayComponent[V]): Unit = {
    // Decompose the number of items component to get the maximum number of expansions
    val numItemsComp = checkArg(maComp.makeArray.numItems)
    refine(numItemsComp)
    // Only visit the items that have already been expanded
    val maxExpanded = maComp.maxExpanded
    // Ensure expansions exist up to the maximum number of items, but do not recurse on the potentially new ones
    maComp.expand()
    // Decompose each of the existing items
    for(item <- maComp.makeArray.items.take(maxExpanded)) {
      val itemComp = checkArg(item)
      refine(itemComp)
    }
    // Make range based on the ranges of the items
    generateRange(maComp)

    // All item components used in the generation of this MakeArray, newly expanded or otherwise
    val itemComps = for(item <- maComp.makeArray.items.take(maComp.maxExpanded)) yield checkArg(item)
    // The range for this component is complete if the number of items and each item have complete ranges
    // This also implies that the component is fully refined because there are no subproblems
    maComp.fullyEnumerated = numItemsComp.fullyEnumerated && itemComps.forall(_.fullyEnumerated)
    maComp.fullyRefined = maComp.fullyEnumerated
  }
}
