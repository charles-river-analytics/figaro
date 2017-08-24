/*
 * TopDownStrategy.scala
 * Strategies that refine components in a top-down manner.
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
import com.cra.figaro.util

/**
 * Strategies that refine a fixed set of components, as well as all dependent components that are also in the
 * collection.
 * @param collection Collection of components to refine.
 * @param topLevel Components from which to initialize the top-down search for dependent components. Strictly speaking,
 * it is not essential that these components be top-level (i.e. have no args), but most components that are not
 * top-level cannot be refined without first refining their args. It is up to the caller of this constructor to ensure
 * that (1) the components in this set and their arguments already belong to the collection, and (2) that such a call
 * maintains consistency across component ranges.
 */
class TopDownStrategy(collection: ComponentCollection, topLevel: ProblemComponent[_]*)
  extends FlatStrategy(collection, TopDownStrategy.childrenInCollection(collection, topLevel:_*))

object TopDownStrategy {
  /**
   * Use a top-down search to get the components in the collection that depend on the components given, including the
   * components given.
   * @param collection Collection of components to refine.
   * @param topLevel Top-level components to work down from.
   * @return A strategy that uses the given components to find all (directly or indirectly) dependent
   */
  def childrenInCollection(collection: ComponentCollection, topLevel: ProblemComponent[_]*): Set[ProblemComponent[_]] = {
    // Finds the direct children of the given component that are both in the component collection and not fully refined.
    // Used in computing the set of components that need updates after refining the top-level components.
    def children(comp: ProblemComponent[_]): Traversable[ProblemComponent[_]] = {
      val elem = comp.element
      for {
        child <- elem.universe.directlyUsedBy(elem)
        if collection.contains(child)
        childComp = collection(child)
        if !childComp.fullyRefined
      } yield childComp
    }

    util.reachable(children, true, topLevel:_*)
  }
}
