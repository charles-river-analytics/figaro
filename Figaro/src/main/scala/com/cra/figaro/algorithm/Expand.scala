/*
 * Expand.scala
 * Computes the expansion of a universe.
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Jan 1, 2009
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.algorithm

import com.cra.figaro.language._

/**
 * Class of objects that compute the expansion of a universe. The expansion contains all elements that could
 * be generated as the result of expanding chains. Expansion recursively expands chains that are themselves
 * the result of expansion.
 */
class Expand private (universe: Universe) {
  // A ChainMap maps a chain to a map from parent values to resulting elements
  private val chainMaps: scala.collection.mutable.Map[Element[_], Map[_, Element[_]]] = scala.collection.mutable.Map()

  /**
   * An object for computing range of values of elements in the expanded universe.
   */
  private val values = Values(universe)

  /**
   * Gets the mapping from parent values to result elements associated with a chain.
   */
  def getMap[T, U](chain: Chain[T, U]): Map[T, Element[U]] =
    chainMaps(chain).asInstanceOf[Map[T, Element[U]]]

  /*
   * Creates the map for a particular chain.
   */
  private def makeMap[T, U](chain: Chain[T, U]): Map[T, Element[U]] =
    Map((values(chain.parent).toSeq map (expandValue(chain, _))): _*)

  private def expandValue[T, U](chain: Chain[T, U], value: T): (T, Element[U]) = {
    val result = chain.get(value)
    expandElem(result)
    value -> result
  }

  // We keep track of the set of expanded elements so we don't expand the same one twice.
  // It is important to mark an element as expanded before recursively expanding its arguments so that we
  // don't have an infinite recursion with cyclic models.
  private var expanded: Set[Element[_]] = Set()

  /*
   * Creates all the recursively generated elements, and generates the chainMap.
   */
  private def expandElem[T](elem: Element[T]): Unit = {
    if (!(expanded contains elem)) {
      expanded += elem
      elem.args foreach (expandElem(_))
      elem match {
        case chain: Chain[_, _] => chainMaps += chain -> makeMap(chain)
        case _ => ()
      }
    }
  }

  // Make sure elements are removed from chainMaps when they are deactivated.
  universe.register(chainMaps)

  universe.activeElements foreach (expandElem(_))
}

object Expand {
  private val expansions = scala.collection.mutable.Map[Universe, Expand]()

  /**
   * Remove an expansion of a universe from the expansion cache.
   */
  def removeExpansion(universe: Universe = Universe.universe) {
    expansions -= universe
  }

  /**
   * Expand a universe.
   */
  def apply(universe: Universe = Universe.universe): Expand = {
    expansions.get(universe) match {
      case Some(e) => e
      case None =>
        val expansion = new Expand(universe)
        expansions += (universe -> expansion)
        universe.registerUniverse(expansions)
        expansion
    }
  }
}
