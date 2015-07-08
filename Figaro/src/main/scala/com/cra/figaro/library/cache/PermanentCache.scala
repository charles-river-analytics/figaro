/*
 * PermanentCache.scala
 * Only caches permanent result elements in chain.
 *
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Jan 1, 2009
 *
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */
package com.cra.figaro.library.cache

import com.cra.figaro.language._
import scala.collection.mutable.Map

/**
 * A class which only caches permanent result elements in chain. This class does not cache any non-caching
 * chain result elements. Since this class does not implement any element cleanup operations, it is
 * best used in an algorithm that clears temporary elements periodically.
 *
 *
 */
class PermanentCache(universe: Universe) extends Cache(universe) {

  /* Caching chain cache that maps from an element to a map of parent values and resulting elements */
  private[figaro] val ccCache: Map[Element[_], Map[Any, Element[_]]] = Map()

  /* The inverted cache. This maps from result elements back to the chain that uses them. This is needed
   * to properly clean up deactivated elements 
   */
  private[figaro] val ccInvertedCache: Map[Element[_], Map[Element[_], Any]] = Map()

  /**
   * Retrieve any cached element generated from the current value of the supplied element. Returns None if
   * the element does not generate another element.
   *
   */
  def apply[T](element: Element[T]): Option[Element[T]] = {
    element match {
      case c: CachingChain[_, T] => {
        doCachingChain(c)
      }
      case c: NonCachingChain[_, T] => {
        Some(c.get(c.parent.value))
      }
      case _ => None
    }
  }

  /*
   * Retrieves an element from the caching chain cache, or inserts a new one if none is found for
   * the value of this element
   */
  private def doCachingChain[U, T](c: Chain[U, T]): Option[Element[T]] = {

    val cachedElems = ccCache.getOrElseUpdate(c, Map())
    val cachedValue = cachedElems.get(c.parent.value)
    if (!cachedValue.isEmpty) cachedValue.asInstanceOf[Option[Element[T]]]
    else {
      // If the value of the element is not found in the cache, generate a new element by calling the chain,
      // add it to the cache -only if the result is a permanent element-
      val result = c.get(c.parent.value)
      if (!result.isTemporary) {
        cachedElems += (c.parent.value -> result)
        val invertedElems = ccInvertedCache.getOrElseUpdate(result, Map())
        invertedElems += (c -> c.parent.value)
      }
      Some(result)
    }
  }

  /**
   * Removes an element from the cache. This is needed to properly clean up elements as they are deactivated.
   */
  def -=(element: Element[_]) = {
    ccCache -= element
    val invertValue = ccInvertedCache.get(element)
    if (invertValue.nonEmpty) invertValue.get.foreach(e => ccCache(e._1) -= e._2)
    ccInvertedCache -= element
    this
  }

  /**
   * Clears the cache of all stored elements.
   */
  def clear() = {
    ccCache.clear()
    ccInvertedCache.clear()
    universe.deregister(this)
  }
}