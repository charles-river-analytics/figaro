/*
 * Cache.scala
 * Abstract class to manage caching of element generation for a universe.
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
import scala.collection.generic.Shrinkable

/**
 * Abstract class to manage caching of element generation for a universe. This class can be used
 * by algorithms to manage caching of chains.
 */
abstract class Cache(universe: Universe) extends Shrinkable[Element[_]] {

  /**
   * Return the next element from the generative process defined by element. If no process
   * is found, return None
   */
  def apply[T](element: Element[T]): Option[Element[T]]

  universe.register(this)

  /**
   * Clear any caching
   */
  def clear(): Unit
  
}

/** A Cache class which performs no caching */
class NoCache(universe: Universe) extends Cache(universe) {
  def apply[T](element: Element[T]): Option[Element[T]] = {
    element match {
      case c: Chain[_,T] => Some(c.get(c.parent.value))
      case _ => None
      
    }
  }
  def clear() = {}
  def -=(element: Element[_]) = this
}


