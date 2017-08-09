/*
 * PriorityMap.scala 
 * Trait of priority maps
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Jan 1, 2009
 * 
 * Copyright 2017 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.util

import scala.collection.mutable.Map

/**
 *  A priority map is like a priority queue except that keys in the clue are mapped to values that
 *  determine their place in the queue. The value associated with a key can be updated, resulting in
 *  the key being moved to a different place in the queue.
 */
trait PriorityMap[T, U] extends Map[T, U] {
  /**
   * Extract and return the item with the minimum value.
   */
  def extractMin(): (T, U)
}

object PriorityMap {
  /**
   * Create a priority map containing the given key/value pairs. Uses the heap priority map
   * implementation.
   */
  def apply[T, U](pairs: (T, U)*)(implicit ord: Ordering[U]): PriorityMap[T, U] = {
    val result = new HeapPriorityMap[T, U]()(ord)
    pairs foreach (result += _)
    result
  }
}

