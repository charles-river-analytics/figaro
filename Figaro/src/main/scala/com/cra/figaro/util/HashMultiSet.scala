/*
 * HashMultiSet.scala 
 * A hashed MultiSet
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Jan 1, 2009
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.util

import scala.collection.mutable.HashMap

/**
 * An implementation of a MultiSet backed by a HashMap.
 */
class HashMultiSet[T] extends MultiSet[T] {
  private val map = HashMap[T, Int]()

  def apply(t: T): Int = map.getOrElse(t, 0)

  /* All modification operations are in place. */
  def addOne(t: T): HashMultiSet[T] = {
    map += t -> (apply(t) + 1)
    this
  }

  def addMany(t: T, count: Int): HashMultiSet[T] = {
    require(count > 0)
    map += t -> (apply(t) + count)
    this
  }

  def removeOne(t: T): HashMultiSet[T] = {
    apply(t) match {
      case 0 => () // Attempting to remove an element not present does not do anything - no failure
      case 1 => map -= t
      case n => map += t -> (n - 1)
    }
    this
  }

  def removeAll(t: T): HashMultiSet[T] = {
    map -= t
    this
  }

  def union(that: MultiSet[T]): MultiSet[T] = {
    val result = new HashMultiSet[T]()
    // This will catch all keys in this map, whether or not they are contained in the other
    for { (key, value) <- map } { result.addMany(key, value + that(key)) }
    // This will catch all keys in the other map, whether or not they are contained in this
    for { (key, value) <- that.counts; if apply(key) == 0 } { result.addMany(key, value) }
    result
  }

  def counts: Set[(T, Int)] = map.toSet

  def elements: List[T] = map.toList.map(pair => List.fill(pair._2)(pair._1)).flatten

  def map[U](fn: T => U): HashMultiSet[U] = {
    val result = new HashMultiSet[U]()
    // Different Ts might map to the same U; this is correctly handled by addMany below.
    for { (key, value) <- map } { result.addMany(fn(key), value) }
    result
  }

  def foreach[U](fn: T => U): Unit = {
    for { (key, value) <- map; i <- 1 to value } { fn(key) }
  }
}

object HashMultiSet {
  /**
   * Create a new HashMultiSet and adds the given values.
   */
  def apply[T](elems: T*): HashMultiSet[T] = {
    val result = new HashMultiSet[T]()
    elems.foreach(result.addOne(_))
    result
  }
}
