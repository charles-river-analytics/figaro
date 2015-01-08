/*
 * MultiSet.scala  
 * Trait of multisets that can contain more than one instance of the same element.
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

/**
 * Trait of multisets that can contain more than one instance of the same element. 
 * 
 * @tparam T The type of values contained in the set
 */
trait MultiSet[T] extends Traversable[T] {
  /**
   * Return the number of instances of the value in the set.
   */
  def apply(t: T): Int

  /**
   * Add an instance of this value to the set.
   */
  def addOne(t: T): MultiSet[T]

 /**
   * Add several instances of this value to the set.
   */
  def addMany(t: T, count: Int): MultiSet[T]

  /**
   * Remove one instance of this value from the set.
   */
  def removeOne(t: T): MultiSet[T]

  /** Remove all instances of this value from the set.
   *  
   */
  def removeAll(t: T): MultiSet[T]

  /**
   * Return the union of another set with this set.
   */
  def union(that: MultiSet[T]): MultiSet[T]

  /**
   * Map this set to another set by applying the supplied function.
   */
  def map[U](fn: T => U): MultiSet[U]

  /**
   * Iterate over instances in this set and apply the given function.
   */
  def foreach[U](fn: T => U): Unit

  /**
   * Creates a list of elements, where each element may appear multiple times. Order is arbitrary.
   */
  def elements: List[T]

  /**
   * Creates a list of (element, count) pairs. Order is arbitrary.
   */
  def counts: Set[(T, Int)]

  override def hashCode: Int = counts.hashCode

  /**
   * Returns true if the counts are the same.
   */
  override def equals(that: Any): Boolean = {
    try {
      val ms = that.asInstanceOf[MultiSet[T]]
      ms.counts == counts
    } catch {
      case _: ClassCastException => false
    }
  }
}
