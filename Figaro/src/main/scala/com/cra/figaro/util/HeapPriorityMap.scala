/*
 * HeapPriorityMap.scala  
 * Priority maps using a heap implementation
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

import scala.collection.mutable.Map
import annotation.tailrec

/**
 * A priority map using a heap implementation. Provides O(log n) insertion, update, and extracting the
 * minimum element.
 */

class HeapPriorityMap[T, U]()(implicit ord: Ordering[U]) extends PriorityMap[T, U] {
  private var numElements = 0
  private var capacity = 0
  private var contents: Array[(T, U)] = Array.ofDim(0)

  private def changeSize(newCapacity: Int): Unit = {
    val oldCapacity = capacity
    capacity = newCapacity
    val newContents: Array[(T, U)] = Array.ofDim(capacity)
    for { i <- 0 until numElements } { newContents(i) = contents(i) }
    contents = newContents
  }

  private def grow(): Unit = changeSize(capacity * 2 + 1)

  private def shrink(): Unit = changeSize((capacity + 1) / 2 - 1)

  private val indexMap: Map[T, Int] = Map()

  private def swap(index1: Int, index2: Int): Unit = {
    val (item1, score1) = contents(index1)
    val (item2, score2) = contents(index2)
    contents(index1) = (item2, score2)
    contents(index2) = (item1, score1)
    indexMap(item1) = index2
    indexMap(item2) = index1
  }

  private def parent(i: Int) = i / 2
  private def leftChild(i: Int) = i * 2
  private def rightChild(i: Int) = i * 2 + 1

  private def sendDown(index: Int): Unit = {
    val left = leftChild(index)
    if (left < numElements) {
      val right = rightChild(index)
      if (right < numElements &&
        ord.lt(contents(right)._2, contents(left)._2) &&
        ord.lt(contents(right)._2, contents(index)._2)) {
        swap(index, right)
        sendDown(right)
      } else if (ord.lt(contents(left)._2, contents(index)._2)) {
        swap(index, left)
        sendDown(left)
      }
    }
  }

  private def sendUp(index: Int): Unit = {
    if (index > 0) {
      val par = parent(index)
      if (ord.lt(contents(index)._2, contents(par)._2)) {
        swap(index, par)
        sendUp(par)
      }
    }
  }

  private def doInsert(item: T, score: U): Unit = {
    val oldNumElements = numElements
    if (numElements >= capacity) grow()
    numElements += 1
    contents(oldNumElements) = (item, score)
    indexMap += item -> oldNumElements
    sendUp(oldNumElements)
  }

  private def doRemove(index: Int): U = {
    val (item, score) = contents(index)
    val oldNumElements = numElements
    numElements -= 1
    swap(index, numElements)
    if (numElements <= capacity / 2) shrink()
    indexMap -= item
    sendDown(index)
    score
  }

  @tailrec private def sortHelper(accum: List[(T, U)]): List[(T, U)] =
    if (numElements == 0) accum.reverse
    else sortHelper(extractMin() :: accum)

   /**
   * Make a copy of this HeapPriorityMap, returning a new HeapPriorityMap
   */
  override def clone: HeapPriorityMap[T, U] = {
    val result = new HeapPriorityMap[T, U]()
    result.numElements = numElements
    result.capacity = capacity
    result.contents = contents.clone
    result
  }

  override def isEmpty: Boolean = numElements == 0

  /**
   * Produce an iterator of the (item, score) pairs in increasing order of score.
   */
  def iterator: Iterator[(T, U)] = {
    // It's easiest to use this structures own methods to do the sorting in place, but we want to restore
    // everything at the end.
    val savedNumElements = numElements
    val savedCapacity = capacity
    val savedContents = contents.clone
    val result = sortHelper(List())
    numElements = savedNumElements
    capacity = savedCapacity
    contents = savedContents
    result.iterator
  }

  /**
   * Add the key/value pair to the priority map. If the key is already present, update its value,
   * moving it in the map.
   */
  def +=(pair: (T, U)): HeapPriorityMap.this.type = {
    val (item, score) = pair
    indexMap.get(item) foreach (doRemove(_))
    doInsert(item, score)
    this
  }

  /**
   * Remove the key/value pair from the priority map.
   */
  def -=(item: T): HeapPriorityMap.this.type = {
    indexMap.get(item) foreach (doRemove(_))
    this
  }

  /**
   * Get the value associated with the key.
   */
  def get(item: T): Option[U] =
    indexMap.get(item) map (contents(_)._2)

  /**
   * Extract and return the item with the minimum value.
   */
  def extractMin(): (T, U) = {
    if (numElements == 0) throw new IllegalArgumentException("Empty")
    val (item, score) = contents(0)
    doRemove(0)
    (item, score)
  }
}
