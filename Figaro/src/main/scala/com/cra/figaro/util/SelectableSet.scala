/*
 * SelectableSet.scala 
 * Sets that allow selection of an element uniformly at random.
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

import scala.collection.mutable.Set

/**
 * A set that supports selection of an element uniformly at random, in addition to the usual set
 * operations.
 */
trait SelectableSet[T] extends Set[T] {
  /**
   * Select an element uniformly at random.
   */
  def select(): T
}

/**
 * HashSelectableSet uses a hashset implementation to provide
 * O(1) insertion,
 * O(1) removal,
 * O(1) search,
 * O(n) listing elements, and
 * O(log n) selecting an element uniformly at random.
 */
class HashSelectableSet[T] extends SelectableSet[T] {
  private var power = 0

  private var numElements = 0

  private var arraySize: Int = _

  private var maxNumElements: Int = _

  private var minNumElements: Int = _

  private var binSizeLimit: Int = _

  private var contents: Array[List[T]] = _

  /**
   * Make a copy of this HashSelectableSet, returning a new HashSelectableSet
   */
  override def clone: SelectableSet[T] = {
    val result = new HashSelectableSet[T]
    result.power = power
    result.numElements = numElements
    result.arraySize = arraySize
    result.maxNumElements = maxNumElements
    result.minNumElements = minNumElements
    result.binSizeLimit = binSizeLimit
    result.contents = contents.clone
    result
  }

  // Used for testing
  private def report = (power, numElements, arraySize, maxNumElements, binSizeLimit, contents, minNumElements)

  private def calcVars() = {
    arraySize = scala.math.pow(2, power).toInt + 1 // surely there's a better way to do this
    maxNumElements = arraySize
    minNumElements = arraySize / 3
    binSizeLimit = power + 1
    contents = Array.fill(arraySize)(Nil)
  }

  calcVars()

  private def index(elem: T): Int = (elem.hashCode % arraySize).abs

  private def resizeHelper(elem: T): Unit = {
    val i = index(elem)
    contents(i) = elem :: contents(i)
  }

  private def resize(increase: Boolean): Unit = {
    val allElements = toList
    if (increase) power += 1; else power -= 1
    calcVars()
    allElements foreach (resizeHelper(_))
  }

  private def increaseSize() = resize(true)

  private def decreaseSize() = resize(false)

  override def toList = contents.toList.flatten

  def iterator = toList.iterator

  /**
   * Add an element to the set.
   */
  def +=(elem: T): this.type = {
    val i = index(elem)
    val binContents = contents(i)
    if (!binContents.contains(elem)) {
      if (numElements >= maxNumElements || binContents.length == binSizeLimit) {
        resize(true)
        add(elem)
      } else {
        contents(i) = elem :: binContents
        numElements += 1
      }
    }
    this
  }

  /**
   * Remove an element from the set.
   */
  def -=(elem: T): this.type = {
    val i = index(elem)
    val binContents = contents(i)
    if (binContents.contains(elem)) {
      if (numElements <= minNumElements) {
        resize(false)
        remove(elem)
      } else {
        contents(i) = binContents.filterNot(_ == elem)
        numElements -= 1
      }
    }
    this
  }

  /**
   * Test whether the set contains the given element.
   */
  def contains(elem: T): Boolean =
    contents(index(elem)).contains(elem)

  /*
   * select() selects an element uniformly at random. The challenge is that we cannot simply select a
   * bin and select an element in the bin, because an element in a bin that contains only one element
   * will be favored. Instead, we use the following rejection sampling scheme:
   *
   * Let M = binSizeLimit. Choose an integer i at random from 0 to M-1, and choose a bin b at
   * random. If n has more than i elements, choose an element uniformly from n. Otherwise, try again.
   * The probability of a successful choice of an element x in cell b with k elements is
   * P(i < k) 1/k = k/M 1/k = 1/M. Therefore the probability of choosing an element does not depend on
   * the number of elements in the cell and is uniform. The amount of time required for a successful
   * selection is on the order of M. Since M grows (at most) with log of the number N of elements, the selection
   * time is O(log(N)).
   */
  private def doSelect(): T = {
    val i = random.nextInt(binSizeLimit)
    val binContents = contents(random.nextInt(arraySize))
    val len = binContents.length
    if (len > i) binContents(random.nextInt(len))
    else doSelect()
  }

  /**
   * Select an element uniformly at random.
   */
  def select(): T =
    if (numElements == 0) throw new UnsupportedOperationException("Cannot call select on empty set")
    else doSelect()
}
