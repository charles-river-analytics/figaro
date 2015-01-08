/*
 * Process.scala
 * Trait to map indices to elements over values
 *
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Oct 14, 2014
 *
 * Copyright 2014 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.library.collection

import com.cra.figaro.language._
import com.cra.figaro.util.memo

/**
 * A Process maps indices to elements over values.
 *
 * Elements, of course, have a universe. Wherever possible, operations on processes, such as getting elements, or mapping or chaining through
 * a function, produce an element in the same universe as the original element.
 * An exception is an element over an optional value created for an index out of range, which goes in the default universe,
 * because there is no source element.
 * Another exception is a fold operation on a container, which is given a universe (defaults to the current Universe.universe).
 */
trait Process[Index, Value] {
  /**
   * Thrown if the index does not have an element.
   */
  case class IndexOutOfRangeException(index: Index) extends RuntimeException

  /**
   * Check whether the given index has an element.
   */
  def rangeCheck(index: Index): Boolean

  /**
   * Produce the elements representing the value of the process at the given indices.
   * Ensures that any dependencies between the elements are represented.
   * This method must be implemented by implementations of Process.
   * The return value maps each provided index to the corresponding element.
   * This method can assume that the indices has already been range checked.
   */
  def generate(indices: List[Index]): Map[Index, Element[Value]]

  /**
   * Produce the element corresponding to the value of the process at the given index.
   * This method can assume that the indices has already been range checked.
   */
  def generate(index: Index): Element[Value]

  private val cachedElements = scala.collection.mutable.Map[Index, Element[Value]]()

  private[collection] def generateCached(index: Index): Element[Value] = {
    cachedElements.getOrElseUpdate(index, generate(index))
  }

  /**
   * Get an element representing the value of the process at the given index.
   * Throws IndexOutOfRangeException if the index has no value.
   *
   * This apply method is cached, so calling process(index) always returns the same element.
   */
  def apply(index: Index): Element[Value] = {
    if (!rangeCheck(index)) throw IndexOutOfRangeException(index)
    generateCached(index)
  }

  /**
   * Get the elements representing the value of the process at the given indices.
   * Throws IndexOutOfRangeException if any index has no value.
   */
  def apply(indices: Traversable[Index]): Map[Index, Element[Value]] = {
    for { index <- indices } {
      if (!rangeCheck(index)) throw IndexOutOfRangeException(index)
    }
    generate(indices.toList)
  }

  /**
   * Safely get an element over an optional value at the index.
   * If the index is in range, the value of the element will be Some(something).
   * If the index is out of range, the value of the element will be None.
   */
  def get(index: Index): Element[Option[Value]] = {
    try {
      val elem = apply(index)
      new Apply1("", apply(index), (v: Value) => Some(v), elem.universe)
    } catch {
      case _: IndexOutOfRangeException => Constant(None)
    }
  }

  /**
   * Safely get the elements over optional values at all of the indices.
   * Any index that is not in range will always have value None.
   * Dependencies between elements for indices in range will be produced.
   */
  def get(indices: Traversable[Index]): Map[Index, Element[Option[Value]]] = {
    val (inRange, outOfRange) = indices.partition(rangeCheck(_))
    val inRangeElems: Map[Index, Element[Value]] = generate(inRange.toList)
    val inRangeOptPairs: List[(Index, Element[Option[Value]])] =
      for { (index, elem) <- inRangeElems.toList } yield {
        val elem2: Element[Option[Value]] = new Apply1("", elem, (v: Value) => Some(v), elem.universe)
        (index, elem2)
      }

    val outOfRangeOptPairs: List[(Index, Element[Option[Value]])] = {
      for { i <- outOfRange.toList } yield {
        val elem: Element[Option[Value]] = new Constant("", None, Universe.universe)
        (i, elem)
      }
    }
    Map((inRangeOptPairs ::: outOfRangeOptPairs):_*)
  }

  /**
   * Apply the given function to every value in this process, returning a new process.
   */
  def map[Value2](f: Value => Value2): Process[Index, Value2] = {
    val thisProcess = this
    new Process[Index, Value2] {
      def generate(i: Index) = {
        val elem1 = thisProcess(i)
        Apply(elem1, f)("", elem1.universe)
      }
      def generate(indices: List[Index]) =
        thisProcess.generate(indices).mapValues((e: Element[Value]) => Apply(e, f)("", e.universe))
      def rangeCheck(i: Index) = thisProcess.rangeCheck(i)
    }
  }

  /**
   * Chain every value in this process through the given function, returning a new process.
   */
  def chain[Value2](f: Value => Element[Value2]): Process[Index, Value2] = {
    val thisProcess = this
    new Process[Index, Value2] {
      def generate(i: Index) = {
        val elem1 = thisProcess(i)
        Chain(elem1, f)("", elem1.universe)
      }
      def generate(indices: List[Index]) =
        thisProcess.generate(indices).mapValues((e: Element[Value]) => Chain(e, f)("", e.universe))
      def rangeCheck(i: Index) = thisProcess.rangeCheck(i)
    }
  }

  /**
   * Returns a new process containing the elements of this process and the argument.
   * If an index is defined in both processes, the element of the argument is used.
   */
  def ++(that: Process[Index, Value]): Process[Index, Value] = {
    val thisProcess = this
    new Process[Index, Value] {
      def generate(i: Index) =
        if (that.rangeCheck(i)) that.generate(i); else thisProcess.generate(i)
      def generate(indices: List[Index]) = {
        val (fromThatIndices, fromThisIndices) = indices.partition(that.rangeCheck(_))
        val fromThis = thisProcess.generate(fromThisIndices)
        val fromThat = that.generate(indices)
        fromThis ++ fromThat
      }
      def rangeCheck(i: Index) = that.rangeCheck(i) || thisProcess.rangeCheck(i)
    }
  }
}
