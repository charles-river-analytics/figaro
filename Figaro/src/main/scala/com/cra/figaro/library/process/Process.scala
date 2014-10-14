package com.cra.figaro.library.process

import com.cra.figaro.language._
import com.cra.figaro.util.memo


/**
 * A Process maps indices to elements over values.
 */
trait Process[Index, Value] {
  val name: Name[_] = ""
  val collection: ElementCollection = Universe.universe

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
  
  /**
   * Get an element representing the value of the process at the given index.
   * Throws IndexOutOfRangeException if the index has no value.
   */
  def apply(index: Index): Element[Value] = {
    if (!rangeCheck(index)) throw IndexOutOfRangeException(index)
    generate(index)
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
      Apply(apply(index), (v: Value) => Some(v))
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
    val inRangeOpts: Map[Index, Element[Option[Value]]] = 
      inRangeElems.mapValues(elem => {
        val optElem: Element[Option[Value]] = Apply(elem, (v: Value) => Some(v))
        optElem 
      }) 
    val pairs: Seq[(Index, Element[Option[Value]])] = {
      for { i <- outOfRange.toList } yield {
        val elem: Element[Option[Value]] = Constant(None)
        (i, elem)
      }
    }
    inRangeOpts ++ Map(pairs:_*)
  }
  
  /**
   * Apply the given function to every value in this process, returning a new process.
   */
  def map[Value2](f: Value => Value2): Process[Index, Value2] = {
    val thisProcess = this 
    new Process[Index, Value2] {
      override val name: Name[_] = thisProcess.name + ".map"
      override val collection = thisProcess.collection 
      def generate(i: Index) = thisProcess(i).map(f)
      def generate(indices: List[Index]) =
        thisProcess.generate(indices).mapValues(_.map(f))
      def rangeCheck(i: Index) = thisProcess.rangeCheck(i)
    }
  }
  
  /**
   * Chain every value in this process through the given function, returning a new process.
   */
  def chain[Value2](f: Value => Element[Value2]): Process[Index, Value2] = {
    val thisProcess = this 
    new Process[Index, Value2] {
      override val name: Name[_] = thisProcess.name + ".map"
      override val collection = thisProcess.collection 
      def generate(i: Index) = thisProcess(i).flatMap(f)
      def generate(indices: List[Index]) =
        thisProcess.generate(indices).mapValues(_.flatMap(f))
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
      override val name: Name[_] = thisProcess.name + "++" + that.name 
      override val collection = that.collection 
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