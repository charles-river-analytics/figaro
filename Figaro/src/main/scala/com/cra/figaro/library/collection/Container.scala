/*
 * Container.scala
 * Trait for a Process with a defined sequence of indices
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

import scala.language.implicitConversions

import com.cra.figaro.language._
import com.cra.figaro.library.atomic.discrete.FromRange
import com.cra.figaro.library.compound._

/**
 * A Container is a Process with a defined sequence of indices.
 */
trait Container[Index, Value]
extends Process[Index, Value] {
  val indices: Seq[Index]

  def rangeCheck(index: Index) = indices.contains(index)

  /**
   * Return all the elements in this container as an ordinary Scala Seq.
   * This method caches the elements, like apply, so the same elements will be returned every time.
   */
  def elements: Seq[Element[Value]] = indices.map(generateCached(_))

  /**
   * Convert this container into an ordinary Scala map.
   */
  def toMap: Map[Index, Element[Value]] =
    Map(indices.map(i => i -> apply(i)):_*)

  /**
   * Apply the given function to every value in this container, returning a new container.
   */
  override def map[Value2](f: Value => Value2): Container[Index, Value2] = {
    val thisContainer = this
    new Container[Index, Value2] {
      val indices = thisContainer.indices
      def generate(i: Index) = {
        val elem1 = thisContainer(i)
        Apply(elem1, f)("", elem1.universe)
      }
      def generate(indices: List[Index]) =
        thisContainer.generate(indices).mapValues((e: Element[Value]) => Apply(e, f)("", e.universe))
    }
  }

  /**
   * Chain every value in this container through the given function, returning a new container.
   */
  override def chain[Value2](f: Value => Element[Value2]): Container[Index, Value2] = {
    val thisContainer = this
    new Container[Index, Value2] {
      val indices = thisContainer.indices
      def generate(i: Index) = {
        val elem1 = thisContainer(i)
        Chain(elem1, f)("", elem1.universe)
      }
      def generate(indices: List[Index]) =
        thisContainer.generate(indices).mapValues((e: Element[Value]) => Chain(e, f)("", e.universe))
    }
  }
  
   /**
   * Chain every value in this container through the given function, returning a new container.
   */
  override def flatMap[Value2](f: Value => Element[Value2]): Container[Index, Value2] = { chain(f) }

  /**
   * Fold the values in this container through the given function.
   */
  def foldLeft[Value2](start: Value2)(f: (Value2, Value) => Value2)(implicit name: Name[Value2], collection: ElementCollection): Element[Value2] = {
    val myArgs = indices.map(apply(_)).toList
//    myArgs.foreach { arg => if (!arg.active) arg.activate }
    FoldLeft(start, f)(myArgs:_*)(name, collection)
  }

  /**
   * Fold the values in this container through the given function.
   */
  def foldRight[Value2](start: Value2)(f: (Value, Value2) => Value2)(implicit name: Name[Value2], collection: ElementCollection): Element[Value2] = {
    val myArgs = indices.map(apply(_)).toList
//    myArgs.foreach { arg => if (!arg.active) arg.activate }
    FoldRight(start, f)(myArgs:_*)(name, collection)
  }

  /**
   * Reduce the values in this container through the given function.
   */
  def reduce(f: (Value, Value) => Value)(implicit name: Name[Value], collection: ElementCollection): Element[Value] = {
    val myArgs = indices.map(apply(_)).toList
//    myArgs.foreach { arg => if (!arg.active) arg.activate }
    Reduce(f)(myArgs:_*)(name, collection)
  }

  /**
   * Aggregate the results of applying an operator to each element.
   */
  def aggregate[Value2](start: => Value2)(seqop: (Value2, Value) => Value2, combop: (Value2, Value2) => Value2)
  (implicit name: Name[Value2], collection: ElementCollection): Element[Value2] = {
    foldLeft(start)((v1: Value2, v2: Value) => combop(v1, seqop(v1, v2)))(name, collection)
  }

  /**
   * Returns an element representing the number of elements in the container whose values satisfy the predicate.
   */
  def count(f: (Value) => Boolean)(implicit name: Name[Int], collection: ElementCollection): Element[Int] = {
    foldLeft(0)((i: Int, v: Value) => if (f(v)) i + 1 else i)(name, collection)
  }

  /**
   * Returns an element representing whether the value of any element in the container satisfies the predicate.
   */
  def exists(pred: Value => Boolean)(implicit name: Name[Boolean], collection: ElementCollection): Element[Boolean] = {
    foldLeft(false)((b: Boolean, v: Value) => pred(v) || b)(name, collection)
  }

  /**
   * Returns an element representing whether the values of all elements in the container satisfy the predicate.
   */
  def forall(pred: Value => Boolean)(implicit name: Name[Boolean], collection: ElementCollection): Element[Boolean] = {
    foldLeft(true)((b: Boolean, v: Value) => pred(v) && b)(name, collection)
  }

  /**
   * Returns an element representing the optional index of the first element in the container whose value satisfies the predicate.
   * The result has value None if no element is found.
   */
  def findIndex(pred: Value => Boolean)(implicit name: Name[Option[Index]], collection: ElementCollection): Element[Option[Index]] = {
    val argMap = indices.map(arg => (arg, apply(arg))).toMap
    def step(oi: Option[Index], index: Index) = oi match {
      case Some(_) => oi
      case None => if (pred(argMap(index).value)) Some(index) else None
    }
    val myArgs = argMap.values.toList
//    myArgs.foreach { arg => if (!arg.active) arg.activate }
  	new Deterministic[Option[Index]](name, collection) {
      def args = myArgs
      def generateValue(): Option[Index] = {
        indices.foldLeft(None: Option[Index])(step)
      }
	  }
  }

  /**
   * Returns a new container containing the elements of this container and the argument.
   * If an index is defined in both container, the element of the argument is used.
   */
  def ++(that: Container[Index, Value]): Container[Index, Value] = {
    val thisContainer = this
    new Container[Index, Value] {
      val indices = {
        val fromThis = thisContainer.indices.filterNot(that.indices.contains(_))
        fromThis ++ that.indices
      }
      def generate(i: Index) =
        if (that.rangeCheck(i)) that.generate(i); else thisContainer.generate(i)
      def generate(indices: List[Index]) = {
        val (fromThatIndices, fromThisIndices) = indices.partition(that.rangeCheck(_))
        val fromThis = thisContainer.generate(fromThisIndices)
        val fromThat = that.generate(indices)
        fromThis ++ fromThat
      }
    }
  }

  /**
   * Choose a random element from this container.
   */
  def randomElement() = {
    val selector = FromRange(0, indices.length)
    Chain(selector, (i: Int) => apply(indices(i)))
  }
}
object Container {
  def apply[T](elements: Element[T]*): Container[Int, T] = {
    new FixedSizeArray(elements.size, (i: Int) => elements(i))
  }

  def apply[T](numItems: Element[Int], generator: Int => Element[T])(implicit name: Name[FixedSizeArray[T]], collection: ElementCollection): FixedSizeArrayElement[T] = {
    VariableSizeArray(numItems, generator)(name, collection)
  }

  implicit def toContainerElement[I, T](container: Container[I, T]): ContainerElement[I, T] = new ContainerElement(Constant(container))
}
