package com.cra.figaro.library.process

import com.cra.figaro.language._
import scala.reflect.macros.Universe

/**
 * A Container is a Process with a defined sequence of indices.
 */
trait Container[Index, Value]
extends Process[Index, Value] {
  val indices: Seq[Index]
  
  def rangeCheck(index: Index) = indices.contains(index)

  /**
   * Apply the given function to every value in this container, returning a new container.
   */
  override def map[Value2](f: Value => Value2): Container[Index, Value2] = {
    val thisContainer = this 
    new Container[Index, Value2] {
      val indices = thisContainer.indices
      override val name: Name[_] = thisContainer.name + ".map"
      override val collection = thisContainer.collection 
      def generate(i: Index) = thisContainer(i).map(f)
      def generate(indices: List[Index]) =
        thisContainer.generate(indices).mapValues(_.map(f))
    }
  }
  
  /**
   * Chain every value in this container through the given function, returning a new container.
   */
  override def chain[Value2](f: Value => Element[Value2]): Container[Index, Value2] = {
    val thisContainer = this 
    new Container[Index, Value2] {
      val indices = thisContainer.indices
      override val name: Name[_] = thisContainer.name + ".map"
      override val collection = thisContainer.collection 
      def generate(i: Index) = thisContainer(i).flatMap(f)
      def generate(indices: List[Index]) =
        thisContainer.generate(indices).mapValues(_.flatMap(f))
    }
  }

  /**
   * Fold the values in this container through the given function.
   */
  def foldLeft[Value2](start: Value2)(f: (Value2, Value) => Value2): Element[Value2] = {
    val myArgs = indices.map(apply(_)).toList
    new Deterministic[Value2](name + ".foldLeft", collection) {
      def args = myArgs
      def generateValue(): Value2 = {
        args.map(_.value).foldLeft(start)(f)
      }
    }
  }

  /**
   * Fold the values in this container through the given function.
   */
  def foldRight[Value2](start: Value2)(f: (Value, Value2) => Value2): Element[Value2] = {
    val myArgs = indices.map(apply(_)).toList
    new Deterministic[Value2](name + ".foldRight", collection) {
      def args = myArgs
      def generateValue(): Value2 = {
        args.map(_.value).foldRight(start)(f)
      }
    }
  }
  
  /**
   * Reduce the values in this container through the given function.
   */
  def reduce(f: (Value, Value) => Value): Element[Value] = {
    val myArgs = indices.map(apply(_)).toList
    new Deterministic[Value](name + ".foldRight", collection) {
      def args = myArgs
      def generateValue(): Value = {
        args.map(_.value).reduce(f)
      }
    }
  }
  
  /**
   * Aggregate the results of applying an operator to each element.
   */
  def aggregate[Value2](start: => Value2)(seqop: (Value2, Value) => Value2, combop: (Value2, Value2) => Value2): Element[Value2] = {
    foldLeft(start)((v1: Value2, v2: Value) => combop(v1, seqop(v1, v2)))
  }
  
  /**
   * Returns an element representing the number of elements in the container whose values satisfy the predicate.
   */
  def count(f: (Value) => Boolean): Element[Int] = {
    foldLeft(0)((i: Int, v: Value) => if (f(v)) i + 1 else i)
  }
  
  /**
   * Returns an element representing whether the value of any element in the container satisfies the predicate.
   */
  def exists(pred: Value => Boolean): Element[Boolean] = {
    foldLeft(false)((b: Boolean, v: Value) => pred(v) || b)
  }
  
  /**
   * Returns an element representing whether the values of all elements in the container satisfy the predicate.
   */
  def forall(pred: Value => Boolean): Element[Boolean] = {
    foldLeft(true)((b: Boolean, v: Value) => pred(v) && b)
  }
  
  /**
   * Returns an element representing the optional index of the first element in the container whose value satisfies the predicate.
   * The result has value None if no element is found.
   */
  def findIndex(pred: Value => Boolean): Element[Option[Index]] = {
    val argMap = indices.map(arg => (arg, apply(arg))).toMap
    def step(oi: Option[Index], index: Index) = oi match {
      case Some(_) => oi
      case None => if (pred(argMap(index).value)) Some(index) else None
    }
	new Deterministic[Option[Index]](name + ".findIndex", collection) {
      def args = argMap.values.toList
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
      override val name: Name[_] = thisContainer.name + "++" + that.name 
      override val collection = that.collection 
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
}

object Container {
  def apply[Value](elements: Traversable[Element[Value]])(implicit name: Name[_], collection: ElementCollection): Container[Int, Value] = {
    val elementArray = elements.toArray
    new FixedIndependentArray[Value](
      elementArray.size,
      (index: Int) => elementArray(index),
      name,
      collection
    )
  }
}