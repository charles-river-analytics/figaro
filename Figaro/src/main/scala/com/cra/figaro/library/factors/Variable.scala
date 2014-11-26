/*
 * Variable.scala
 * Variables that appear in factors.
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Jan 1, 2009
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.algorithm.factored

import com.cra.figaro.algorithm._
import com.cra.figaro.language._
import scala.collection.mutable.Map
import com.cra.figaro.algorithm.lazyfactored.{ LazyValues, Extended, ValueSet }

/**
 * Variables that appear in factors.
 *
 * @param range The range of values of the variable
 */
class Variable[T](val valueSet: ValueSet[T]) {
  type Value = T

  val range: List[Extended[T]] = valueSet.xvalues.toList

  /**
   * The unique identifier of the variable.
   */
  val id = Variable.nextId()

  /**
   * Size of the range of the variable.
   */
  val size = range.size

  /**
   * Override equality of Variable. Variables are the same if their id's are the same
   */
  override def equals(o: Any) = o match {
    case that: Variable[T] => that.id == id
    case _ => false
  }
  override def hashCode = id

  override def toString = range.toString
}

/**
 * Variables generated from elements.
 *
 * @param range The range of values of the variable
 */
class ElementVariable[T](val element: Element[T]) extends Variable(LazyValues(element.universe).storedValues(element)) {

  override val id = Variable.nextId(element)

  override def toString = element.toString
}

/**
 * Variables generated from parameterized elements
 *
 * @param range The range of values of the variable
 */
class ParameterizedVariable[T](override val element: Parameterized[T]) extends ElementVariable(element) {
  override def toString = "Parameterized variable:" + element.toString
}

/* Variables generated from sufficient statistics of parameters */
object Variable {
  // An element should always map to the same variable
  private val memoMake: Map[Element[_], Variable[_]] = Map()

  // Make sure to register this map (or replace the memoMake)
  private val idCache: Map[Element[_], Int] = Map()

  private var id: Int = 0

  def nextId(elem: Element[_]): Int = {
    idCache.get(elem) match {
      case Some(id) => id
      case None =>
        val id = nextId()
        idCache += (elem -> id)
        id
    }
  }

  private def nextId(): Int = {
    id += 1
    id
  }

  private def make[T](elem: Element[T]): Variable[T] = {
    // Make sure that the element will be removed from the memoMake map when it is inactivated
    elem.universe.register(memoMake)
    elem.universe.register(idCache)
    new ElementVariable(elem)
  }

  private def make[T](p: Parameterized[T]): Variable[T] = {
    // Make sure that the element will be removed from the memoMake map when it is inactivated
    p.universe.register(memoMake)
    p.universe.register(idCache)
    new ParameterizedVariable(p)
  }

  /**
   * Create the variable associated with an element. This method is memoized.
   */
  def apply[T](elem: Element[T]): Variable[T] =
    memoMake.get(elem) match {
      case Some(v) => v.asInstanceOf[Variable[T]]
      case None =>
        val result = make(elem)
        memoMake += elem -> result
        result
    }

  def apply[T](elem: Parameterized[T]): Variable[T] =
    memoMake.get(elem) match {
      case Some(v) => v.asInstanceOf[Variable[T]]
      case None =>
        val result = make(elem)
        memoMake += elem -> result
        result
    }

  def clearCache() { memoMake.clear() }
}
