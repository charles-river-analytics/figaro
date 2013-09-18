/*
 * Variable.scala
 * Variables that appear in factors.
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Jan 1, 2009
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 */

package com.cra.figaro.algorithm.factored

import com.cra.figaro.algorithm._
import com.cra.figaro.language._
import scala.collection.mutable.Map

/**
 * Variables that appear in factors.
 * 
 * @param range The range of values of the variable
 */
class Variable[T](val range: List[T]) {
  type Value = T

  /**
   * The unique identifier of the variable.
   */
  val id = Variable.nextId()
  
  /**
   * Size of the range of the variable.
   */
  val size = range.size

  override def toString = range.toString
}

/**
 * Variables generated from elements.
 * 
 * @param range The range of values of the variable 
 */
class ElementVariable[T](val element: Element[T]) extends Variable(Values(element.universe)(element).toList) {
  override def toString = element.toString
}

/**
 * Variables generated from parameterized elements
 * 
 * @param range The range of values of the variable
 */
class ParameterizedVariable[T](override val element: Parameterized[T]) extends ElementVariable(element) {
  /**
   * Get the sufficient statistics for the parameter of this variable.
   */
  def sufficientStatistics(t: T) = {
    element.parameter.sufficientStatistics(t)
  }

  override def toString = "Parameterized variable:" + element.toString
}

/* Variables generated from sufficient statistics of parameters */
object Variable {
  // An element should always map to the same variable
  private val memoMake: Map[Element[_], Variable[_]] = Map()

  private var id: Int = 0

  private def nextId(): Int = {
    id += 1
    id
  }

  private def make[T](elem: Element[T]): Variable[T] = {
    // Make sure that the element will be removed from the memoMake map when it is inactivated
    elem.universe.register(memoMake)
    new ElementVariable(elem)
  }

  private def make[T](p: Parameterized[T]): Variable[T] = {
    // Make sure that the element will be removed from the memoMake map when it is inactivated
    p.universe.register(memoMake)
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

}

