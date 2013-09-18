/*
 * IntElement.scala
 * Elements over Ints.
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Mar 1, 2011
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 */

package com.cra.figaro.library.compound

import com.cra.figaro.language._

/**
 * Elements over Ints on which element-level Int operators are defined.
 */
class IntElement(elem: Element[Int]) {
  /**
   * Element that adds another element to this element. The ++ notation is necessary to distinguish
   * from string addition.
   */
  def ++(that: Element[Int]) = Apply(elem, that, (i1: Int, i2: Int) => i1 + i2)

  /**
   * Element that subtracts another element from this element.
   */
  def -(that: Element[Int]) = Apply(elem, that, (i1: Int, i2: Int) => i1 - i2)

  /**
   * Element that multiplies this element with another element.
   */
  def *(that: Element[Int]) = Apply(elem, that, (i1: Int, i2: Int) => i1 * i2)

  /**
   * Element that divides this element from another element.
   */
  def /(that: Element[Int]) = Apply(elem, that, (i1: Int, i2: Int) => i1 / i2)

  /**
   * Element that represents this element module another element.
   */
  def %(that: Element[Int]) = Apply(elem, that, (i1: Int, i2: Int) => i1 % i2)

  /**
   * Element that represents the negative of this element.
   */
  def unary_- = Apply(elem, (i: Int) => -i)
}