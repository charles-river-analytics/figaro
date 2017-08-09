/*
 * DoubleElement.scala
 * Elements over Doubles.
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Mar 1, 2011
 * 
 * Copyright 2017 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.library.compound

import com.cra.figaro.language._

/**
 * Elements over Doubles on which element-level Double operators are defined.
 */
class DoubleElement(elem: Element[Double]) {
  /**
   * Element that adds another element to this element. The ++ notation is necessary to distinguish
   * from string addition.
   */
  def ++(that: Element[Double]) = Apply(elem, that, (d1: Double, d2: Double) => d1 + d2)

  /**
   * Element that subtracts another element from this element.
   */
  def -(that: Element[Double]) = Apply(elem, that, (d1: Double, d2: Double) => d1 - d2)

  /**
   * Element that multiplies this element with another element.
   */
  def *(that: Element[Double]) = Apply(elem, that, (d1: Double, d2: Double) => d1 * d2)

  /**
   * Element that divides this element from another element.
   */
  def /(that: Element[Double]) = Apply(elem, that, (d1: Double, d2: Double) => d1 / d2)

  /**
   * Element that represents the negative of this element.
   */
  def unary_- = Apply(elem, (d: Double) => -d)
}
