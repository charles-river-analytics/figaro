/*
 * BooleanElement.scala
 * Elements over Booleans.
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Jan 1, 2009
 * 
 * Copyright 2017 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.library.compound

import com.cra.figaro.language._

/**
 * Elements over Booleans on which element-level Boolean operators are defined.
 */
class BooleanElement(elem: Element[Boolean]) {
  /**
   * Element that tests for the conjunction of this element with another element.
   */
  def &&(that: Element[Boolean]) = Apply(elem, that, (b1: Boolean, b2: Boolean) => b1 && b2)

  /**
   * Element that tests for the disjunction of this element with another element.
   */
  def ||(that: Element[Boolean]) = Apply(elem, that, (b1: Boolean, b2: Boolean) => b1 || b2)

  /**
   * Element that is the negation of this element.
   */
  def unary_! = Apply(elem, (b: Boolean) => !b)
}

