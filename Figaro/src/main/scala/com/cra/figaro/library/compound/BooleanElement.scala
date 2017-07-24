/*
 * BooleanElement.scala
 * Elements over Booleans.
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Jan 1, 2009
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
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
  def &&(that: Element[Boolean]) = And(elem, that)

  /**
   * Element that tests for the disjunction of this element with another element.
   */
  def ||(that: Element[Boolean]) = Or(elem, that)

  /**
   * Element that is the negation of this element.
   */
  def unary_! = Apply(elem, (b: Boolean) => !b)
}

class And(name: Name[Boolean], arg1: Element[Boolean], arg2: Element[Boolean], collection: ElementCollection)
  extends Apply2(name, arg1, arg2, (b1: Boolean, b2: Boolean) => b1 && b2, collection) {
  override def toString = "(" + arg1 + "&&" + arg2 + ")"
}

object And {
  /**
   * Short-circuiting boolean "and" function. This is short circuiting in the sense that computing the and function of
   * `false` and * returns `false`. This optimization applies only to lazy factored inference.
   */
  def apply(arg1: Element[Boolean], arg2: Element[Boolean])
           (implicit name: Name[Boolean], collection: ElementCollection): And = {
    new And(name, arg1, arg2, collection)
  }
}

class Or(name: Name[Boolean], arg1: Element[Boolean], arg2: Element[Boolean], collection: ElementCollection)
  extends Apply2(name, arg1, arg2, (b1: Boolean, b2: Boolean) => b1 || b2, collection) {
  override def toString = "(" + arg1 + "||" + arg2 + ")"
}

object Or {
  /**
   * Short-circuiting boolean "or" function. This is short circuiting in the sense that computing the or function of
   * `true` and * returns `true`. This optimization applies only to lazy factored inference.
   */
  def apply(arg1: Element[Boolean], arg2: Element[Boolean])
           (implicit name: Name[Boolean], collection: ElementCollection): Or = {
    new Or(name, arg1, arg2, collection)
  }
}
