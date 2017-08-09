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

import com.cra.figaro.algorithm.lazyfactored._
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

/**
 * Trait for short-circuiting boolean functions. This implements an optimization on extended values for lazy factored
 * inference: the result of applying the operator to * and a regular argument value can result in a regular result value
 * if the result value is uniquely determined by the regular argument value.
 */
trait BooleanOperator extends Apply2[Boolean, Boolean, Boolean] {
  /**
   * Short-circuiting boolean function on extended values.
   */
  def extendedFn(xv1: Extended[Boolean], xv2: Extended[Boolean]): Extended[Boolean]
}

class And(name: Name[Boolean], arg1: Element[Boolean], arg2: Element[Boolean], collection: ElementCollection)
  extends Apply2(name, arg1, arg2, (b1: Boolean, b2: Boolean) => b1 && b2, collection) with BooleanOperator {
  override def toString = "(" + arg1 + "&&" + arg2 + ")"

  override def extendedFn(xv1: Extended[Boolean], xv2: Extended[Boolean]): Extended[Boolean] = (xv1, xv2) match {
    case (Regular(v1), Regular(v2)) => Regular(v1 && v2)
    case (Regular(false), Star()) => Regular(false)
    case (Star(), Regular(false)) => Regular(false)
    case _ => Star()
  }
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
  extends Apply2(name, arg1, arg2, (b1: Boolean, b2: Boolean) => b1 || b2, collection) with BooleanOperator {
  override def toString = "(" + arg1 + "||" + arg2 + ")"

  override def extendedFn(xv1: Extended[Boolean], xv2: Extended[Boolean]): Extended[Boolean] = (xv1, xv2) match {
    case (Regular(v1), Regular(v2)) => Regular(v1 || v2)
    case (Regular(true), Star()) => Regular(true)
    case (Star(), Regular(true)) => Regular(true)
    case _ => Star()
  }
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
