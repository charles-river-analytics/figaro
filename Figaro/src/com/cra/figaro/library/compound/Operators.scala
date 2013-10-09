/*
 * Operators.scala
 * Operators on elements.
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Oct 18, 2011
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.library.compound

import com.cra.figaro.language._

/**
 * Element representing equality between two elements. You can use the === operator defined on
 * elements to create this element.
 */
class Eq[T](name: Name[Boolean], arg1: Element[T], arg2: Element[T], collection: ElementCollection)
  extends Apply2(name, arg1, arg2, (t1: T, t2: T) => t1 == t2, collection) {
  override def toString = arg1.toString + " === " + arg2.toString
}

/**
 * Element representing inequality between two elements. You can use the !== operator defined on
 * elements to create this element.
 */
class Neq[T](name: Name[Boolean], arg1: Element[T], arg2: Element[T], collection: ElementCollection)
  extends Apply2(name, arg1, arg2, (t1: T, t2: T) => t1 != t2, collection) {
  override def toString = arg1.toString + " !== " + arg2.toString
}
