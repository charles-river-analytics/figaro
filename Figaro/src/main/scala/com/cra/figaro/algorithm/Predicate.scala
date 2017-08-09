/*
 * Predicate.scala
 * Predicates on elements
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Jan 1, 2009
 * 
 * Copyright 2017 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.algorithm

import com.cra.figaro.language._

/**
 * A predicate defined on an element.
 */
case class Predicate[T](element: Element[T], fn: T => Boolean) {
  /**
   * Test whether the predicate is true for a value of the element.
   */
  def test: Boolean = fn(element.value)
}
