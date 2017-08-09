/*
 * Values.scala
 * Compute the range of values of elements.
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
import com.cra.figaro.library.compound._
import com.cra.figaro.util._
import scala.collection.mutable.Map
import com.cra.figaro.algorithm.lazyfactored._

/**
 * Object for computing the range of values of elements in a universe. If an element has an abstraction,
 * it computes the abstract values. Although the implementation uses lazy values, values are computed to
 * maximum depth, so that a set of ordinary values is returned.
 */
class Values private(results: com.cra.figaro.algorithm.lazyfactored.LazyValues) {
  def apply[T](element: Element[T]): Set[T] = { 
    val valueSet = results(element, Integer.MAX_VALUE) 
    valueSet.regularValues
  }
}


object Values {
  type ValuesGetter[T] = Element[T] => Set[T]

  /**
   * Create an object for computing the range of values of elements in the universe. This object is only
   * created once for a universe.
   */
  def apply(universe: Universe = Universe.universe) = {
    new Values(com.cra.figaro.algorithm.lazyfactored.LazyValues(universe))
  }
}
