/*
 * MPEAlgorithm.scala
 * Algorithms that compute the most likely values of elements.
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Jan 1, 2009
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 */

package com.cra.figaro.algorithm

import com.cra.figaro.language._

/**
 * Algorithms that compute the most likely values of elements.
 */
abstract class MPEAlgorithm(val universe: Universe)
  extends Algorithm {
  /*
   * Particular implementations of algorithm must provide the following method.
   */

  /**
   * Returns the most likely value for the target element.
   */
  def mostLikelyValue[T](target: Element[T]): T
}
