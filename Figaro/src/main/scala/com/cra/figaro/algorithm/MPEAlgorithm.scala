/*
 * MPEAlgorithm.scala
 * Algorithms that compute the most likely values of elements.
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Jan 1, 2009
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.algorithm

import com.cra.figaro.language._

/**
 * Algorithms that compute the most likely values of elements.
 */
trait MPEAlgorithm extends Algorithm {
  val universe: Universe
  /*
   * Particular implementations of algorithm must provide the following method.
   */

  /**
   * Returns the most likely value for the target element.
   */
  def mostLikelyValue[T](target: Element[T]): T
  
  // Defined in one time or anytime versions
  protected def doMostLikelyValue[T](target: Element[T]): T
}
