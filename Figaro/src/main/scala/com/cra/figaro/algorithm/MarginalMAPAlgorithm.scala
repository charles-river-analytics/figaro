/*
 * MarginalMAPAlgorithm.scala
 * Algorithms that compute the most likely values of some elements, and marginalize over all other elements.
 * 
 * Created By:      William Kretschmer (kretsch@mit.edu)
 * Creation Date:   Jun 2, 2016
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.algorithm

import com.cra.figaro.language.Element
import com.cra.figaro.language.Universe

/**
 * Algorithms that compute the most likely values of some elements, and marginalize over all other elements.
 */
trait MarginalMAPAlgorithm extends Algorithm {
  val universe: Universe
  
  /**
   * Elements for which to perform MAP queries. This algorithm marginalizes over elements not in this list.
   */
  def mapElements: List[Element[_]]

  /**
   * Returns the most likely value for the target element.
   */
  def mostLikelyValue[T](target: Element[T]): T
  
  // Defined in one time or anytime versions
  protected def doMostLikelyValue[T](target: Element[T]): T
}