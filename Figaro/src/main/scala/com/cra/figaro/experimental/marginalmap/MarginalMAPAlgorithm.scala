/*
 * MarginalMAPAlgorithm.scala
 * Algorithms that compute the most likely values of some elements, and marginalize over all other elements.
 *
 * Created By:      William Kretschmer (kretsch@mit.edu)
 * Creation Date:   Jun 2, 2016
 *
 * Copyright 2016 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.experimental.marginalmap

import com.cra.figaro.algorithm.{Algorithm, AlgorithmException, AlgorithmInactiveException}
import com.cra.figaro.language._

/**
 * Algorithms that compute max a posteriori (MAP) values of some elements, and marginalize over all other elements.
 */
trait MarginalMAPAlgorithm extends Algorithm {
  class NotATargetException[T](target: Element[T]) extends AlgorithmException
  
  def universe: Universe
  
  /**
   * Elements for which to perform MAP queries. This algorithm marginalizes over elements not in this list.
   */
  def mapElements: Seq[Element[_]]
  
  /*
   * Particular implementations of algorithm must provide the following method.
   */
  def computeMostLikelyValue[T](target: Element[T]): T

  /*
   * Defined in one time and anytime marginal MAP versions of this class. Does not need to be defined
   * by particular algorithm implementations.
   */
  protected def doMostLikelyValue[T](target: Element[T]): T

  private def check[T](target: Element[T]): Unit = {
    if (!active) throw new AlgorithmInactiveException
    if (!(mapElements contains target)) throw new NotATargetException(target)
  }

  /**
   * Returns an estimate of the max a posteriori value of the target.
   * @throws NotATargetException if called on a target that is not in the list of MAP elements.
   * @throws AlgorithmInactiveException if the algorithm is inactive.
   */
  def mostLikelyValue[T](target: Element[T]): T = {
    check(target)
    doMostLikelyValue(target)
  }
  
  universe.registerAlgorithm(this)
}