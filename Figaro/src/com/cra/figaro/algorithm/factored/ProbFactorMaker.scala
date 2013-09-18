/*
 * ProbFactorMaker.scala
 * Trait of elements for which factors can be created.
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Jan 1, 2009
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 */

package com.cra.figaro.algorithm.factored

/**
 * Trait of elements for which probabilistic factors can be created. Elements that implement this trait must
 * implement the makeFactors method.
 */
trait ProbFactorMaker {
  /**
   * Returns the factors corresponding to this element.
   */
  def makeFactors: List[Factor[Double]]
}