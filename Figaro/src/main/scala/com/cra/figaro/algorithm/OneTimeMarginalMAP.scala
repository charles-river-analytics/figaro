/*
 * OneTimeMarginalMAP.scala
 * One time algorithms that compute the most likely values of some elements, and marginalize over all other elements.
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

/**
 * One-time algorithms that compute the most likely values of some elements, and marginalize over others.
 * A class that implements this trait must implement run and mostLikelyValue methods.
 */
trait OneTimeMarginalMAP extends MarginalMAPAlgorithm with OneTime {
  protected def doMostLikelyValue[T](target: Element[T]): T = mostLikelyValue(target)
}