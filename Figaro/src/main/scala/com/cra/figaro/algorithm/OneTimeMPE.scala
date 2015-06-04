/*
 * OneTimeMPE.scala
 * One-time algorithms that compute the most likely values of elements.
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
 * One-time algorithms that compute the most likely values of elements.
 * A class that implements this trait must implement run and mostLikelyValue methods.
 */
trait OneTimeMPE extends MPEAlgorithm with OneTime {
  protected def doMostLikelyValue[T](target: Element[T]): T = mostLikelyValue(target)
}
