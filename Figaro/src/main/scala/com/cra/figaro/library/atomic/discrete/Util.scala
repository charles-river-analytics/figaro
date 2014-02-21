/*
 * Util.scala
 * Utility functions for atomic discrete elements.
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Feb 25, 2011
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.library.atomic.discrete

import com.cra.figaro.util._
import scala.math.{ ceil, log }

object Util {
  /**
   * Generate a geometric distributed random variable.
   */
  def generateGeometric(probFail: Double) =
    ceil(log(random.nextDouble()) / log(probFail)).toInt
}

