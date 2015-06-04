/*
 * OneTime.scala
 * One-time algorithms.
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

/**
 * A one-time algorithm runs in a single step to produce its answers. It cannot be run again to improve the
 * answers. Implementations of OneTime must implement the run method.
 */
trait OneTime extends Algorithm {
  /**
   * Run the algorithm, performing its computation to completion.
   */
  def run(): Unit

  protected[algorithm] def doStart() = {
    initialize()
    run()
  }

  protected[algorithm] def doStop() = ()

  protected[algorithm] def doResume() = ()

  protected[algorithm] def doKill() = {
    cleanUp()
  }
}
