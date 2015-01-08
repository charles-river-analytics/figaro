/*
 * LazyAlgorithm.scala
 * Lazy algorithms.
 *
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Dec 28, 2013
 *
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.algorithm

/**
 * A lazy algorithm is an algorithm that can be run to increasing depths.
 */
trait LazyAlgorithm extends Algorithm {
  /**
   * The current depth to which the algorithm should be run.
   */
  var depth = 0

  /**
   * Run the algorithm to the given depth.
   */
  def run(depth: Int): Unit

  /**
   * Start the algorithm. This will run the algorithm to one depth.
   */
  def doStart() { pump() }

  /**
   * Increase the depth and run the algorithm again.
   */
  def pump() { depth += 1; run(depth) }

  /**
   * Stop the algorithm.
   */
  def doStop() { }

  /**
   * Resume the algorithm by increasing the depth and running again.
   */
  def doResume() { pump() }

  /**
   * Kill the algorithm.
   */
  def doKill() { depth = -1 }
}
