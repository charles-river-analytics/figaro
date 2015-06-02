/*
 * ParAnytime.scala
 * Parallel anytime algorithms
 *
 * Created By:      Lee Kellogg (lkellog@cra.com)
 * Creation Date:   May 11, 2015
 *
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.algorithm.sampling.parallel

import com.cra.figaro.algorithm.Anytime
import scala.collection.parallel.ParSeq

/**
 * Parallel anytime sampling algorithms. These algorithms have a parallel collection of algorithm instances 
 * that will do their work on separate threads, over separate universes.
 */
trait ParAnytime extends ParSamplingAlgorithm {
  
  protected val parAlgs: ParSeq[Anytime]

  /**
   * Run one step of the algorithm on each thread.
   */
  def runStep() = parAlgs foreach (_.runStep())

  /**
   * Call stopUpdate on algorithms in all threads.
   */
  def stopUpdate() = parAlgs foreach (_.stopUpdate())
  
  /**
   * Release all resources from this anytime algorithm.
   */
  def shutdown = parAlgs foreach (_.shutdown)
}
