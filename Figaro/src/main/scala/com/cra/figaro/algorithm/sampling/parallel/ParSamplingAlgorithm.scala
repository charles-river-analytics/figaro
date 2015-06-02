/*
 * ParAlgorithm.scala
 * Parallel algorithms.
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

import scala.collection.parallel.ParSeq
import com.cra.figaro.algorithm.Algorithm

/**
 * Parallel sampling algorithms. These algorithms have a parallel collection of algorithm instances
 * that will do their work on separate threads, over separate universes.
 */
trait ParSamplingAlgorithm extends Algorithm {

  protected val parAlgs: ParSeq[Algorithm]

  private def call(func: (Algorithm) => Unit) = parAlgs foreach func

  /**
   * Calls initialize() on all algorithms.
   */
  override def initialize(): Unit = call(_.initialize())

  /**
   * Calls cleanUp() on all algorithms.
   */
  override def cleanUp(): Unit = call(_.cleanUp())

  protected[algorithm] def doStart(): Unit = call(_.doStart())

  protected[algorithm] def doStop(): Unit = call(_.doStop())

  protected[algorithm] def doResume(): Unit = call(_.doResume())

  protected[algorithm] def doKill(): Unit = call(_.doKill())
}