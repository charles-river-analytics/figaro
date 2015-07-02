/*
 * ParOneTime.scala
 * Parallel one-time algorithms.
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
import com.cra.figaro.algorithm.OneTime

/**
 * Parallel one-time sampling algorithms. These algorithms have multiple instances that execute across multiple threads.
 */
trait ParOneTime extends ParSamplingAlgorithm with OneTime {
  
  protected val parAlgs: ParSeq[OneTime]

  /**
   * Run the algorithms in parallel, performing their computations to completion.
   */
  override def run() = parAlgs foreach (_.run())

  /** Specify delegation of algorithm management to ParSamplingAlgorithm **/
  override protected[algorithm] def doStart(): Unit = super[ParSamplingAlgorithm].doStart()
  override protected[algorithm] def doStop(): Unit = super[ParSamplingAlgorithm].doStop()
  override protected[algorithm] def doResume(): Unit = super[ParSamplingAlgorithm].doResume()
  override protected[algorithm] def doKill(): Unit = super[ParSamplingAlgorithm].doKill()
}
