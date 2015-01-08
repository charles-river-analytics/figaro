/*
 * Schedule.scala
 * A Schedule for annealing
 * 
 * Created By:      Brian Ruttenberg (bruttenberg@cra.com)
 * Creation Date:   Mar 1, 2013
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.algorithm.sampling

/**
 *  The schedule class determines the annealing schedule in the annealer.
 */
class Schedule(sch: (Double, Int) => Double) {
  /**
   * Return the temperature given the current temperature and sampler iteration.
   */
  def temperature(current: Double, iter: Int) = sch(current, iter)
}

object Schedule {

  // the no annealing schedule, used for burn-in
  /**
   * A schedule that performs no annealing (always returns 1.0).
   */
  val schedule = new Schedule((c: Double, i: Int) => 1.0)

  /**
   * The default schedule used the standard logarithmic schedule, where calling default with a
   * double will divide the log score by the parameter k.
   */
  def default(k: Double = 1.0) = new Schedule((c: Double, i: Int) => math.log(i.toDouble + 1.0) / k)

  /**
   * Create a new schedule from the supplied function.
   */
  def apply(sch: (Double, Int) => Double) = new Schedule(sch)

}
