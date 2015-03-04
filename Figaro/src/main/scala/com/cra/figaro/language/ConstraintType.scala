/*
 * ConstraintType.scala
 * Convenience class to place all constraints in logarithmic form
 * 
 * Created By:      Brian Ruttenberg (bruttenberg@cra.com)
 * Creation Date:   May 7, 2014
 * 
 * Copyright 2014 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.language

import scala.math.log
import scala.math.exp


/*
 * Convenience class to place all constraints in logarithmic form
 * 
 * These classes don't need to be instantiated by the user, and are handled automatically in Figaro 
 */
private[figaro] abstract class ConstraintType[T] extends Function1[T, Double] {
  def apply(d: T): Double
}

/*
 * Case class for user defined constraints that are already in logarithmic form
 */
private[figaro] case class LogConstraintType[T](fcn: T => Double) extends ConstraintType[T] {
  def apply(d: T) = fcn(d)
}

/*
 * Case class for user defined constraints that are already in double form, converts to logs
 */
private[figaro] case class ProbConstraintType[T](fcn: T => Double) extends ConstraintType[T] {
  def apply(d: T) = log(fcn(d))
}