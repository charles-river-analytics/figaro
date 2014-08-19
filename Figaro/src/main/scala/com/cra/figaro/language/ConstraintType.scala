package com.cra.figaro.language

import scala.math.log
import scala.math.exp


/*
 * Convenience class to place all constraints in logarithmic form
 * 
 * These classes don't need to be instantiated by the user, and are handled automatically in Figaro 
 */
abstract class ConstraintType[T] extends Function1[T, Double] {
  def apply(d: T): Double
}

/*
 * Case class for user defined constraints that are already in logarithimc form
 */
case class LogConstraintType[T](fcn: T => Double) extends ConstraintType[T] {
  def apply(d: T) = fcn(d)
}

/*
 * Case class for user defined constraints that are already in double form, converts to logs
 */
case class ProbConstraintType[T](fcn: T => Double) extends ConstraintType[T] {
  def apply(d: T) = log(fcn(d))
}