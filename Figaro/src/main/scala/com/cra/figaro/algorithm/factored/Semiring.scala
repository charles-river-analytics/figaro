/*
 * Semiring.scala
 * Sum and product operations according to a semiring algebraic structure.
 * 
 * Created By:      Michael Howard (mhoward@cra.com)
 * Creation Date:   Jun 3, 2013
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.algorithm.factored

/**
 * Operations in factored algorithms are defined by a semiring algebraic structure. 
 * Each semiring defines a product and sum operation, and a value for zero and one which satisfy a set of properties. 
 * Different semirings are appropriate for certain algorithms and data types
 */
trait Semiring[T] {
  /**
   * Product of two entries, as defined by the particular inference problem.
   */
  def product(x: T, y: T): T

  /**
   * Sum of two entries, as defined by the particular inference problem. E.g., for computing conditional probabilities,
   * sum is ordinary addition; for most probable explanation, it is max.
   */
  def sum(x: T, y: T): T

  /**
   * A value such that a + 0 = a
   */
  val zero: T
  /**
   * A value such that a*1 = a
   */
  val one: T

}

object SumProductUtilitySemiring extends Semiring[(Double, Double)] {
  /**
   * Decision joint factor combination.
   */
  def product(x: (Double, Double), y: (Double, Double)) = (x._1 * y._1, x._2 + y._2)

  /**
   * Decision joint factor marginalization
   */
  def sum(x: (Double, Double), y: (Double, Double)) = if (x._1 + y._1 != 0.0) (x._1 + y._1, (x._1 * x._2 + y._1 * y._2) / (x._1 + y._1)); else (0.0, 0.0)

  /**
   * 0 probability and 0 utility
   */
  val zero = (0.0, 0.0)
  /**
   * 1 probability and 0 utility 
   */
  val one = (1.0, 0.0)

}

object BooleanSemiring extends Semiring[Boolean] {
  /**
   * x AND y
   */  
  def product(x: Boolean, y: Boolean): Boolean = x && y
  /**
   * x OR y
   */
  def sum(x: Boolean, y: Boolean): Boolean = x || y
  /**
   * False
   */
  val zero = false
  /**
   * True
   */
  val one = true
}

object SumProductSemiring extends Semiring[Double] {
  /**
   * Standard multiplication
   */
  def product(x: Double, y: Double) = x * y
  /**
   * Standard addition
   */
  def sum(x: Double, y: Double) = x + y
  /**
   * 0
   */
  val zero = 0.0
  /**
   * 1
   */
  val one = 1.0
}

object MaxProductSemiring extends Semiring[Double] {
  /**
   * Standard multiplication
   */
  def product(x: Double, y: Double) = x * y
  /**
   * The maximum of x and y.
   */
  def sum(x: Double, y: Double) = x max y
  /**
   * 0
   */
  val zero = 0.0
  /**
   * 1
   */
  val one = 1.0
}

