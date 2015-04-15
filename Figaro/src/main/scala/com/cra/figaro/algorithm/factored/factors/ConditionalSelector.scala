/*
 * ConditionalSelector.scala
 * Implementation of conditional selector (in chain) using sparse
 * factors.
 * 
 * Created By:      Glenn Takata (gtakata@cra.com)
 * Creation Date:   Feb 20, 2015
 * 
 * Copyright 2015 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */package com.cra.figaro.algorithm.factored.factors

import scala.reflect.runtime.universe._

/**
 * ConditionalSelector Factor. A conditional selector factor is associated chain elements and represents the relationship
 * between parent elements and outcome elements. It is a special form of sparse factor where the default element is not 
 * 0 but 1 (don't care) which is the most frequent in this type of factor.
 * 
 * @author Glenn Takata Feb 20, 2015
 *
 * @param <T>
 */
class ConditionalSelector[T](_parents: List[Variable[_]], _output: List[Variable[_]], _semiring: Semiring[T] = SumProductSemiring().asInstanceOf[Semiring[T]])
  extends BasicFactor[T](_parents, _output, _semiring) {

  override def createFactor[T](_parents: List[Variable[_]], _output: List[Variable[_]], _semiring: Semiring[T] = semiring) = 
    new BasicFactor[T](_parents, _output, _semiring)

  /**
   * Get the value associated with a row. The row is identified by an list of indices
   * into the ranges of the variables over which the factor is defined. Rows with
   * default values will be missing, so supply the missing value for these rows
   */
  override def get(indices: List[Int]): T = {
    contents.get(indices) match {
      case Some(value) => value.asInstanceOf[T]
      case _ => semiring.one
    }
  }
  /**
   * Convert the contents of the target by applying the given function to all elements of this factor.
   */
  override def mapTo[U](fn: T => U, _semiring: Semiring[U] = semiring): Factor[U] = {
    val newFactor = new ConditionalSelector[U](parents, output, _semiring)
    for { (key, value) <- contents } {
      newFactor.set(key, fn(value))
    }
    newFactor
  }
}