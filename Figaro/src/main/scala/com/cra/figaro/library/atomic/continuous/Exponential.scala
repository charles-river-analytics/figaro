/*
 * Exponential.scala
 * Elements representing exponential distributions.
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Feb 25, 2011
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.library.atomic.continuous

import com.cra.figaro.language._
import com.cra.figaro.util._
import scala.math.{ log, exp }

/**
 * An exponential distribution in which the parameter is a constant.
 */
class AtomicExponential(name: Name[Double], lambda: Double, collection: ElementCollection)
  extends Element[Double](name, collection) with Atomic[Double] {
  type Randomness = Double

  def generateRandomness() = Util.generateExponential(lambda)

  def generateValue(rand: Randomness) = rand

  /**
   * Density of a value.
   */
  def density(d: Double) = if (d < 0.0) 0.0 else lambda * exp(-lambda * d)

  override def toString = "Exponential(" + lambda + ")"
}

/**
 * An exponential distribution in which the parameter is an element.
 */
class CompoundExponential(name: Name[Double], lambda: Element[Double], collection: ElementCollection)
  extends NonCachingChain(
    name,
    lambda,
    (l: Double) => new AtomicExponential("", l, collection),
    collection) {
  override def toString = "Exponential(" + lambda + ")"
}

object Exponential extends Creatable {
  /**
   * Create an exponential distribution in which the parameter is a constant.
   */
  def apply(lambda: Double)(implicit name: Name[Double], collection: ElementCollection) =
    new AtomicExponential(name, lambda, collection)

  /**
   * Create an exponential distribution in which the parameter is an element.
   */
  def apply(lambda: Element[Double])(implicit name: Name[Double], collection: ElementCollection) =
    new CompoundExponential(name, lambda, collection)

  type ResultType = Double

  def create(args: List[Element[_]]) = apply(args(0).asInstanceOf[Element[Double]])
}
