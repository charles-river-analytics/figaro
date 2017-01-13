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

package com.cra.figaro.experimental.normalproposals

import com.cra.figaro.language._
import com.cra.figaro.library.atomic.continuous.Util

import scala.math.{exp, log}

/**
 * An exponential distribution in which the parameter is a constant.
 */
class AtomicExponential(name: Name[Double], val lambda: Double, collection: ElementCollection)
  extends Element[Double](name, collection) with NormalProposer {
  // Lower bound for normal proposals
  override def lower = 0.0
  // Proposal scale is 20% of the standard deviation
  def proposalScale = 0.2 * lambda

  def generateRandomness() = Util.generateExponential(lambda)

  /**
   * Density of a value.
   */
  def density(d: Double) = if (d < 0.0) 0.0 else lambda * exp(-lambda * d)

  override def toString = "Exponential(" + lambda + ")"
}

object Exponential {
  /**
   * Create an exponential distribution in which the parameter is a constant.
   */
  def apply(lambda: Double)(implicit name: Name[Double], collection: ElementCollection) =
    new AtomicExponential(name, lambda, collection)
}
