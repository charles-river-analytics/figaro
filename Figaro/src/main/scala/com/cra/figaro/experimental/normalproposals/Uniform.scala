/*
 * Uniform.scala
 * Elements representing continuous uniform distributions.
 *
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Oct 18, 2010
 *
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.experimental.normalproposals

import com.cra.figaro.language._
import com.cra.figaro.util.{bound, random}

import scala.math.log

/**
 * A continuous uniform distribution in which the parameters are constants.
 */
class AtomicUniform(name: Name[Double], override val lower: Double, override val upper: Double, collection: ElementCollection)
  extends Element[Double](name, collection) with NormalProposer {

  private lazy val diff = upper - lower

  // Proposal scale is 20% of the standard deviation
  def proposalScale = 0.2 * diff / math.sqrt(12)

  def generateRandomness() = random.nextDouble() * diff + lower

  private lazy val constantDensity = 1.0 / diff

  def density(d: Double) = if (d >= lower && d < upper) constantDensity; else 0.0

  override def toString = "Uniform(" + lower + ", " + upper + ")"
}

object Uniform {
  /**
   * Create a continuous uniform distribution in which the parameters are constants.
   */
  def apply(lower: Double, upper: Double)(implicit name: Name[Double], collection: ElementCollection) =
    new AtomicUniform(name, lower, upper, collection)
}
