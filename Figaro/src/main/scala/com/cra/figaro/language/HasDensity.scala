/*
 * HasDensity.scala
 * Trait for TBD
 *
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Jun 10, 2014
 *
 * Copyright 2014 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.language

/**
 * Trait of elements for which a density method is defined.
 */
trait HasDensity[T] extends Element[T] {
  /** The probability density of a value. */
  def density(t: T): Double
}
