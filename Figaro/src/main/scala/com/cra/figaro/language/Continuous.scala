/*
 * Continuous.scala
 * Trait for TBD
 * 
 * Created By:      Synapski (no e-mail)
 * Creation Date:   Oct 6, 2014
 * 
 * Copyright 2014 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.language

/**
 * Doc needed
 */
trait Continuous[T] extends Element[T] {

  /**
   * Log-likelihood of a value.
   */
  def logp(value: T): Double

  override def observe(value: T) {
    addLogConstraint { _ => logp(value) }
    set(value)
  }

  override def unobserve() {
    unset()
  }

}
