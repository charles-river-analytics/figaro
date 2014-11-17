package com.cra.figaro.language

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
