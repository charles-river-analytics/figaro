package com.cra.figaro.language

trait HasDensity[T] extends Element[T] {
  /** The probability density of a value. */
  def density(t: T): Double
}