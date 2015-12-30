package com.cra.figaro.algorithm.factored.factors.factory

import com.cra.figaro.algorithm.factored.factors.Factor

/**
 * A trait for elements that are able to construct their own Factor.
 */
trait FactorMaker[T] {
  def makeFactors[T]: List[Factor[Double]]
}
