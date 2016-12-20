/*
 * StructuredVE.scala
 * A structured variable elimination algorithm.
 *
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   March 1, 2015
 *
 * Copyright 2015 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.algorithm.structured.algorithm.structured

import com.cra.figaro.language._
import com.cra.figaro.algorithm.structured.solver._
import com.cra.figaro.algorithm.structured.strategy.solve._
import com.cra.figaro.algorithm.structured.algorithm.StructuredMPEAlgorithm
import com.cra.figaro.algorithm.factored.factors.MaxProductSemiring

class StructuredMPEBP(universe: Universe, iterations: Int) extends StructuredMPEAlgorithm(universe) {

  val semiring = MaxProductSemiring()

  def solvingStrategy() = new ConstantStrategy(problem, structured, mpeBeliefPropagation(iterations))
}

object StructuredMPEBP {
  /** Create a structured variable elimination algorithm with the given query targets. */
  def apply(iterations: Int)(implicit universe: Universe) = {        
    new StructuredMPEBP(universe, iterations)
  }

  /**
   * Use VE to compute the probability that the given element satisfies the given predicate.
   */
  def mostLikelyValue[T](target: Element[T], iterations: Int): T = {
    val alg = new StructuredMPEBP(target.universe, iterations)
    alg.start()
    val result = alg.mostLikelyValue(target)
    alg.kill()
    result
  }
}
