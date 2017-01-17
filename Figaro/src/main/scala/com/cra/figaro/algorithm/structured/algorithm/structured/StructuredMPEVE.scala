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
import com.cra.figaro.algorithm.structured.algorithm._

class StructuredMPEVE(universe: Universe) extends StructuredMPEAlgorithm(universe) with DecompositionMPE {

  def solvingStrategy() = new ConstantStrategy(problem, structuredRaising, mpeVariableElimination)
}

object StructuredMPEVE {
  /** Create a structured variable elimination algorithm with the given query targets. */
  def apply()(implicit universe: Universe) = {        
    new StructuredMPEVE(universe)
  }

  /**
   * Use VE to compute the probability that the given element satisfies the given predicate.
   */
  def mostLikelyValue[T](target: Element[T]): T = {
    val alg = new StructuredMPEVE(target.universe)
    alg.start()
    val result = alg.mostLikelyValue(target)
    alg.kill()
    result
  }
}
