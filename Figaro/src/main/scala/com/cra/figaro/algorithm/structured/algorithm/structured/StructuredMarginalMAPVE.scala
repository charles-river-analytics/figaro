/*
 * StructuredMarginalMAPVE.scala
 * A structured variable elimination algorithm to compute marginal MAP queries.
 *
 * Created By:      William Kretschmer (kretsch@mit.edu)
 * Creation Date:   Jun 3, 2016
 *
 * Copyright 2015 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.algorithm.structured.algorithm.structured

import com.cra.figaro.language._
import com.cra.figaro.algorithm.structured._
import com.cra.figaro.algorithm.structured.strategy._
import com.cra.figaro.algorithm.structured.solver._
import com.cra.figaro.algorithm.structured.strategy.solve.MarginalMAPVEStrategy
import com.cra.figaro.algorithm.structured.algorithm._
import com.cra.figaro.algorithm.structured.strategy.decompose._
import com.cra.figaro.algorithm.factored.factors.factory._
import com.cra.figaro.algorithm.factored.factors.MaxProductSemiring

/**
 * A structured marginal MAP algorithm that computes MAP values for permanent elements,
 * and marginalizes over temporary elements. Temporary elements are generally result
 * elements of a Chain. Note that result elements of a Chain can be permanent elements
 * if these elements are specified elsewhere in the model.
 */
class StructuredMarginalMAPVE(universe: Universe) extends StructuredMarginalMAPAlgorithm(universe, universe.permanentElements) {

  val semiring = MaxProductSemiring()

  def run() {    
    val strategy = DecompositionStrategy.recursiveStructuredStrategy(problem, new MarginalMAPVEStrategy, defaultRangeSizer, Lower, false)
    strategy.execute(initialComponents)    
  }
}

object StructuredMarginalMAPVE {
  /**
   * Create a structured variable elimination algorithm with the given query targets.
   * This algorithm computes MAP queries for permanent elements in the universe, and
   * marginalizes over all temporary elements in the universe.
   */
  def apply()(implicit universe: Universe) = {        
    new StructuredMarginalMAPVE(universe)
  }

  /**
   * Use variable elimination to compute the most likely value of the given element.
   * This algorithm computes MAP queries for permanent elements in the universe, and
   * marginalizes over all temporary elements in the universe.
   */
  def mostLikelyValue[T](target: Element[T]): T = {
    val alg = new StructuredMarginalMAPVE(target.universe)
    alg.start()
    val result = alg.mostLikelyValue(target)
    alg.kill()
    result
  }
}
