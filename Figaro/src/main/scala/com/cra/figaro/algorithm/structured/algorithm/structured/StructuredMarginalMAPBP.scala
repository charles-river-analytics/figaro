/*
 * StructuredMarginalMAPBP.scala
 * A structured belief propagation algorithm to compute marginal MAP queries.
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
import com.cra.figaro.algorithm.structured.strategy.solve.MarginalMAPBPStrategy
import com.cra.figaro.algorithm.structured.algorithm._
import com.cra.figaro.algorithm.structured.strategy.decompose._
import com.cra.figaro.algorithm.factored.factors.factory._
import com.cra.figaro.algorithm.factored.factors.MaxProductSemiring

/**
 * A structured marginal MAP algorithm that computes MAP values for permanent elements,
 * and marginalizes over temporary elements. Temporary elements are generally result
 * elements of a Chain. Note that result elements of a Chain can be permanent elements
 * if these elements are specified elsewhere in the model.
 * 
 * Each run of BP on a subproblem runs the algorithm for the specified number of iterations.
 */
class StructuredMarginalMAPBP(universe: Universe, iterations: Int) extends StructuredMarginalMAPAlgorithm(universe, universe.permanentElements) {

  val semiring = MaxProductSemiring()

  def run() {    
    val strategy = DecompositionStrategy.recursiveStructuredStrategy(problem, new MarginalMAPBPStrategy(iterations), defaultRangeSizer, Lower, false)
    strategy.execute(initialComponents)    
  }
}

object StructuredMarginalMAPBP {
  /**
   * Create a structured belief propagation algorithm with the given query targets.
   * This algorithm computes MAP queries for permanent elements in the universe, and
   * marginalizes over all temporary elements in the universe. Each run of BP on a
   * subproblem runs the algorithm for the specified number of iterations.
   */
  def apply(iterations: Int)(implicit universe: Universe) = {        
    new StructuredMarginalMAPBP(universe, iterations)
  }

  /**
   * Use belief propagation to compute the most likely value of the given element.
   * This algorithm computes MAP queries for permanent elements in the universe, and
   * marginalizes over all temporary elements in the universe. Each run of BP on a
   * subproblem runs the algorithm for the specified number of iterations.
   */
  def mostLikelyValue[T](target: Element[T], iterations: Int): T = {
    val alg = new StructuredMarginalMAPBP(target.universe, iterations)
    alg.start()
    val result = alg.mostLikelyValue(target)
    alg.kill()
    result
  }
}
