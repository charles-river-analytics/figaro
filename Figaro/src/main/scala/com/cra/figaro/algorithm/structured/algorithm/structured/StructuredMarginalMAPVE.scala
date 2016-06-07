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
 * A structured marginal MAP algorithm that uses VE to compute MAP queries.
 * @param universe Universe on which to perform inference.
 * @param mapElements Elements for which to compute MAP queries. Elements not in this list are summed over.
 */
class StructuredMarginalMAPVE(universe: Universe, mapElements: List[Element[_]])
  extends StructuredMarginalMAPAlgorithm(universe, mapElements) {

  def run() {    
    val strategy = DecompositionStrategy.recursiveStructuredStrategy(problem, new MarginalMAPVEStrategy, defaultRangeSizer, Lower, false)
    strategy.execute(initialComponents)    
  }
}

object StructuredMarginalMAPVE {
  /**
   * Create a structured variable elimination algorithm with the given query targets.
   * @param mapElements Elements for which to compute MAP queries. Elements not in this list are summed over,
   * and cannot be queried.
   */
  def apply(mapElements: List[Element[_]])(implicit universe: Universe) = {        
    new StructuredMarginalMAPVE(universe, mapElements)
  }

  /**
   * Use variable elimination to compute the most likely value of the given element.
   * @param target Element for which to compute MAP value.
   * @param mapElements Additional elements to MAP. Elements not in this list are summed over.
   */
  def mostLikelyValue[T](target: Element[T], mapElements: List[Element[_]]): T = {
    val alg = new StructuredMarginalMAPVE(target.universe, (target :: mapElements).distinct)
    alg.start()
    val result = alg.mostLikelyValue(target)
    alg.kill()
    result
  }
  
    /**
   * Use variable elimination to compute the most likely value of the given element.
   * @param target Element for which to compute MAP value. All other elements in the universe are summed over.
   */
  def mostLikelyValue[T](target: Element[T]): T = {
    val alg = new StructuredMarginalMAPVE(target.universe, List(target))
    alg.start()
    val result = alg.mostLikelyValue(target)
    alg.kill()
    result
  }
}
