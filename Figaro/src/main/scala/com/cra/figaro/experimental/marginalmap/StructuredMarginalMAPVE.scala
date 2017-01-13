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

package com.cra.figaro.experimental.marginalmap

import com.cra.figaro.algorithm.structured._
import com.cra.figaro.algorithm.structured.strategy.decompose._
import com.cra.figaro.language._

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
  def apply(mapElements: Element[_]*)(implicit universe: Universe) = {        
    new StructuredMarginalMAPVE(universe, mapElements.toList)
  }

  /**
   * Use variable elimination to compute the most likely value of the given element.
   * @param target Element for which to compute MAP value.
   * @param mapElements Additional elements to MAP. Elements not in this list are summed over.
   */
  def mostLikelyValue[T](target: Element[T], mapElements: Element[_]*): T = {
    val alg = StructuredMarginalMAPVE((target +: mapElements).distinct:_*)
    alg.start()
    val result = alg.mostLikelyValue(target)
    alg.kill()
    result
  }
}
