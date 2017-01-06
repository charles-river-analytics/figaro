/*
 * DecompositionStructuredAlgorithm.scala
 * One time structured algorithms that decompose the entire model.
 *
 * Created By:      William Kretschmer (kretsch@mit.edu)
 * Creation Date:   Jan 05, 2017
 *
 * Copyright 2017 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */
package com.cra.figaro.algorithm.structured.algorithm

import com.cra.figaro.algorithm.structured.strategy.refine._
import com.cra.figaro.language._

trait DecompositionStructuredAlgorithm extends OneTimeStructured {
  def initialElements: List[Element[_]] = {
    (problemTargets ::: universe.conditionedElements ::: universe.constrainedElements).distinct
  }

  override def refiningStrategy(): RefiningStrategy = {
    new BottomUpStrategy(problem, defaultRangeSizer, false, initialElements.map(collection(_)))
  }
}

trait DecompositionProbQuery extends OneTimeStructuredProbQuery with DecompositionStructuredAlgorithm

trait DecompositionMPE extends OneTimeStructuredMPE with DecompositionStructuredAlgorithm {
  override def initialElements = universe.permanentElements
}
