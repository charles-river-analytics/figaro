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

/**
 * One time structured algorithms that use a single bottom-up refining strategy to decompose the entire model. This uses
 * the default range sizer, and does not create parameterized factors.
 */
trait DecompositionStructuredAlgorithm extends OneTimeStructured {
  /**
   * Initial elements to pass to the bottom-up strategy for decomposition. Defaults to a list containing all problem
   * targets and all evidence elements in the universe.
   */
  def initialElements: List[Element[_]] = {
    (problemTargets ::: universe.conditionedElements ::: universe.constrainedElements).distinct
  }

  override def initialize(): Unit = {
    super.initialize()
    collection.useSingleChainFactor = true
  }

  override def refiningStrategy(): RefiningStrategy = {
    new BottomUpStrategy(problem, defaultRangeSizer, initialElements.map(collection(_)))
  }
}

trait DecompositionProbQuery extends OneTimeStructuredProbQuery with DecompositionStructuredAlgorithm

trait DecompositionMPE extends OneTimeStructuredMPE with DecompositionStructuredAlgorithm {
  // For MPE, any permanent element can be queried, so they must all passed as initial elements for decomposition.
  override def initialElements = universe.permanentElements
}
