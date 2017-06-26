/*
 * LazyStructured.scala
 * Lazy structured algorithms.
 *
 * Created By:      William Kretschmer (kretsch@mit.edu)
 * Creation Date:   Jun 26, 2017
 *
 * Copyright 2017 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */
package com.cra.figaro.algorithm.structured.algorithm

import com.cra.figaro.algorithm.factored.ParticleGenerator
import com.cra.figaro.algorithm.structured.strategy.range.RangingStrategy

/**
 * Structured algorithms that are lazy. This changes the `checkConstraintBounds` method to requires all constraints to
 * be in the range [0.0, 1.0]. Additionally, one-time and anytime versions of the algorithm are modified to use the
 * respective lazy ranging strategies.
 */
trait LazyStructured extends StructuredAlgorithm {
  override def checkConstraintBounds(): Unit = {
    // Look at the non constraint factors of the components not fully enumerated. This check is necessary because
    // components not fully enumerated might later have constraints out of bounds when their ranges increase in size.
    for(comp <- problem.components if !comp.fullyEnumerated) {
      // Verify that all entries in the factors are in the range [0.0, 1.0].
      for(factor <- comp.constraintFactors() ; indices <- factor.getIndices) {
        val entry = factor.get(indices)
        require(0.0 <= entry && entry <= 1.0, s"constraint for element ${comp.element} out of bounds: $entry")
      }
    }
  }
}

trait AnytimeLazyStructured extends LazyStructured with AnytimeStructured {
  override val rangingStrategy = RangingStrategy.defaultLazy(1)
}

trait OneTimeLazyStructured extends LazyStructured with OneTimeStructured {
  override val rangingStrategy = RangingStrategy.defaultLazy(ParticleGenerator.defaultNumSamplesFromAtomics)
}
