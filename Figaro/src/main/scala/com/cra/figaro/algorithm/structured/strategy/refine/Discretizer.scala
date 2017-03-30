/*
 * Discretizer.scala
 * Discretization for distributions of atomic elements.
 *
 * Created By:      William Kretschmer (kretsch@mit.edu)
 * Creation Date:   Mar 30, 2017
 *
 * Copyright 2017 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */
package com.cra.figaro.algorithm.structured.strategy.refine

import com.cra.figaro.algorithm.lazyfactored.Extended
import com.cra.figaro.language._

/**
 * Class for producing discrete approximations to distributions for atomic elements. This class can be mutable so that
 * repeated calls to `discretize()` return successively better approximations.
 * @param atomic Atomic element for which to produce a distribution.
 */
abstract class Discretizer[T](atomic: Atomic[T]) {
  /**
   * Discretize the distribution of this element. This possibly mutates the state of this discretizer so that future
   * calls return more refined approximations.
   * @return A discretized distribution for this element, represented as a map from extended values to probabilities
   * such that the sum of the probabilities is 1.0.
   */
  def discretize(): Map[Extended[T], Double]
}
