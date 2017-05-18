/*
 * RangingStrategy.scala
 * Class to choose
 *
 * Created By:      William Kretschmer (kretsch@mit.edu)
 * Creation Date:   May 18, 2017
 *
 * Copyright 2017 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */
package com.cra.figaro.algorithm.structured.strategy.range

import com.cra.figaro.algorithm.structured.ComponentCollection
import com.cra.figaro.language._
import com.cra.figaro.library.atomic.discrete.{AtomicBinomial, AtomicGeometric, AtomicPoisson}

/**
 * Ranging strategies specify which method to use to range a particular atomic element.
 */
abstract class RangingStrategy {
  def apply[T](atomic: Atomic[T]): AtomicRanger[T]
}

object RangingStrategy {
  /**
   * Default ranging strategy. Uses finite ranging for finite atomics, counting for infinite integer-valued atomics, and
   * sampling for all other atomics.
   * @param numValues Number of values to count (for infinite integer-valued atomics) or sample (for sampled atomics) at
   * each iteration.
   */
  def default(numValues: Int) = new RangingStrategy {
    override def apply[T](atomic: Atomic[T]): AtomicRanger[T] = {
      atomic match {
        case flip: AtomicFlip => new FiniteRanger(flip)
        case select: AtomicSelect[T] => new FiniteRanger(select)
        case binomial: AtomicBinomial => new FiniteRanger(binomial)
        case geometric: AtomicGeometric => new CountingRanger(geometric, 1, numValues)
        case poisson: AtomicPoisson => new CountingRanger(poisson, 0, numValues)
        case atomic: Atomic[T] => new SamplingRanger(atomic, numValues)
      }
    }
  }
}
