/*
 * AtomicRanger.scala
 * Ranging for atomic elements.
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

import com.cra.figaro.algorithm.factored.ParticleGenerator
import com.cra.figaro.algorithm.lazyfactored.{Extended, Regular, Star}
import com.cra.figaro.algorithm.structured.{ComponentCollection, Range}
import com.cra.figaro.language._

import scala.collection.mutable

/**
 * Ranging for atomic elements. This involves producing a set of (extended) values, mapping each value to its weight.
 * This encodes a (discretized) distribution for the element.
 * @param atomic Atomic to range.
 */
abstract class AtomicRanger[T](val atomic: Atomic[T]) {
  /**
   * Discretize the distribution of this atomic element. This possibly mutates the state of this ranger so that future
   * calls return more refined approximations.
   * @return A discretized distribution for this element, represented as a map from extended values to probabilities
   * such that the sum of the probabilities is 1.0.
   */
  def discretize(): Map[Extended[T], Double]

  /**
   * Whether or not this ranger can compute the complete (i.e. fully refined) distribution.
   * @return True if and only if `discretize()` will always return a complete range and distribution.
   */
  def fullyRefinable(): Boolean
}

/**
 * Range using the default (finite) ranging method. It uses the finite range of the element if it exists, returning {*}
 * otherwise.
 * @param atomic Atomic to range.
 */
class FiniteRanger[T](atomic: Atomic[T]) extends AtomicRanger(atomic) {
  override val discretize: Map[Extended[T], Double] = {
    val range = Range.atomicRange(atomic)
    if(range.hasStar) {
      // If range contains *, it must be equal to {*}; return this
      assert(range.xvalues.size == 1)
      Map(Star[T]() -> 1.0)
    }
    else {
      // Return each of the regular values mapped to their density
      val regularProbs = for(value <- range.xvalues) yield value -> atomic.density(value.value)
      regularProbs.toMap
    }
  }

  // If the distribution computed consists only of regular values, then it is the fully refined distribution.
  // Otherwise, the distribution is only over {*}, in which case it is not fully refinable.
  override val fullyRefinable: Boolean = discretize.head._1.isRegular
}

/**
 * Approximate a distribution by sampling. This takes additional samples at each iteration.
 * @param atomic Atomic to range.
 * @param samplesPerIteration Additional samples to take each time `discretize()` is called. Must be > 0.
 */
class SamplingRanger[T](atomic: Atomic[T], var samplesPerIteration: Int = 1) extends AtomicRanger(atomic) {
  /**
   * All samples taken so far, stored as a map from a value to the number of times it has been sampled.
   */
  protected val samples: mutable.Map[T, Int] = mutable.Map().withDefaultValue(0)

  /**
   * Number of samples taken so far.
   */
  protected var samplesTaken: Int = 0

  override def discretize(): Map[Extended[T], Double] = {
    for(_ <- 1 to samplesPerIteration) {
      atomic.generate()
      val sample = atomic.value
      // Even if sample isn't in the map, the map has default value 0
      samples(sample) += 1
    }
    samplesTaken += samplesPerIteration
    // Turn counts into weights by dividing by the total number of samples taken
    val normalizer = samplesTaken.toDouble
    val normalized = for((prob, numSamples) <- samples) yield Regular(prob) -> numSamples / normalizer
    normalized.toMap
  }

  // A sampled component is never fully refinable
  override val fullyRefinable: Boolean = false
}

/**
 * Components for atomics whose ranges consist of integers in the range [L,infinity) for some inclusive lower bound L.
 * Ranging proceeds by taking all integers in the range [L,U] for an increasing upper bound U.
 * @param atomic Atomic to range.
 * @param lower Lower bound L, described above.
 * @param valuesPerIteration Number of additional values to take each time `discretize()` is called.
 */
class CountingRanger(atomic: Atomic[Int], val lower: Int, var valuesPerIteration: Int = 1) extends AtomicRanger(atomic) {
  // TODO consider other ways to sample than just taking the first n values
  // For example, when the mode is much greater than 0, the current method will initially put most of the mass on *
  // If possible, a greedy approach might work better: select the remaining values with greatest density

  /**
   * Current number of regular values in the range of this component.
   */
  protected var numValues: Int = 0

  override def discretize(): Map[Extended[Int], Double] = {
    // Take additional samples each iteration
    numValues += valuesPerIteration
    // Accumulate the total probability mass on regular values
    var regularTotal = 0.0
    // Compute probability masses on particular values in the range [lower, lower + numvalues)
    val regularValues = for(value <- lower until (lower + numValues)) yield {
      val prob = atomic.density(value)
      regularTotal += prob
      Regular(value) -> prob
    }
    // All remaining unaccounted probability mass goes to *
    val starProb = 1.0 - regularTotal
    if(starProb > 0.0) regularValues.toMap[Extended[Int], Double].updated(Star(), starProb)
    else regularValues.toMap
  }

  // A component with possibly infinite range is not fully refinable
  override val fullyRefinable: Boolean = false
}

/**
 * Non-SFI ranging for backward compatability. The discretize method is to be called after the range has been generated,
 * for the purpose of putting a distribution on the values.
 */
private[figaro] class ValuesRanger[T](atomic: Atomic[T], collection: ComponentCollection) extends AtomicRanger(atomic) {
  override def discretize(): Map[Extended[T], Double] = {
    val atomicComp = collection(atomic)
    val variable = atomicComp.variable
    // Default ranging falls into one of 3 cases:
    // 1: the range was not completely generated and is {*}
    if(variable.valueSet.hasStar) {
      // Default ranging only produces * when the entire range is {*}
      // Verify that this is the case and return {* -> 1.0}
      assert(variable.size == 1)
      Map(variable.range.head -> 1.0)
    }
    // 2: the element was sampled because it is an infinite atomic
    else if(ParticleGenerator.exists(atomic.universe) && ParticleGenerator(atomic.universe).samplesTaken(atomic) > 0) {
      val samples = ParticleGenerator(atomic.universe)(atomic)
      // Map samples to regular values
      val extendedSamples = for((prob, value) <- samples) yield Regular(value) -> prob
      extendedSamples.toMap
    }
    // 3: the entire range was generated because the element is a finite atomic
    else {
      // Map each regular value to its density
      variable.range.map(xvalue => xvalue -> atomic.density(xvalue.value)).toMap
    }
  }

  // This component should not be used within LSFI, so this method always returns false
  override def fullyRefinable(): Boolean = false
}

// TODO binning component for continuous elements
