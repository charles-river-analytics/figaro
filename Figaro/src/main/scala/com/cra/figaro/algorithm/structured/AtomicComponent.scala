/*
 * AtomicComponent.scala
 * Problem components for atomic elements.
 *
 * Created By:      William Kretschmer (kretsch@mit.edu)
 * Creation Date:   Apr 14, 2017
 *
 * Copyright 2017 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.algorithm.structured

import com.cra.figaro.algorithm.factored.ParticleGenerator
import com.cra.figaro.algorithm.factored.factors.Variable
import com.cra.figaro.algorithm.lazyfactored.{Extended, Regular, ValueSet}
import com.cra.figaro.language._

import scala.collection.mutable

/**
 * Problem components for atomic elements.
 */
abstract class AtomicComponent[Value](problem: Problem, val atomic: Atomic[Value]) extends ProblemComponent(problem, atomic) {
  /**
   * Probabilities associated with values in the range of this component. This is used for factor creation. It is
   * required that (1) the key set of this map equals the range of the component, and (2) the values in this map sum to
   * 1.0.
   */
  var probs: Map[Extended[Value], Double] = Map(range.xvalues.head -> 1.0) // Initialize to {* -> 1.0}

  /**
   * Discretize the distribution of this atomic element. This possibly mutates the state of this component so that
   * future calls return more refined approximations.
   * @return A discretized distribution for this element, represented as a map from extended values to probabilities
   * such that the sum of the probabilities is 1.0.
   */
  def discretize(): Map[Extended[Value], Double]

  /**
   * Whether or not this component can compute its complete (i.e. fully refined) distribution.
   * @return True if and only if `discretize()` will always return a complete range and distribution.
   */
  def fullyRefinable(): Boolean

  override def generateRange(): Unit = {
    // Generate extended values and probabilities with the discretizer
    probs = discretize()
    // Assign the range in the normal way using the extended values
    val newRange = new ValueSet(probs.keySet)
    if ((newRange.hasStar ^ range.hasStar) || (newRange.regularValues != range.regularValues)) {
      range = newRange
      setVariable(new Variable(range))
    }
  }
}

/**
 * Atomic components that use the default (finite) ranging method. It uses the finite range of the element if it exists,
 * returning {*} otherwise.
 */
class FiniteAtomicComponent[Value](problem: Problem, atomic: Atomic[Value]) extends AtomicComponent(problem, atomic) {
  override val discretize: Map[Extended[Value], Double] = {
    val range = Range(this)
    if(range.hasStar) {
      // If range contains *, it must be equal to {*}; return this
      assert(range.xvalues.size == 1)
      Map(range.xvalues.head -> 1.0)
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
 * Atomic components that approximate a distribution by sampling.
 */
class SampledAtomicComponent[Value](problem: Problem, atomic: Atomic[Value]) extends AtomicComponent(problem, atomic) {
  /**
   * Number of samples to take each time `discretize()` is called.
   */
  var samplesPerIteration: Int = 1

  /**
   * All samples taken so far, stored as a map from a value to the number of times it has been sampled.
   */
  protected val samples: mutable.Map[Value, Int] = mutable.Map().withDefaultValue(0)

  /**
   * Number of samples taken so far.
   */
  protected var samplesTaken: Int = 0

  override def discretize(): Map[Extended[Value], Double] = {
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
 * Atomic components that use non-SFI ranging for backward compatability. Unlike other atomic components, the discretize
 * method is to be called after the range has been generated, for the purpose of putting a distribution on the values.
 */
class ValuesAtomicComponent[Value](problem: Problem, atomic: Atomic[Value]) extends AtomicComponent(problem, atomic) {
  override def discretize(): Map[Extended[Value], Double] = {
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

// TODO binning component for continuous elements, counting component for positive integer-valued components
