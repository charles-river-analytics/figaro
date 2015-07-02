/*
 * ElementSampler.scala
 * A forward sampler for elements.
 * 
 * Created By:      Brian Ruttenberg (bruttenberg@cra.com)
 * Creation Date:   Sep 26, 2014
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.algorithm.sampling

import com.cra.figaro.algorithm._
import com.cra.figaro.language._
import com.cra.figaro.util._
import scala.collection.mutable.Map

/**
 * An abstract class to generates samples from the marginal distribution of an element.
 * @param target The element to generate samples from
 */
abstract class ElementSampler(target: Element[_]) extends BaseUnweightedSampler(target.universe, target) {

  def sample(): (Boolean, Sample) = {
    Forward(target)    
    (true, Map[Element[_], Any](target -> target.value))
  }

  protected def doInitialize(): Unit = {
    // Need to prime the universe to make sure all elements have a generated value    
    Forward(target)
  }

}

/**
 * Anytime Element sampler.
 */
class AnytimeElementSampler(target: Element[_])
  extends ElementSampler(target)
  with UnweightedSampler with AnytimeProbQuerySampler {
  /**
   * Initialize the sampler.
   */
  override def initialize(): Unit = {
    super.initialize()
    doInitialize()
  }

  /**
   * Clean up the sampler, freeing memory.
   */
  override def cleanUp(): Unit = {
    universe.clearTemporaries()
    super.cleanUp()
  }
}

/**
 * One-time Element sampler.
 *
 * @param myNumSamples The number samples to take from the element
 */
class OneTimeElementSampler(target: Element[_], myNumSamples: Int)
  extends ElementSampler(target)
  with UnweightedSampler with OneTimeProbQuerySampler {

  val numSamples = myNumSamples

  /**
   * Run the algorithm, performing its computation to completion.
   */
  override def run(): Unit = {
    doInitialize()
    super.run()
    update
  }
}


object ElementSampler {

  /**
   * Create an anytime Element sampler with the given target element
   */
  def apply(target: Element[_]) =  new AnytimeElementSampler(target)

  /**
   * Create an one time Element sampler with the given target element using the number of samples
   */
  def apply(target: Element[_], numSamples: Int) = new OneTimeElementSampler(target, numSamples)
}
