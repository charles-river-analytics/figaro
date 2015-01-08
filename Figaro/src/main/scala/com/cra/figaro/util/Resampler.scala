/*
 * Resampler.scala 
 * Resamplers
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Jan 1, 2009
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.util

import java.util.TreeMap

/**
 * A resampler allows resampling from a set of weighted samples provided as the inputs.
 * 
 * @param inputs A sequence of pairs of probability and values.
 */
abstract class Resampler[T](inputs: Seq[(Double, T)]) {
  /**
   * Choose a sample with probability proportional to its weight.
   */
  def resample(): T
}

/**
 * A resampler that allows efficient resampling from a set of weighted samples provided as the inputs.
 * Creating the resampler takes O(N log N)
 * time, where N is the number of input samples, and selecting M samples takes O(M log N).
 * 
 * @param inputs A sequence of pairs of probability and values.
 */
class MapResampler[T](inputs: Seq[(Double, T)]) extends Resampler(inputs) {
  // In this design, we create a map that maps a uniform random number to the sample that should be chosen.
  // The map is designed so that the difference between the key for a sample and the next key is equal to the
  // probability of the sample.
  // When we sample a uniform random number, we search for the greatest key that is less than or equal to it. The
  // probability of choosing a key will then be equal to the difference between the key and the next one up, which
  // by construction is the desired probability of the sample.
  // A TreeMap is used to provide log n insertion and search.
  // Java's TreeMap is used because it provides floorEntry, which lets us find the right key.
  private val (weights, samples) = inputs.toList.unzip
  private val theMap: TreeMap[Double, T] = new TreeMap()
  private var total = 0.0
  for { (probability, sample) <- normalize(weights) zip samples } {
    theMap.put(total, sample)
    total += probability
  }

  /**
   * Choose a sample with probability proportional to its weight.
   */
  def resample(): T = theMap.floorEntry(random.nextDouble()).getValue

  // For testing
  private def getTheMap(): TreeMap[Double, T] = theMap
}
