/*
 * DecisionSample.scala
 * Convenience class to handle the output of decision inference algorithms
 * 
 * Created By:      Brian Ruttenberg (bruttenberg@cra.com)
 * Creation Date:   Oct 5, 2012
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.library.decision

/**
 * Convenience class to handle the output of decision inference algorithms. 
 * Contains a weighted utility and the weight
 * 
 * @param weightedUtil The utility multiplied by its weight
 * 
 */
 case class DecisionSample(weightedUtil: Double, weight: Double) {
  /**
   * Adding two DecisionSamples produces the sum of their weighted utilities and their total weight.
   */
  def +(that: DecisionSample) = new DecisionSample(this.weightedUtil + that.weightedUtil, this.weight + that.weight)
  /**
   * Normalize a Decision sample's weighted utility by its weight
   */
  def norm = weightedUtil / weight
}
