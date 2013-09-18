/*
 * OneTimeProbEvidence.scala
 * One-time algorithms that compute probability of evidence.
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Jan 1, 2009
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 */

package com.cra.figaro.algorithm

/**
 * One-time algorithms that compute probability of evidence.
 * A class that implements this trait must implement the run and computeprobEvidence methods.
 */
trait OneTimeProbEvidence extends ProbEvidenceAlgorithm with OneTime {
  /**
   * Returns the probability of evidence of the universe on which the algorithm operates.
   * Throws AlgorithmInactiveException if the algorithm is not active.
   */
  def probabilityOfEvidence(): Double = {
    if (!active) throw new AlgorithmInactiveException
    computedResult
  }
}