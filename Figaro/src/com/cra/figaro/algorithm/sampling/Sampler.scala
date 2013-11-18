/*
 * Sampler.scala
 * Anytime and one-time samplers.
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Jan 1, 2009
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.algorithm.sampling

import com.cra.figaro.algorithm._
import com.cra.figaro.language._

/**
 * A trait for sampling algorithms.
 */
trait Sampler extends Algorithm {
  protected def doSample(): Unit

  /* This is called each time a new set of samples is collected. */
  protected def resetCounts(): Unit

  /* Called at the end of the run to force a lazy update on any sampler values */
  protected def update(): Unit
}

/**
 * Anytime sampling algorithms.
 */
trait AnytimeSampler extends Algorithm with Anytime with Sampler {
  /**
   * Number of samples that should be taken in a single step of the algorithm.
   */
  val blockSize = 1 // override this field to use a larger block size

  /**
   * Called when the algorithm starts before running any steps.
   */
  override def initialize() = {
    super.initialize()
    resetCounts()
  }

  /**
   * Run a single step of the algorithm. The algorithm must be able to provide answers after each step.
   */
  def runStep() =
    for { i <- 1 to blockSize } { doSample() }

  /**
   * Override the stopUpdate function in anytime to call the sampler update function
   */
  override def stopUpdate() = update
}

/**
 * Anytime sampling algorithms that compute conditional probability of query elements.
 */
trait AnytimeProbQuerySampler extends AnytimeProbQuery with AnytimeSampler

/**
 * Anytime sampling algorithms that compute probability of evidence.
 */
trait AnytimeProbEvidenceSampler extends AnytimeSampler with AnytimeProbEvidence {
  def additionalEvidenceAlgorithm(evidence: List[NamedEvidence[_]]) =
    new ProbEvidenceSampler(universe, evidence, computedResult) with AnytimeProbEvidenceSampler
}

/**
 * Anytime sampling algorithms that compute MPE.
 */
trait AnytimeMPESampler extends AnytimeSampler with AnytimeMPE

/**
 * One-time sampling algorithms.
 */
trait OneTimeSampler extends Sampler with OneTime {
  /**
   * The number of samples to collect from the model.
   */
  val numSamples: Int

  /**
   * Run the algorithm, performing its computation to completion.
   */
  def run() = {
    resetCounts()
    for { i <- 1 to numSamples } { doSample() }
    update()
  }
}

/**
 * One-time sampling algorithms that compute conditional probability of query elements.
 */
trait OneTimeProbQuerySampler extends ProbQueryAlgorithm with OneTimeSampler with OneTimeProbQuery

/**
 * One-time sampling algorithms that compute probability of evidence.
 */
trait OneTimeProbEvidenceSampler extends OneTimeSampler with OneTimeProbEvidence {
  def additionalEvidenceAlgorithm(evidence: List[NamedEvidence[_]]) = {
    val ns = numSamples
    new ProbEvidenceSampler(universe, evidence, computedResult) with OneTimeProbEvidenceSampler { val numSamples = ns }
  }
}

/**
 * One-time sampling algorithms that compute probability of evidence.
 */
trait OneTimeMPESampler extends OneTimeSampler with OneTimeMPE
