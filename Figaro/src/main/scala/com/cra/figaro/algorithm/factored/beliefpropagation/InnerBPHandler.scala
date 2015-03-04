/*
 * InnerBPHandler.scala
 * Trait for creating and running Belief Propagation within another algorithm
 * 
 * Created By:      Brian Ruttenberg (bruttenberg@cra.com)
 * Creation Date:   Oct 20, 2014
 * 
 * Copyright 2014 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.algorithm.factored.beliefpropagation

import com.cra.figaro.language._
import com.cra.figaro.algorithm.OneTimeProbQuery
import com.cra.figaro.algorithm.AnytimeProbQuery
import com.cra.figaro.algorithm.sampling.ProbEvidenceSampler
import com.cra.figaro.algorithm.Anytime

/**
 * Trait for creating and running Belief Propagation within another algorithm.
 */
trait InnerBPHandler {

  /**
   * Universe associated with this algorithm.
   */
  protected var currentUniverse: Universe = _

  /**
   * BP algorithm associated with this time step.
   */
  protected[figaro] var bp: ProbQueryBeliefPropagation = _

  /**
   * Instantiates the appropriate BP algorithm for the current time step.
   */
  protected def createBP(targets: List[Element[_]], depth: Int = Int.MaxValue, upperBounds: Boolean = false): Unit

  /**
   * Runs the BP algorithm at the current time step.
   */
  protected def runBP(): Unit

}

/**
 * Trait for running Factored Frontier with one time Belief Propagation.
 */
trait OneTimeInnerBPHandler extends InnerBPHandler {
  /**
   * Number of iterations to run BP per step.
   */
  val innerIterations: Int

  protected def createBP(targets: List[Element[_]], depth: Int = Int.MaxValue, upperBounds: Boolean = false): Unit = {
    bp = new ProbQueryBeliefPropagation(currentUniverse, targets: _*)(List(),
      (u: Universe, e: List[NamedEvidence[_]]) => () => ProbEvidenceSampler.computeProbEvidence(10000, e)(u), depth, upperBounds) with OneTimeProbabilisticBeliefPropagation with OneTimeProbQuery {
      override val iterations = innerIterations      
    }
  }

  protected def runBP() {
    bp.start()
  }
}

/**
 * Trait for running Factored Frontier with anytime Belief Propagation.
 */
trait AnytimeInnerBPHandler extends InnerBPHandler {
  /**
   * Time, in milliseconds, to run BP per step.
   */
  val myStepTimeMillis: Long

  protected def createBP(targets: List[Element[_]], depth: Int = Int.MaxValue, upperBounds: Boolean = false): Unit = {
    if (bp != null) bp.kill
    bp = new ProbQueryBeliefPropagation(currentUniverse, targets: _*)(List(),
      (u: Universe, e: List[NamedEvidence[_]]) => () => ProbEvidenceSampler.computeProbEvidence(10000, e)(u), depth, upperBounds) with AnytimeProbabilisticBeliefPropagation with AnytimeProbQuery
  }

  protected def runBP() {
    bp.start()
    Thread.sleep(myStepTimeMillis)
    bp.stop()    
  }
}
