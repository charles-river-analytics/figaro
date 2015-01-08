/*
 * ProbEvidenceSampler.scala
 * Samplers that compute probability of evidence.
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
import scala.language.existentials
import com.cra.figaro.util.logSum

/**
* Algorithm that computes probability of evidence using forward sampling.
* The evidence is specified as NamedEvidence.
* Only the probability of this evidence is computed. Conditions and constraints that are already on elements are considered part of the definition of the model.
*/
abstract class ProbEvidenceSampler(override val universe: Universe, override val evidence: List[NamedEvidence[_]] = List[NamedEvidence[_]](), partition: Double = 1.0)
  extends ProbEvidenceAlgorithm with Sampler {
  private var successWeight: Double = _
  private var totalWeight: Double = _
  
  //Logarithmic versions.
  
  protected def resetCounts() = {
    successWeight = Double.NegativeInfinity
    totalWeight = 0.0
	
  }

  /*
  * To protect against underflow, the probabilities are computed in log-space.
  */
  protected def doSample(): Unit = {
    Forward(universe)

	//Some values in log constraints may be negative infinity.
    val weight = universe.constrainedElements.map(_.constraintValue).sum
	
    val satisfied = universe.conditionedElements forall (_.conditionSatisfied)
	
    totalWeight += 1
    if (satisfied) successWeight = logSum(successWeight, weight)
    universe.clearTemporaries() // avoid memory leaks
  }

  protected def update(): Unit = {}
  
  protected def logComputedResult = {
	successWeight - Math.log(partition) - Math.log(totalWeight)
  }
  
   protected def computedResult = {
	Math.exp(logComputedResult)
  }
  
   override def logProbEvidence: Double = {
    if (!active) throw new AlgorithmInactiveException
    logComputedResult
  }
  
}

object ProbEvidenceSampler {
  /**
   * Create a one-time sampler that computes probability of the named evidence using the given number of samples.
   * Takes the conditions and constraints in the model as part of the model definition.
   */
  def apply(numSamplesToUse: Int, evidence: List[NamedEvidence[_]])(implicit universe: Universe): ProbEvidenceAlgorithm = {
    val baseline = new ProbEvidenceSampler(universe) with OneTimeProbEvidenceSampler { val numSamples = numSamplesToUse }
    baseline.start()
    baseline.probAdditionalEvidence(evidence)
  }

  /**
   * Create an anytime sampler that computes probability of the named evidence.
   * Takes the conditions and constraints in the model as part of the model definition.
   * It also uses an anytime sampler for computing the baseline probability of conditions and constraints in the 
   * program. 
   * 
   * @param baselineWaitingTime The amount of time to allow the algorithm for computing the baseline probability to run.
   */
  def apply(baselineWaitingTime: Long, evidence: List[NamedEvidence[_]])(implicit universe: Universe): ProbEvidenceAlgorithm = {
    val baseline = new ProbEvidenceSampler(universe) with AnytimeProbEvidenceSampler
    baseline.start()
    Thread.sleep(baselineWaitingTime)
    baseline.stop()
    val alg = baseline.probAdditionalEvidence(evidence)
    baseline.kill
    alg
  }
  
  /**
   * Use one-time sampling to compute the probability of the given named evidence.
   * Takes the conditions and constraints in the model as part of the model definition.
   * This method takes care of creating and running the necessary algorithms.
   */
  def computeProbEvidence(numSamplesToUse: Int, evidence: List[NamedEvidence[_]])(implicit universe: Universe): Double = {
    val alg1 = new ProbEvidenceSampler(universe) with OneTimeProbEvidenceSampler { val numSamples = numSamplesToUse }
    alg1.start()
    val alg2 = alg1.probAdditionalEvidence(evidence)
    alg1.kill()
    alg2.start()
    val result = alg2.probEvidence
    alg2.kill()
    result
  }

  /**
   * Use anytime sampling to compute the probability of the given named evidence, taking the conditions and constraints in the model as part of the model definition.
   * Takes the conditions and constraints in the model as part of the model definition.
   * This method takes care of creating and running the necessary algorithms.
   * 
   * @param waitingTime Total time given to all steps of the method.
   */
  def computeProbEvidence(waitingTime: Long, evidence: List[NamedEvidence[_]])(implicit universe: Universe): Double = {
    val alg1 = new ProbEvidenceSampler(universe) with AnytimeProbEvidenceSampler
    alg1.start()
    Thread.sleep(waitingTime / 2)
    alg1.stop()
    val alg2 = alg1.probAdditionalEvidence(evidence)
    alg1.kill()
    alg2.start()
    Thread.sleep(waitingTime / 2)
    val result = alg2.probEvidence
    alg2.kill()
    result
  }

  /**
   * Default algorithm to pass to dependent universe algorithms.
   */
  val default = (u: Universe, e: List[NamedEvidence[_]]) => () => computeProbEvidence(10000, e)(u)
}
