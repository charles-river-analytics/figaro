/*
 * ProbEvidenceMarginalMAP.scala
 * A marginal MAP algorithm based on probability of evidence.
 *
 * Created By:      William Kretschmer (kretsch@mit.edu)
 * Creation Date:   Aug 1, 2016
 *
 * Copyright 2016 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.algorithm.sampling

import com.cra.figaro.algorithm.{AnytimeMarginalMAP, MarginalMAPAlgorithm, OneTimeMarginalMAP}
import com.cra.figaro.language._

import scala.collection.mutable

/**
 * An algorithm for marginal MAP. This algorithm works by searching for the assignment to the MAP elements that
 * maximizes the probability of evidence of observing that assignment. The maximization is done by simulated annealing.
 * @param universe Universe on which to run the algorithm.
 * @param proposalScheme Scheme for proposing new values. This can propose any element in the universe, but updates to
 * non-MAP elements are only used for generating new values for MAP elements.
 * @param schedule Schedule that produces an increasing temperature for simulated annealing.
 * @param logProbEvidenceComputer A function that given a list of elements, computes the log probability of evidence of
 * observing the values of those elements, ignoring any other evidence on those elements.
 *
 * Generally, this works by observing the values, running a probability of evidence sampler, then unobserving the values.
 * However, this is intentionally kept flexible; if there is a more efficient or precise way to compute the probability
 * of evidence for a specific model, the user can provide such a function.
 *
 * When the function returns, the values of the given elements and any evidence on them should be unchanged.
 * @param mapElements List of elements over which to perform marginal MAP.
 */
abstract class ProbEvidenceMarginalMAP(universe: Universe, proposalScheme: ProposalScheme, schedule: Schedule,
                                       logProbEvidenceComputer: List[Element[_]] => Double, val mapElements: List[Element[_]])
  extends MetropolisHastings(universe, proposalScheme, 0, 1, mapElements:_*) with MarginalMAPAlgorithm {
  import MetropolisHastings._

  // Current and best log probability of evidence over the MAP elements.
  protected var logProbEvidence = Double.MinValue
  protected var bestLogProbEvidence = Double.MinValue

  // Increasing temperature used for simulated annealing.
  protected var temperature = 1.0

  /**
   * Get the current temperature. Used for debugging.
   */
  def getTemperature = temperature

  override protected def initConstrainedValues() = {
    // We only initialize constraints for MAP elements
    // After this point, the keys in the map are assumed to be fixed because the constrained MAP elements won't change
    for(elem <- universe.constrainedElements.intersect(mapElements)) {
      currentConstraintValues(elem) = elem.constraintValue
    }
  }

  override protected def computeScores(): Double = {
    // Compute the log ratio of constraint values for only the MAP elements
    val scores = currentConstraintValues.keys.map(elem => elem.constraintValue - currentConstraintValues(elem))
    scores.sum
  }

  /*
   * Proposes a new state, computes acceptance probability, and decides whether or not to accept.
   * Updates temperature and logProbEvidence accordingly.
   */
  override protected def mhStep(): State = {
    val newStateUnconstrained = proposeAndUpdate()

    // Compute the new log probability of evidence, increment the temperature, and use these to compute the model
    // probability ratio
    val newStateLogProbEvidence = logProbEvidenceComputer(mapElements)
    temperature = schedule.temperature(temperature, sampleCount)
    val modelProb = (newStateLogProbEvidence - logProbEvidence) * temperature
    // Include constraints in the model probability ratio, and ignore any dissatisfied non-MAP elements
    val newState = State(newStateUnconstrained.oldValues, newStateUnconstrained.oldRandomness,
      newStateUnconstrained.proposalProb, modelProb + computeScores(),
      newStateUnconstrained.dissatisfied.intersect(fastTargets), newStateUnconstrained.visitOrder)

    if(decideToAccept(newState)) {
      logProbEvidence = newStateLogProbEvidence
      accepts += 1
      accept(newState)
    } else {
      rejects += 1
      undo(newState)
    }
    newState
  }

  override def sample(): (Boolean, Sample) = {
    mhStep()
    if(dissatisfied.isEmpty) {
      // Only update if we found a better state
      if(logProbEvidence > bestLogProbEvidence) {
        bestLogProbEvidence = logProbEvidence
        val values = mapElements.map(elem => elem -> elem.value)
        (true, mutable.Map(values:_*))
      }
      else {
        (true, mutable.Map())
      }
    }
    else {
      (false, mutable.Map())
    }

  }

  override protected def updateTimesSeenForTarget[T](elem: Element[T], newValue: T): Unit = {
    // Override the last update, which will later be returned as the most likely value
    allLastUpdates(elem) = (newValue, sampleCount)
  }

  override def computeMostLikelyValue[T](target: Element[T]): T = {
    // The last recorded value is the one that maximizes the probability of evidence
    allLastUpdates(target)._1.asInstanceOf[T]
  }

  override protected def doInitialize(): Unit = {
    super.doInitialize()
    // Only record dissatisfied MAP elements
    dissatisfied = dissatisfied.intersect(fastTargets)
  }

  /*
   * Prevent memory leaks by clearing temporary elements, particularly those in the cache.
   */
  override def cleanUp(): Unit = {
    super.cleanUp()
    universe.clearTemporaries()
    chainCache.clear()
  }

  /*
   * We don't want to force updates at the end of sampling, since the current values of MAP elements are not necessarily
   * the values that maximize the probability of evidence. We only want to make changes to allLastUpdates when a new set
   * of values increases the probability of evidence.
   */
  override def update(): Unit = {}
}

class AnytimeProbEvidenceMarginalMAP(universe: Universe,
                                     proposalScheme: ProposalScheme,
                                     schedule: Schedule,
                                     logProbEvidenceComputer: List[Element[_]] => Double,
                                     mapElements: List[Element[_]])
  extends ProbEvidenceMarginalMAP(universe, proposalScheme, schedule, logProbEvidenceComputer, mapElements)
    with AnytimeSampler with AnytimeMarginalMAP {
  /**
   * Initialize the algorithm.
   */
  override def initialize(): Unit = {
    super.initialize()
    doInitialize()
  }
}

class OneTimeProbEvidenceMarginalMAP(val numSamples: Int,
                                     universe: Universe,
                                     proposalScheme: ProposalScheme,
                                     schedule: Schedule,
                                     logProbEvidenceComputer: List[Element[_]] => Double,
                                     mapElements: List[Element[_]])
  extends ProbEvidenceMarginalMAP(universe, proposalScheme, schedule, logProbEvidenceComputer, mapElements)
    with OneTimeSampler with OneTimeMarginalMAP {

  /**
   * Run the algorithm, performing its computation to completion.
   */
  override def run(): Unit = {
    doInitialize()
    super.run()
  }
}

object ProbEvidenceMarginalMAP {
  /**
   * Run a one time log probability of evidence sampler.
   * @param universe Universe over which to run probability of evidence sampling.
   * @param samples Number of samples to take.
   * @param targets Elements whose values should be observed for the computation.
   * @return The result of the sampler after observing the current values in `targets`. When viewed as a partially
   * applied function of `targets`, this is a valid log probability of evidence computer for `ProbEvidenceMarginalMAP`.
   */
  def logProbEvidence(universe: Universe, samples: Int)(targets: List[Element[_]]): Double = {
    val conditions: Map[Element[_], List[(_ => Boolean, Element.Contingency)]] =
      targets.map(elem => (elem, elem.allConditions)).toMap
    val constraints: Map[Element[_], List[(_ => Double, Element.Contingency)]] =
      targets.map(elem => (elem, elem.allConstraints)).toMap

    for(elem <- targets){
      elem.removeConditions()
      elem.removeConstraints()
      elem.observe(elem.value)
    }

    val peAlg = new ProbEvidenceSampler(universe) with OneTimeProbEvidenceSampler { val numSamples = samples }
    peAlg.start()
    val result = peAlg.logProbEvidence
    peAlg.kill()

    for(elem <- targets){
      // We would call elem.unobserve, but we don't want to regenerate the value of the element
      elem.removeConditions()
      for((condition, contingency) <- conditions(elem)){
        elem.addCondition(condition.asInstanceOf[elem.Condition], contingency)
      }
      for((constraint, contingency) <- constraints(elem)){
        elem.addConstraint(constraint.asInstanceOf[elem.Constraint], contingency)
      }
    }

    result
  }

  /**
   * Creates a one time marginal MAP algorithm that maximizes according to probability of evidence computations.
   * Runs one time probability of evidence sampling at each iteration.
   * @param iterations Iterations of simulated annealing to run.
   * @param samplesPerIteration Number of probability of evidence samples to take per iteration.
   * @param proposalScheme Scheme for proposing new values for MAP elements.
   * @param schedule Schedule that produces an increasing temperature for simulated annealing.
   * @param mapElements List of elements over which to perform marginal MAP.
   */
  def apply(iterations: Int, samplesPerIteration: Int, proposalScheme: ProposalScheme, schedule: Schedule,
            mapElements: Element[_]*)(implicit universe: Universe) =
    new OneTimeProbEvidenceMarginalMAP(iterations, universe, proposalScheme, schedule,
      logProbEvidence(universe, samplesPerIteration), mapElements.toList)

  /**
   * Creates an anytime marginal MAP algorithm that maximizes according to probability of evidence computations.
   * Runs one time probability of evidence sampling at each iteration.
   * @param samplesPerIteration Number of probability of evidence samples to take per iteration.
   * @param proposalScheme Scheme for proposing new values for MAP elements.
   * @param schedule Schedule that produces an increasing temperature for simulated annealing.
   * @param mapElements List of elements over which to perform marginal MAP.
   */
  def apply(samplesPerIteration: Int, proposalScheme: ProposalScheme, schedule: Schedule,
            mapElements: Element[_]*)(implicit universe: Universe) =
  new AnytimeProbEvidenceMarginalMAP(universe, proposalScheme, schedule,
    logProbEvidence(universe, samplesPerIteration), mapElements.toList)

  /**
   * Creates a one time marginal MAP algorithm that maximizes according to probability of evidence computations.
   * @param iterations Iterations of simulated annealing to run.
   * @param proposalScheme Scheme for proposing new values for MAP elements.
   * @param schedule Schedule that produces an increasing temperature for simulated annealing.
   * @param logProbEvidence Function for computing log probability of evidence. See `ProbEvidenceMarginalMAP` abstract
   * class for more details.
   * @param mapElements List of elements over which to perform marginal MAP.
   */
  def apply(iterations: Int, proposalScheme: ProposalScheme, schedule: Schedule, logProbEvidence: List[Element[_]] => Double,
            mapElements: Element[_]*)(implicit universe: Universe) =
  new OneTimeProbEvidenceMarginalMAP(iterations, universe, proposalScheme, schedule, logProbEvidence, mapElements.toList)

  /**
   * Creates an anytime marginal MAP algorithm that maximizes according to probability of evidence computations.
   * Runs one time probability of evidence sampling at each iteration.
   * @param proposalScheme Scheme for proposing new values for MAP elements/
   * @param schedule Schedule that produces an increasing temperature for simulated annealing.
   * @param logProbEvidence Function for computing log probability of evidence. See `ProbEvidenceMarginalMAP` abstract
   * class for more details.
   * @param mapElements List of elements over which to perform marginal MAP.
   */
  def apply(proposalScheme: ProposalScheme, schedule: Schedule, logProbEvidence: List[Element[_]] => Double,
            mapElements: Element[_]*)(implicit universe: Universe) =
  new AnytimeProbEvidenceMarginalMAP(universe, proposalScheme, schedule, logProbEvidence, mapElements.toList)
}
