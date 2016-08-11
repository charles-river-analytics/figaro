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

package com.cra.figaro.experimental.marginalmap

import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.language._
import com.cra.figaro.util._

import scala.collection.mutable

/**
 * An algorithm for marginal MAP. This algorithm works by searching for the assignment to the MAP elements that
 * maximizes the probability of evidence of observing that assignment. Uses one time probability of evidence sampling at
 * each iteration for the given number of samples. The maximization is done by simulated annealing.
 * @param universe Universe on which to run the algorithm.
 * @param samplesPerIteration Number of probability of evidence samples to take per simulated annealing iteration.
 * @param proposalScheme Scheme for proposing new values. This can propose any element in the universe, but updates to
 * non-MAP elements are only used for generating new values for MAP elements.
 * @param schedule Schedule that produces an increasing temperature for simulated annealing.
 * @param mapElements List of elements over which to perform marginal MAP. These elements must not have evidence on them
 * that is contingent on the values of non-MAP elements.
 */
abstract class ProbEvidenceMarginalMAP(universe: Universe,
                                       samplesPerIteration: Int,
                                       proposalScheme: ProposalScheme,
                                       schedule: Schedule,
                                       val mapElements: List[Element[_]])
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

  /**
   * Computes the log probability of evidence of observing the current values of the MAP elements, using a one time
   * probability of evidence sampler. Does not change the state of the universe,
   * @return The log probability of observing the current values of the MAP elements.
   */
  def computeLogProbEvidence(): Double = {
    // Record the state of the universe, since changing it would break MH
    val state = new UniverseState(universe)

    // While running probability of evidence sampling, don't deactivate temporary elements in the MH cache
    val preserve = universe.activeElements.toSet

    for(elem <- mapElements) {
      // We can't just call elem.observe(elem.value) because elem.observe regenerates part of the model, so it might
      // change the values of other MAP elements. Fortunately, we have already recorded the state, giving us a fixed
      // value for each element.
      elem.observe(state.elementStates(elem).value.asInstanceOf[elem.Value])
    }

    val algorithm = new ProbEvidenceSampler(universe) with OneTimeProbEvidenceSampler {
      val numSamples = samplesPerIteration

      // Override this method so we don't call universe.clearTemporaries() after each sample
      override protected def doSample(): Unit = {
        totalWeight += 1

        try {
          val weight = lw.computeWeight(universe.activeElements)
          successWeight = logSum(successWeight, weight)
        } catch {
          case Importance.Reject => ()
        }

        // Deactivate only the temporary elements created for probability of evidence sampling
        for(elem <- universe.activeElements) {
          // Since an element deactivates its direct context contents when deactivated, it's possible that an element
          // in the list will be deactivated before we reach it, so we have to check again that it is active
          if(!preserve.contains(elem) && elem.active) elem.deactivate()
        }
      }
    }
    algorithm.start()
    val result = algorithm.logProbEvidence
    algorithm.kill()

    state.restore()

    result
  }

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
    val newStateLogProbEvidence = computeLogProbEvidence()
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
                                     samplesPerIteration: Int,
                                     proposalScheme: ProposalScheme,
                                     schedule: Schedule,
                                     mapElements: List[Element[_]])
  extends ProbEvidenceMarginalMAP(universe, samplesPerIteration, proposalScheme, schedule, mapElements)
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
                                     samplesPerIteration: Int,
                                     proposalScheme: ProposalScheme,
                                     schedule: Schedule,
                                     mapElements: List[Element[_]])
  extends ProbEvidenceMarginalMAP(universe, samplesPerIteration, proposalScheme, schedule, mapElements)
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
   * Creates a one time marginal MAP algorithm using probability of evidence.
   * Uses the default proposal scheme and schedule.
   * @param iterations Iterations of simulated annealing to run.
   * @param samplesPerIteration Number of probability of evidence samples to take per iteration.
   * @param mapElements List of elements over which to perform marginal MAP.
   */
  def apply(iterations: Int, samplesPerIteration: Int, mapElements: Element[_]*)(implicit universe: Universe) =
    new OneTimeProbEvidenceMarginalMAP(iterations, universe, samplesPerIteration, ProposalScheme.default(universe),
      Schedule.default(), mapElements.toList)

  /**
   * Creates an anytime marginal MAP algorithm using probability of evidence.
   * Uses the default proposal scheme and schedule.
   * @param samplesPerIteration Number of probability of evidence samples to take per iteration.
   * @param mapElements List of elements over which to perform marginal MAP.
   */
  def apply(samplesPerIteration: Int, mapElements: Element[_]*)(implicit universe: Universe) =
    new AnytimeProbEvidenceMarginalMAP(universe, samplesPerIteration, ProposalScheme.default(universe),
      Schedule.default(), mapElements.toList)

  /**
   * Creates a one time marginal MAP algorithm using probability of evidence.
   * @param iterations Iterations of simulated annealing to run.
   * @param samplesPerIteration Number of probability of evidence samples to take per iteration.
   * @param proposalScheme Scheme for proposing new values for MAP elements.
   * @param schedule Schedule that produces an increasing temperature for simulated annealing.
   * @param mapElements List of elements over which to perform marginal MAP.
   */
  def apply(iterations: Int, samplesPerIteration: Int, proposalScheme: ProposalScheme, schedule: Schedule,
            mapElements: Element[_]*)(implicit universe: Universe) =
    new OneTimeProbEvidenceMarginalMAP(iterations, universe, samplesPerIteration, proposalScheme, schedule, mapElements.toList)

  /**
   * Creates an anytime marginal MAP algorithm using probability of evidence.
   * @param samplesPerIteration Number of probability of evidence samples to take per iteration.
   * @param proposalScheme Scheme for proposing new values for MAP elements.
   * @param schedule Schedule that produces an increasing temperature for simulated annealing.
   * @param mapElements List of elements over which to perform marginal MAP.
   */
  def apply(samplesPerIteration: Int, proposalScheme: ProposalScheme, schedule: Schedule,
            mapElements: Element[_]*)(implicit universe: Universe) =
  new AnytimeProbEvidenceMarginalMAP(universe, samplesPerIteration, proposalScheme, schedule, mapElements.toList)
}
