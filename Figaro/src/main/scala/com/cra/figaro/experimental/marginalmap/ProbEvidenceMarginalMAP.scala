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
import com.cra.figaro.language.Element.ElemVal
import com.cra.figaro.language._
import com.cra.figaro.util._

import scala.annotation.tailrec
import scala.collection.mutable

/**
 * An algorithm for marginal MAP. This algorithm works by searching for the assignment to the MAP elements that
 * maximizes the probability of evidence of observing that assignment. Uses one time probability of evidence sampling at
 * each iteration for the given number of samples. Since the probability of evidence is just an estimate, this algorithm
 * is allowed to repeatedly take more probability of evidence samples until it believes with high confidence that one
 * state is better than another state, or it has reached the maximum number of allowed runs. The maximization is done by
 * simulated annealing.
 * @param universe Universe on which to run the algorithm.
 * @param tolerance Confidence level used deciding to accept or reject under uncertainty. This corresponds to a maximum
 * allowed p-value. Thus, setting this to 0.05 means we only accept or reject if we are at least 95% confident that
 * we're making the right decision. Must between 0 and 0.5.
 * @param samplesPerIteration Number of probability of evidence samples to take per run. Must be strictly greater than 1.
 * A reasonable starting point is 100.
 * @param maxRuns Maximum number of runs of probability of evidence sampling allowed before giving up and returning the
 * proposal with higher estimated probability of evidence. Thus, at each iteration of simulated annealing, this
 * algorithm can take as many as `samplesPerIteration * maxRuns` probability of evidence samples. Setting to 1
 * corresponds to using no hypothesis test at all. Setting to `Int.MaxValue` corresponds to running indefinitely until
 * we are confident that the proposal should be accepted or rejected.
 * @param proposalScheme Scheme for proposing new values. This can propose any element in the universe, but updates to
 * non-MAP elements are only used for generating new values for MAP elements.
 * @param schedule Schedule that produces an increasing temperature for simulated annealing.
 * @param mapElements List of elements over which to perform marginal MAP. These elements must not have evidence on them
 * that is contingent on the values of non-MAP elements. Additionally, these elements must be "observable", in the sense
 * that observing values for these elements and computing the probability of evidence of those observations should not
 * uniquely return zero. Typically, this is satisfiable by elements that are not both continuous and deterministic. The
 * algorithm will still run if this condition is not satisfied, but it will not converge.
 */
abstract class ProbEvidenceMarginalMAP(universe: Universe,
                                       tolerance: Double,
                                       samplesPerIteration: Int,
                                       maxRuns: Int,
                                       proposalScheme: ProposalScheme,
                                       schedule: Schedule,
                                       val mapElements: List[Element[_]])
  // Burn-in and interval aren't needed in this context, so they are set to 0 and 1, respectively
  extends MetropolisHastings(universe, proposalScheme, 0, 1, mapElements:_*) with MarginalMAPAlgorithm {
  import MetropolisHastings._

  require(samplesPerIteration >= 2, "samples per iteration must be at least 2")
  require(0 < tolerance && tolerance < 0.5, "tolerance must be between 0 and 0.5")
  require(maxRuns >= 1, "maximum allowed runs must be at least 1")

  // The probability of evidence sampler associated with the current state of the MAP variables. Initialized when this
  // (i.e. the ProbEvidenceMarginalMAP) is started. In general, while this is active, probEvidenceSampler refers to
  // an active MMAPProbEvidenceSampler that can be run for additional iterations to improve its estimate.
  protected var probEvidenceSampler: MMAPProbEvidenceSampler = _

  // Elements created by MH (and stored in chainCache) that should not be deleted while sampling probability of evidence.
  // This is needed because ProbEvidenceSampler can create temporary elements while running, and they must be cleared to
  // avoid memory leaks. However, we don't just call universe.clearTemporaries() because this would also clear
  // chainCache, which we don't want. This is a var (as opposed to an argument to MMAPProbEvidenceSampler) because it
  // may change between iterations of MH.
  protected var preserve: Set[Element[_]] = _

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

  override protected def mhStep(): State = {
    // This state is not constrained. Constraints are handled in decideToAccept because we incorporate them in the
    // probability of evidence computation.
    val newState = proposeAndUpdate()
    // We don't care about dissatisfied elements that aren't MAP elements; remove them
    newState.dissatisfied.retain(fastTargets.contains)

    if(decideToAccept(newState)) {
      accepts += 1
      accept(newState)
    } else {
      rejects += 1
      undo(newState)
    }
    newState
  }

  /**
  Decide whether or not to accept the new (unconstrained) state, first taking into account conditions on the MAP
   * elements. Does not change the state of the universe. Updates the temperature, preserved elements, and probability
   * of evidence sampler accordingly. Incorporates constraints on the MAP elements.
   */
  override protected def decideToAccept(newState: State): Boolean = {
    // Use the same satisfied / dissatisfied rule as MH
    val nothingNewDissatisfied = newState.dissatisfied subsetOf dissatisfied
    val somethingOldSatisfied = dissatisfied exists (_.conditionSatisfied)
    if (nothingNewDissatisfied && somethingOldSatisfied) true
    else if (!nothingNewDissatisfied && !somethingOldSatisfied) false
    else {
      decideToAcceptSatisfied()
    }
  }

  /**
   * Like decideToAccept, but assume all conditions on the MAP elements are satisfied.
   */
  protected def decideToAcceptSatisfied(): Boolean = {
    // Update the temperature
    temperature = schedule.temperature(temperature, sampleCount)

    // Always accept if none of the MAP values changed. This isn't necessarily meaningless; the values of other elements
    // may have changed, which could result in new proposals later. These are "observations" because they are the values
    // we observe in the probability of evidence sampler.
    val observations = currentMAPValues
    if(observations == probEvidenceSampler.observations) return true

    // Record the universe state because probability of evidence sampling may corrupt it
    val universeState = new UniverseState(universe)
    // Record the elements that probability of evidence sampling shouldn't deactivate (includes those in the cache)
    preserve = universeState.myActiveElements

    // Create and start a probability of evidence sampler over the current values of the MAP elements
    val newProbEvidenceSampler = new MMAPProbEvidenceSampler(observations)
    // This initially runs the sampler for samplesPerIteration samples
    newProbEvidenceSampler.start()

    // Normally, we test if log(U[0,1]) < (log(newProbEvidence) - log(oldProbEvidence) + computeScores) * temperature.
    // We want to reformat this as a hypothesis test involving the log of the mean of two sampled random variables.
    // This yields log(oldProbEvidence) + (log(U[0,1]) / temperature - computeScores) < log(newProbEvidence).
    // Thus, logConstant is the multiplicative constant applied to the old probability of evidence.

    // Note that in this implementation, we choose to ignore the proposal probability. In theory, this should not make a
    // difference in the limiting case, but in practice choosing to incorporate or not incorporate this probability
    // could affect the rate of convergence. Here, we choose to ignore it because the proposal probability may not
    // correspond to a meaningful change over the MAP elements. In particular, if new values are proposed for non-MAP
    // elements but none of the values of MAP elements change, it would not be sensible to weight one state as being
    // more or less favorable than the other.
    val logConstant =  math.log(random.nextDouble) / temperature - computeScores()
    // We've already run newProbEvidence sampler once by calling start(), so use maxRuns - 1
    val accepted = compareMeans(probEvidenceSampler, newProbEvidenceSampler, logConstant, maxRuns - 1)

    // Update the probability of evidence sampler and kill the one we don't keep
    // Calling deregister might be unnecessary if the algorithm deregisters itself when killed
    if(accepted) {
      probEvidenceSampler.kill()
      universe.deregisterAlgorithm(probEvidenceSampler)
      probEvidenceSampler = newProbEvidenceSampler
    }
    else {
      newProbEvidenceSampler.kill()
      universe.deregisterAlgorithm(newProbEvidenceSampler)
    }

    // Restore the universe state, since it may have been modified while running probability of evidence sampling.
    universeState.restore()

    accepted
  }

  /**
   * Record the current values of all MAP elements.
   */
  protected def currentMAPValues: List[ElemVal[_]] = {
    // For whatever reason, the Scala compiler complains if we try to make this an anonymous function.
    def makeElemVal[T](elem: Element[_]) = ElemVal[T](elem.asInstanceOf[Element[T]], elem.value.asInstanceOf[T])
    mapElements.map(makeElemVal)
  }

  /**
   * Decides whether or not the mean of the old sampler, multiplied by the constant given, is likely to be less than the
   * mean of the new sampler. Computes in log space to avoid underflow. This may mutate the state of the universe. This
   * does not take into account conditions and constraints on the MAP elements directly; these should be incorporated in
   * the log constant provided.
   * @param oldSampler Probability of evidence sampler for the previous state of the MAP elements.
   * @param newSampler Probability of evidence sampler for the next state of the MAP elements.
   * @param logConstant Log of a multiplicative constant, by which we multiply the mean of the old sampler.
   * @param runs Maximum allowed additional runs of probability of evidence sampling before this method should return a
   * best guess. This is a kill switch to avoid taking an absurd number of samples when the difference between the means
   * is negligible. Must be >= 0. Setting this to 0 is equivalent to performing no hypothesis test at all and just
   * comparing the values.
   * @return A decision to accept based on a one-sided t-test of the weights sampled from the two samplers.
   */
  @tailrec
  protected final def compareMeans(oldSampler: MMAPProbEvidenceSampler, newSampler: MMAPProbEvidenceSampler,
                           logConstant: Double, runs: Int): Boolean = {
    val oldLogStats = oldSampler.totalLogStatistics.multiplyByConstant(logConstant)
    val newLogStats = newSampler.totalLogStatistics

    // If we aren't allowed to take more samples, our best guess is to return the comparison of the sample means.
    // Otherwise, perform the t-test to see if we're confident that the means differ. We are sure that both counts are
    // greater than 1 because both counts must be at least samplesPerIteration.
    if(runs == 0 || LogStatistics.oneSidedTTest(oldLogStats, newLogStats) < tolerance) {
      oldLogStats.logMean < newLogStats.logMean
    }
    // If we can't decide with the information we have, take more samples and try again.
    // Run both samplers for the same number of additional iterations.
    else {
      oldSampler.run()
      newSampler.run()
      compareMeans(oldSampler, newSampler, logConstant, runs - 1)
    }
  }

  override def sample(): (Boolean, Sample) = {
    mhStep()
    if(dissatisfied.isEmpty) {
      // Update as long as no MAP elements are dissatisfied
      val values = mapElements.map(elem => elem -> elem.value)
      (true, mutable.Map(values:_*))
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
    allLastUpdates(target)._1.asInstanceOf[T]
  }

  override protected def doInitialize(): Unit = {
    super.doInitialize()
    // Only record dissatisfied MAP elements
    dissatisfied = dissatisfied.intersect(fastTargets)

    // Copy the universe state, since the probability of evidence sampler may corrupt it
    val universeState = new UniverseState(universe)
    preserve = universeState.myActiveElements

    // Run the probability of evidence sampler for samplesPerIteration
    probEvidenceSampler = new MMAPProbEvidenceSampler(currentMAPValues)
    probEvidenceSampler.start()
    universeState.restore()
  }

  /*
   * Prevent memory leaks by killing internal algorithms and clearing the cache.
   */
  override def cleanUp(): Unit = {
    super.cleanUp()
    universe.clearTemporaries()
    chainCache.clear()
    probEvidenceSampler.kill()
    // This is unnecessary if the algorithm deregisters itself when killed
    universe.deregisterAlgorithm(probEvidenceSampler)
    probEvidenceSampler = null
    preserve = null
  }

  /*
   * We don't want to force updates at the end of sampling, since the current values of MAP elements are not necessarily
   * the values that maximize the probability of evidence. We only want to make changes to allLastUpdates when a new set
   * of values increases the probability of evidence.
   */
  override def update(): Unit = {}

  /**
   * Special probability of evidence sampler used for marginal MAP. Unlike a regular probability of evidence sampler,
   * this records its own variance. It does so in an online fashion, and computes it in log space to prevent underflow.
   * Additionally, this algorithm may be run multiple times. The rolling mean and variance computation incorporates the
   * samples taken from all runs.
   * @param observations Elements and corresponding values that should be observed each time this algorithm is run.
   * Normally, this contains MAP elements and their proposed values.
   */
  class MMAPProbEvidenceSampler(val observations: List[ElemVal[_]]) extends ProbEvidenceSampler(universe)
    with OneTimeProbEvidenceSampler with OnlineLogStatistics {

    override val numSamples = samplesPerIteration

    /**
     * Observe the necessary values of MAP elements, then run the algorithm. After this is initialized, calling this
     * method again is allowed. The additional samples are accounted for when returning the total log statistics.
     */
    override def run(): Unit = {
      for(ElemVal(elem, value) <- observations) elem.observe(value)
      super.run()
    }

    /**
     * Perform sampling, but additionally update the variance and clear only elements that shouldn't be preserved.
     */
    override protected def doSample(): Unit = {
      totalWeight += 1

      try {
        val weight = lw.computeWeight(universe.activeElements)
        successWeight = logSum(successWeight, weight)
        // Record the weight for the variance computation
        record(weight)
      } catch {
        case Importance.Reject => record(Double.NegativeInfinity)
      }

      // Deactivate only the temporary elements created during probability of evidence sampling
      for(elem <- universe.activeElements) {
        // Since an element deactivates its direct context contents when deactivated, it's possible that an element
        // in the list will be deactivated before we reach it, so we have to check again that it is active
        if(elem.active && !preserve.contains(elem)) elem.deactivate()
      }
    }
  }
}

class AnytimeProbEvidenceMarginalMAP(universe: Universe,
                                     tolerance: Double,
                                     samplesPerIteration: Int,
                                     maxRuns: Int,
                                     proposalScheme: ProposalScheme,
                                     schedule: Schedule,
                                     mapElements: List[Element[_]])
  extends ProbEvidenceMarginalMAP(universe, tolerance, samplesPerIteration, maxRuns, proposalScheme, schedule, mapElements)
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
                                     tolerance: Double,
                                     samplesPerIteration: Int,
                                     maxRuns: Int,
                                     proposalScheme: ProposalScheme,
                                     schedule: Schedule,
                                     mapElements: List[Element[_]])
  extends ProbEvidenceMarginalMAP(universe, tolerance, samplesPerIteration, maxRuns, proposalScheme, schedule, mapElements)
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
   * Creates a one time marginal MAP algorithm that uses probability of evidence.
   * @see [[com.cra.figaro.experimental.marginalmap.ProbEvidenceMarginalMAP]] abstract class for a complete description
   * of the parameters.
   */
  def apply(iterations: Int, tolerance: Double, samplesPerIteration: Int, maxRuns: Int, proposalScheme: ProposalScheme,
            schedule: Schedule, mapElements: Element[_]*)(implicit universe: Universe) =
    new OneTimeProbEvidenceMarginalMAP(iterations, universe, tolerance, samplesPerIteration, maxRuns, proposalScheme, schedule, mapElements.toList)

  /**
   * Creates an anytime marginal MAP algorithm that uses probability of evidence.
   * @see [[com.cra.figaro.experimental.marginalmap.ProbEvidenceMarginalMAP]] abstract class for a complete description
   * of the parameters.
   */
  def apply(tolerance: Double, samplesPerIteration: Int, maxRuns: Int, proposalScheme: ProposalScheme,
            schedule: Schedule, mapElements: Element[_]*)(implicit universe: Universe) =
  new AnytimeProbEvidenceMarginalMAP(universe, tolerance, samplesPerIteration, maxRuns, proposalScheme, schedule, mapElements.toList)

  /**
   * Creates a one time marginal MAP algorithm that uses probability of evidence. Takes 100 samples per iteration at a
   * tolerance of 0.05, up to a maximum of 100 runs. Uses the default proposal scheme and schedule.
   * @see [[com.cra.figaro.experimental.marginalmap.ProbEvidenceMarginalMAP]] abstract class for a complete description
   * of the parameters.
   */
  def apply(iterations: Int, mapElements: Element[_]*)(implicit universe: Universe) =
  new OneTimeProbEvidenceMarginalMAP(iterations, universe, 0.05, 100, 100, ProposalScheme.default(universe), Schedule.default(), mapElements.toList)

  /**
   * Creates an anytime marginal MAP algorithm that uses probability of evidence. Takes 100 samples per iteration at a
   * tolerance of 0.05, up to a maximum of 100 runs. Uses the default proposal scheme and schedule.
   * @see [[com.cra.figaro.experimental.marginalmap.ProbEvidenceMarginalMAP]] abstract class for a complete description
   * of the parameters.
   */
  def apply(mapElements: Element[_]*)(implicit universe: Universe) =
    new AnytimeProbEvidenceMarginalMAP(universe, 0.05, 100, 100, ProposalScheme.default(universe), Schedule.default(), mapElements.toList)
}
