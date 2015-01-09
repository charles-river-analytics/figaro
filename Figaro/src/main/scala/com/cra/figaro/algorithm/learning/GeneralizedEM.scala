/*
 * GeneralizedEM.scala
 * Expectation maximization algorithm using any ProbQueryAlgorithm as the inference algorithm.
 * 
 * Created By:      Michael Howard (mhoward@cra.com)
 * Creation Date:   Jun 1, 2013
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.algorithm.learning

import com.cra.figaro.language._
import com.cra.figaro.algorithm.{ Algorithm, ParameterLearner, ProbQueryAlgorithm, OneTime }
import com.cra.figaro.algorithm.factored.beliefpropagation.BeliefPropagation
import com.cra.figaro.algorithm.sampling.{ Importance, MetropolisHastings, ProposalScheme }
import com.cra.figaro.algorithm.factored.factors.Factory
import com.cra.figaro.patterns.learning.ModelParameters
import com.cra.figaro.algorithm.factored.SufficientStatisticsVariableElimination


/**
 * Base class of Expectation Maximization algorithms. This class also implements the outer EM loop and checks against termination criteria.
 */
abstract class ExpectationMaximization(universe: Universe, val terminationCriteria: () => EMTerminationCriteria, targetParameters: Parameter[_]*) extends Algorithm with ParameterLearner {

  protected def doStart(): Unit = {
    em()
  }

  protected def doExpectationStep(): Map[Parameter[_], Seq[Double]]

  var sufficientStatistics: Map[Parameter[_], Seq[Double]] = Map.empty[Parameter[_], Seq[Double]]
  val debug = false
  protected def em(): Unit = {
    //Instantiate termination criteria here.
    val shouldTerminate = terminationCriteria()
    if (debug) println("Entering EM loop")
    while (shouldTerminate(sufficientStatistics) == false) {
      iteration()
    }
  }

  protected def doMaximizationStep(parameterMapping: Map[Parameter[_], Seq[Double]]): Unit = {

    for (p <- targetParameters) yield {
      p.maximize(parameterMapping(p))
    }
  }

  def iteration(): Unit = {
    sufficientStatistics = doExpectationStep()
    doMaximizationStep(sufficientStatistics)
    if (debug) println("Completed iteration")
  }

  /*
   * Stop the algorithm from computing. The algorithm is still ready to provide answers after it returns.
   */
  protected def doStop(): Unit = {}

  /*
   * Resume the computation of the algorithm, if it has been stopped.
   */

  protected def doResume(): Unit = {}

  /*
   * Kill the algorithm so that it is inactive. It will no longer be able to provide answers.
   */

  protected def doKill(): Unit = {}

}

/**
 * Expectation maximization iteratively produces an estimate of sufficient statistics for learnable parameters,
 * then maximizes the parameters according to the estimate. It uses an factored inference algorithm, SufficientStatisticsVariableElimination,
 * to produce the estimate of the sufficient statistics. This class can be extended with a different expectation
 * or maximization algorithm; see the code for details.
 */
class ExpectationMaximizationWithFactors(universe: Universe, targetParameters: Parameter[_]*)(terminationCriteria: () => EMTerminationCriteria) extends ExpectationMaximization(universe, terminationCriteria, targetParameters: _*) {

  //Defines the length of the sequences corresponding to different parameters
  protected val paramMap: Map[Parameter[_], Seq[Double]] = Map(targetParameters.map(p => p -> p.zeroSufficientStatistics): _*)

  protected def doExpectationStep(): Map[Parameter[_], Seq[Double]] = {
    val algorithm = SufficientStatisticsVariableElimination(paramMap)(universe)
    algorithm.start
    val result = algorithm.getSufficientStatisticsForAllParameters
    algorithm.kill
    result
  }

}

class GeneralizedEM(inferenceAlgorithmConstructor: Seq[Element[_]] => Universe => ProbQueryAlgorithm with OneTime, universe: Universe, targetParameters: Parameter[_]*)(terminationCriteria: () => EMTerminationCriteria) extends ExpectationMaximization(universe, terminationCriteria, targetParameters: _*) {
  /*
   * Start the algorithm. After it returns, the algorithm must be ready to provide answers.
   */

  protected def doExpectationStep(): Map[Parameter[_], Seq[Double]] = {
    val inferenceTargets =
      universe.activeElements.filter(_.isInstanceOf[Parameterized[_]]).map(_.asInstanceOf[Parameterized[_]])

    val algorithm = inferenceAlgorithmConstructor(inferenceTargets)(universe)
    algorithm.start()

    var result: Map[Parameter[_], Seq[Double]] = Map()

    for { parameter <- targetParameters } {
      var stats = parameter.zeroSufficientStatistics
      for {
        target <- universe.directlyUsedBy(parameter)
      } {
        val t: Parameterized[target.Value] = target.asInstanceOf[Parameterized[target.Value]]
        val distribution: Stream[(Double, target.Value)] = algorithm.distribution(t)
        val newStats = t.distributionToStatistics(parameter, distribution)
        stats = (stats.zip(newStats)).map(pair => pair._1 + pair._2)
      }
      result += parameter -> stats
    }
    algorithm.kill()
    result
  }

}

object EMWithBP {
  private def makeBP(numIterations: Int, targets: Seq[Element[_]])(universe: Universe) = {
    Factory.removeFactors()
    BeliefPropagation(numIterations, targets: _*)(universe)
  }
  /**
   * An expectation maximization algorithm using Belief Propagation sampling for inference.
   *
   * @param params parameters to target with EM algorithm
   */
  def apply(params: ModelParameters)(implicit universe: Universe) = {
    val parameters = params.convertToParameterList
    new GeneralizedEM((targets: Seq[Element[_]]) => (universe: Universe) => makeBP(10, targets)(universe), universe, parameters: _*)(EMTerminationCriteria.maxIterations(10))
  }
  /**
   * An expectation maximization algorithm using Belief Propagation sampling for inference.
   * @param emIterations number of iterations of the EM algorithm
   * @param bpIterations number of iterations of the BP algorithm
   * @param params parameters to target with EM algorithm
   */
  def apply(emIterations: Int, bpIterations: Int, p: ModelParameters)(implicit universe: Universe) = {
    val parameters = p.convertToParameterList
    new GeneralizedEM((targets: Seq[Element[_]]) => (universe: Universe) => makeBP(bpIterations, targets)(universe), universe, parameters: _*)(EMTerminationCriteria.maxIterations(emIterations))
  }

  /**
   * An expectation maximization algorithm using Belief Propagation sampling for inference.
   * @param params parameters to target with EM algorithm
   */
  def apply(params: Parameter[_]*)(implicit universe: Universe) =
    new GeneralizedEM((targets: Seq[Element[_]]) => (universe: Universe) => makeBP(10, targets)(universe), universe, params: _*)(EMTerminationCriteria.maxIterations(10))

  /**
   * An expectation maximization algorithm using importance sampling for inference.
   * @param emIterations number of iterations of the EM algorithm
   * @param bpIterations number of iterations of the BP algorithm
   * @param params parameters to target with EM algorithm
   */
  def apply(emIterations: Int, bpIterations: Int, params: Parameter[_]*)(implicit universe: Universe) =
    new GeneralizedEM((targets: Seq[Element[_]]) => (universe: Universe) => makeBP(bpIterations, targets)(universe), universe, params: _*)(EMTerminationCriteria.maxIterations(emIterations))

  /**
   * An expectation maximization algorithm using importance sampling for inference.
   * @param terminationCriteria criteria for stopping the EM algorithm
   * @param bpIterations number of iterations of the BP algorithm
   * @param params parameters to target with EM algorithm
   */
  def apply(terminationCriteria: () => EMTerminationCriteria, bpIterations: Int, params: Parameter[_]*)(implicit universe: Universe) =
    new GeneralizedEM((targets: Seq[Element[_]]) => (universe: Universe) => makeBP(bpIterations, targets)(universe), universe, params: _*)(terminationCriteria)
}

object EMWithImportance {
  private def makeImportance(numParticles: Int, targets: Seq[Element[_]])(universe: Universe) = {
    Importance(numParticles, targets: _*)(universe)
  }

  /**
   * An expectation maximization algorithm using importance sampling for inference.
   *
   * @param emIterations number of iterations of the EM algorithm
   * @param importanceParticles number of particles of the importance sampling algorithm
   */
  def apply(emIterations: Int, importanceParticles: Int, p: Parameter[_]*)(implicit universe: Universe) =
    new GeneralizedEM((targets: Seq[Element[_]]) => (universe: Universe) => makeImportance(importanceParticles, targets)(universe), universe, p: _*)(EMTerminationCriteria.maxIterations(emIterations))

  /**
   * An expectation maximization algorithm using importance sampling for inference.
   *
   * @param terminationCriteria criteria for stopping the EM algorithm
   * @param importanceParticles number of particles of the importance sampling algorithm
   */
  def apply(terminationCriteria: () => EMTerminationCriteria, importanceParticles: Int, p: Parameter[_]*)(implicit universe: Universe) =
    new GeneralizedEM((targets: Seq[Element[_]]) => (universe: Universe) => makeImportance(importanceParticles, targets)(universe), universe, p: _*)(terminationCriteria)

  /**
   * An expectation maximization algorithm using importance sampling for inference.
   *
   * @param params parameters to target with EM algorithm
   */
  def apply(params: ModelParameters)(implicit universe: Universe) = {
    val parameters = params.convertToParameterList
    new GeneralizedEM((targets: Seq[Element[_]]) => (universe: Universe) => makeImportance(100000, targets)(universe), universe, parameters: _*)(EMTerminationCriteria.maxIterations(10))
  }

  /**
   * An expectation maximization algorithm using importance sampling for inference.
   *
   * @param emIterations number of iterations of the EM algorithm
   * @param importanceParticles number of particles of the importance sampling algorithm
   * @param params parameters to target with EM algorithm
   */
  def apply(emIterations: Int, importanceParticles: Int, params: ModelParameters)(implicit universe: Universe) = {
    val parameters = params.convertToParameterList
    new GeneralizedEM((targets: Seq[Element[_]]) => (universe: Universe) => makeImportance(100000, targets)(universe), universe, parameters: _*)(EMTerminationCriteria.maxIterations(emIterations))
  }

  /**
   * An expectation maximization algorithm using importance sampling for inference.
   *
   * @param terminationCriteria criteria for stopping the EM algorithm
   * @param importanceParticles number of particles of the importance sampling algorithm
   * @param params parameters to target with EM algorithm
   */
  def apply(terminationCriteria: () => EMTerminationCriteria, importanceParticles: Int, params: ModelParameters)(implicit universe: Universe) = {
    val parameters = params.convertToParameterList
    new GeneralizedEM((targets: Seq[Element[_]]) => (universe: Universe) => makeImportance(100000, targets)(universe), universe, parameters: _*)(terminationCriteria)
  }

}

object EMWithMH {
  private def makeMH(numParticles: Int, proposalScheme: ProposalScheme, targets: Seq[Element[_]])(universe: Universe) = {
    MetropolisHastings(numParticles, proposalScheme, targets: _*)(universe)
  }

  /**
   * An expectation maximization algorithm using Metropolis Hastings for inference.
   *
   * @param emIterations number of iterations of the EM algorithm
   * @param mhParticles number of particles of the MH algorithm
   */
  def apply(emIterations: Int, mhParticles: Int, p: Parameter[_]*)(implicit universe: Universe) =
    new GeneralizedEM((targets: Seq[Element[_]]) => (universe: Universe) => makeMH(mhParticles, ProposalScheme.default(universe), targets)(universe), universe, p: _*)(EMTerminationCriteria.maxIterations(emIterations))

  /**
   * An expectation maximization algorithm using Metropolis Hastings for inference.
   * @param terminationCriteria criteria for stopping the EM algorithm
   * @param mhParticles number of particles of the MH algorithm
   * @param params parameters to target in EM algorithm
   */
  def apply(terminationCriteria: () => EMTerminationCriteria, mhParticles: Int, params: Parameter[_]*)(implicit universe: Universe) =
    new GeneralizedEM((targets: Seq[Element[_]]) => (universe: Universe) => makeMH(mhParticles, ProposalScheme.default(universe), targets)(universe), universe, params: _*)(terminationCriteria)

  /**
   * An expectation maximization algorithm using Metropolis Hastings for inference.
   *
   * @param iterations number of iterations of the EM algorithm
   * @param mhParticles number of particles of the MH algorithm
   * @param proposalScheme proposal scheme for MH algorithm
   * @param params parameters to target in EM algorithm
   */
  def apply(emIterations: Int, mhParticles: Int, proposalScheme: ProposalScheme, params: Parameter[_]*)(implicit universe: Universe) =
    new GeneralizedEM((targets: Seq[Element[_]]) => (universe: Universe) => makeMH(mhParticles, proposalScheme, targets)(universe), universe, params: _*)(EMTerminationCriteria.maxIterations(emIterations))

  /**
   * An expectation maximization algorithm using Metropolis Hastings for inference.
   * @param params parameters to target in EM algorithm
   */
  def apply(p: ModelParameters)(implicit universe: Universe) = {
    val parameters = p.convertToParameterList
    new GeneralizedEM((targets: Seq[Element[_]]) => (universe: Universe) => makeMH(100000, ProposalScheme.default(universe), targets)(universe), universe, parameters: _*)(EMTerminationCriteria.maxIterations(10))
  }

  /**
   * An expectation maximization algorithm using Metropolis Hastings for inference.
   *
   * @param iterations number of iterations of the EM algorithm
   * @param mhParticles number of particles of the MH algorithm
   * @param proposalScheme proposal scheme for MH algorithm
   * @param params parameters to target in EM algorithm
   */
  def apply(emIterations: Int, mhParticles: Int, proposalScheme: ProposalScheme, p: ModelParameters)(implicit universe: Universe) = {
    val parameters = p.convertToParameterList
    new GeneralizedEM((targets: Seq[Element[_]]) => (universe: Universe) => makeMH(mhParticles, proposalScheme, targets)(universe), universe, parameters: _*)(EMTerminationCriteria.maxIterations(emIterations))
  }

  /**
   * An expectation maximization algorithm using Metropolis Hastings for inference.
   *
   * @param terminationCriteria criteria for stopping the EM algorithm
   * @param mhParticles number of particles of the MH algorithm
   * @param proposalScheme proposal scheme for MH algorithm
   * @param params parameters to target in EM algorithm
   */
  def apply(terminationCriteria: () => EMTerminationCriteria, mhParticles: Int, proposalScheme: ProposalScheme, params: ModelParameters)(implicit universe: Universe) = {
    val parameters = params.convertToParameterList
    new GeneralizedEM((targets: Seq[Element[_]]) => (universe: Universe) => makeMH(mhParticles, proposalScheme, targets)(universe), universe, parameters: _*)(terminationCriteria)
  }

}

object EMWithVE {
  /**
   * An expectation maximization algorithm which will run for the default of 10 iterations.
   */
  def apply(p: Parameter[_]*)(implicit universe: Universe) =
    new ExpectationMaximizationWithFactors(universe, p: _*)(EMTerminationCriteria.maxIterations(10))
  /**
   * An expectation maximization algorithm which will run for the default of 10 iterations.
   */
  def apply(p: ModelParameters)(implicit universe: Universe) =
    new ExpectationMaximizationWithFactors(universe, p.convertToParameterList: _*)(EMTerminationCriteria.maxIterations(10))

  /**
   * An expectation maximization algorithm which will run for the number of iterations specified.
   */
  def apply(iterations: Int, p: ModelParameters)(implicit universe: Universe) =
    new ExpectationMaximizationWithFactors(universe, p.convertToParameterList: _*)(EMTerminationCriteria.maxIterations(iterations))
  /**
   * An expectation maximization algorithm which will run for the number of iterations specified.
   */
  def apply(iterations: Int, p: Parameter[_]*)(implicit universe: Universe) =
    new ExpectationMaximizationWithFactors(universe, p: _*)(EMTerminationCriteria.maxIterations(iterations))

  /**
   * An expectation maximization algorithm which will stop according to a user specified termination criteria.
   */
  def apply(terminationCriteria: () => EMTerminationCriteria, p: Parameter[_]*)(implicit universe: Universe) =
    new ExpectationMaximizationWithFactors(universe, p: _*)(terminationCriteria)

}
