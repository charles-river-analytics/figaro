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
import com.cra.figaro.algorithm.factored.factors.factory.Factory
import com.cra.figaro.patterns.learning.ModelParameters
import com.cra.figaro.algorithm.factored.SufficientStatisticsVariableElimination
import com.cra.figaro.algorithm.online.Online
import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.algorithm.factored.factors.Variable
import com.cra.figaro.algorithm.sampling.Forward

/**
 * Expectation maximization iteratively produces an estimate of sufficient statistics for learnable parameters,
 * then maximizes the parameters according to the estimate. This trait can be extended with a different expectation
 * or maximization algorithm; see the code for details.
 */
trait ExpectationMaximization extends Algorithm with ParameterLearner {
  protected val paramMap: Map[Parameter[_], Seq[Double]] = Map(targetParameters.map(p => p -> p.zeroSufficientStatistics): _*)
  protected def doExpectationStep(): Map[Parameter[_], Seq[Double]]

  protected[algorithm] def doStart(): Unit = {
    em()
  }

  /*
   * Stop the algorithm from computing. The algorithm is still ready to provide answers after it returns.
   */
  protected[algorithm] def doStop(): Unit = {}

  /*
   * Resume the computation of the algorithm, if it has been stopped.
   */

  protected[algorithm] def doResume(): Unit = {}

  /*
   * Kill the algorithm so that it is inactive. It will no longer be able to provide answers.
   */
  protected[algorithm] def doKill(): Unit = {}

  val terminationCriteria: () => EMTerminationCriteria
  val targetParameters: Seq[Parameter[_]]
  var sufficientStatistics: Map[Parameter[_], Seq[Double]] = Map.empty[Parameter[_], Seq[Double]]
  var debug = false
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
}

/**
 * An EM algorithm which learns parameters incrementally
 */
trait OnlineExpectationMaximization extends Online with ExpectationMaximization {

  override def doStart = {}

  protected var lastIterationStatistics: Map[Parameter[_], Seq[Double]] = Map(targetParameters.map(p => p -> p.zeroSufficientStatistics): _*)
  override val initial: Universe
  override val transition: Function0[Universe]
  protected var currentUniverse: Universe = initial

  private def updateStatistics(newStatistics: Map[Parameter[_], Seq[Double]]): Map[Parameter[_], Seq[Double]] = {
    Map((for (p <- paramMap.keys) yield {
      val updatedStatistics = (lastIterationStatistics(p) zip newStatistics(p)).map((pair: (Double, Double)) => pair._1 + pair._2)
      (p, updatedStatistics)
    }).toSeq: _*)
  }

  /**
   * Observe new evidence and perform one expectation step and one maximization step
   */
  def update(evidence: Seq[NamedEvidence[_]] = Seq()): Unit = {
    currentUniverse = transition()
    currentUniverse.assertEvidence(evidence)
    val newStatistics = doExpectationStep
    val updated = updateStatistics(newStatistics)
    doMaximizationStep(updated)
    lastIterationStatistics = updated
  }

}

/**
 * An EM algorithm which learns parameters using a factored algorithm
 */
class ExpectationMaximizationWithFactors(val universe: Universe, val targetParameters: Parameter[_]*)(val terminationCriteria: () => EMTerminationCriteria) extends ExpectationMaximization {

  protected def doExpectationStep(): Map[Parameter[_], Seq[Double]] = {
    val algorithm = SufficientStatisticsVariableElimination(paramMap)(universe)
    algorithm.start
    val result = algorithm.getSufficientStatisticsForAllParameters
    algorithm.kill
    result
  }

}

/**
 * An online EM algorithm which learns parameters using a factored algorithm
 */
class OnlineExpectationMaximizationWithFactors(override val initial: Universe, override val transition: Function0[Universe], val targetParameters: Parameter[_]*)(val terminationCriteria: () => EMTerminationCriteria)
  extends OnlineExpectationMaximization {

  def doExpectationStep = {
    val algorithm = SufficientStatisticsVariableElimination(paramMap)(currentUniverse)
    algorithm.start
    algorithm.stop
    val newStatistics = algorithm.getSufficientStatisticsForAllParameters
    algorithm.kill
    newStatistics
  }
}

/**
 * An EM algorithm which learns parameters using an inference algorithm provided as an argument
 */
class GeneralizedEM(inferenceAlgorithmConstructor: Seq[Element[_]] => Universe => ProbQueryAlgorithm with OneTime, val universe: Universe, val targetParameters: Parameter[_]*)(val terminationCriteria: () => EMTerminationCriteria) extends ExpectationMaximization {

  //Dependent universe doesn't work the same way.
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
        if (inferenceTargets.contains(t)) {
          val distribution: Stream[(Double, target.Value)] = algorithm.distribution(t)
          val newStats = t.distributionToStatistics(parameter, distribution)
          stats = (stats.zip(newStats)).map(pair => pair._1 + pair._2)
        }
      }
      result += parameter -> stats
    }
    algorithm.kill()
    result
  }

}

/**
 * An EM algorithm which learns parameters using an inference algorithm provided as an argument
 */
class GeneralizedOnlineEM(inferenceAlgorithmConstructor: Seq[Element[_]] => Universe => ProbQueryAlgorithm with OneTime, override val initial: Universe, override val transition: Function0[Universe], val targetParameters: Parameter[_]*)(val terminationCriteria: () => EMTerminationCriteria) extends OnlineExpectationMaximization {

  protected def usesParameter(l: List[Element[_]]): Map[Parameter[_], Iterable[Parameterized[_]]] = {
    (l.map { x => x match { case p: Parameterized[_] => { p -> p.parameters.head } } }).groupBy(_._2).mapValues(_.map(_._1))
  }

  protected def doExpectationStep(): Map[Parameter[_], Seq[Double]] = {
    val inferenceTargets =
      currentUniverse.activeElements.filter(_.isInstanceOf[Parameterized[_]]).map(_.asInstanceOf[Parameterized[_]])

    val algorithm = inferenceAlgorithmConstructor(inferenceTargets)(currentUniverse)
    algorithm.start()
    //println("universe: " + currentUniverse.hashCode)
    var result: Map[Parameter[_], Seq[Double]] = Map()

    val uses = usesParameter(inferenceTargets)
    for { parameter <- targetParameters } {
      var stats = parameter.zeroSufficientStatistics
      if (uses.contains(parameter)) {
        for {
          target <- uses(parameter)
        } {
          val t: Parameterized[target.Value] = target.asInstanceOf[Parameterized[target.Value]]
          if (inferenceTargets.contains(t)) {
            val distribution: Stream[(Double, target.Value)] = algorithm.distribution(t)
            val newStats = t.distributionToStatistics(parameter, distribution)
            stats = (stats.zip(newStats)).map(pair => pair._1 + pair._2)
          }
        }
      }
      result += parameter -> stats
    }
    algorithm.kill()
    result
  }

}

object EMWithBP {

  private val defaultBPIterations = 10

  def online(transition: () => Universe, p: Parameter[_]*)(implicit universe: Universe) = {
    new GeneralizedOnlineEM((targets: Seq[Element[_]]) => (universe: Universe) => makeBP(defaultBPIterations, targets)(universe), universe, transition, p: _*)(EMTerminationCriteria.maxIterations(10))
  }

  def online(transition: () => Universe, p: ModelParameters)(implicit universe: Universe) = {
    new GeneralizedOnlineEM((targets: Seq[Element[_]]) => (universe: Universe) => makeBP(defaultBPIterations, targets)(universe), universe, transition, p.convertToParameterList: _*)(EMTerminationCriteria.maxIterations(10))
  }

  private def makeBP(numIterations: Int, targets: Seq[Element[_]])(universe: Universe) = {
    Variable.clearCache
    BeliefPropagation(numIterations, targets: _*)(universe)
  }
  /**
   * An expectation maximization algorithm using Belief Propagation sampling for inference.
   *
   * @param params parameters to target with EM algorithm
   */
  def apply(params: ModelParameters)(implicit universe: Universe) = {
    println("Warning: Using BP with EM can have produce unpredictable behavior if parameterized elements are created inside a Chain.")
    val parameters = params.convertToParameterList
    new GeneralizedEM((targets: Seq[Element[_]]) => (universe: Universe) => makeBP(defaultBPIterations, targets)(universe), universe, parameters: _*)(EMTerminationCriteria.maxIterations(10))
  }
  /**
   * An expectation maximization algorithm using Belief Propagation for inference.
   * @param emIterations number of iterations of the EM algorithm
   * @param bpIterations number of iterations of the BP algorithm
   * @param params parameters to target with EM algorithm
   */
  def apply(emIterations: Int, bpIterations: Int, p: ModelParameters)(implicit universe: Universe) = {
    println("Warning: Using BP with EM can have produce unpredictable behavior if parameterized elements are created inside a Chain.")
    val parameters = p.convertToParameterList
    new GeneralizedEM((targets: Seq[Element[_]]) => (universe: Universe) => makeBP(bpIterations, targets)(universe), universe, parameters: _*)(EMTerminationCriteria.maxIterations(emIterations))
  }

  /**
   * An expectation maximization algorithm using Belief Propagation for inference.
   * @param params parameters to target with EM algorithm
   */
  def apply(params: Parameter[_]*)(implicit universe: Universe) = {
    println("Warning: Using BP with EM can have produce unpredictable behavior if parameterized elements are created inside a Chain.")
    new GeneralizedEM((targets: Seq[Element[_]]) => (universe: Universe) => makeBP(defaultBPIterations, targets)(universe), universe, params: _*)(EMTerminationCriteria.maxIterations(10))
  }

  /**
   * An expectation maximization algorithm using Belief Propagation for inference.
   * @param emIterations number of iterations of the EM algorithm
   * @param bpIterations number of iterations of the BP algorithm
   * @param params parameters to target with EM algorithm
   */
  def apply(emIterations: Int, bpIterations: Int, params: Parameter[_]*)(implicit universe: Universe) = {
    println("Warning: Using BP with EM can have produce unpredictable behavior if parameterized elements are created inside a Chain.")
    new GeneralizedEM((targets: Seq[Element[_]]) => (universe: Universe) => makeBP(bpIterations, targets)(universe), universe, params: _*)(EMTerminationCriteria.maxIterations(emIterations))
  }

  /**
   * An expectation maximization algorithm using Belief Propagation for inference.
   * @param terminationCriteria criteria for stopping the EM algorithm
   * @param bpIterations number of iterations of the BP algorithm
   * @param params parameters to target with EM algorithm
   */
  def apply(terminationCriteria: () => EMTerminationCriteria, bpIterations: Int, params: Parameter[_]*)(implicit universe: Universe) = {
    println("Warning: Using BP with EM can have produce unpredictable behavior if parameterized elements are created inside a Chain.")
    new GeneralizedEM((targets: Seq[Element[_]]) => (universe: Universe) => makeBP(bpIterations, targets)(universe), universe, params: _*)(terminationCriteria)
  }
}

object EMWithImportance {

  private val defaultImportanceParticles = 100000

  private def makeImportance(numParticles: Int, targets: Seq[Element[_]])(universe: Universe) = {
    Importance(numParticles, targets: _*)(universe)
  }

  def online(transition: () => Universe, p: Parameter[_]*)(implicit universe: Universe) = {
    new GeneralizedOnlineEM((targets: Seq[Element[_]]) => (universe: Universe) => makeImportance(defaultImportanceParticles, targets)(universe), universe, transition, p: _*)(EMTerminationCriteria.maxIterations(10))
  }

  def online(transition: () => Universe, p: ModelParameters)(implicit universe: Universe) = {
    new GeneralizedOnlineEM((targets: Seq[Element[_]]) => (universe: Universe) => makeImportance(defaultImportanceParticles, targets)(universe), universe, transition, p.convertToParameterList: _*)(EMTerminationCriteria.maxIterations(10))
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
    new GeneralizedEM((targets: Seq[Element[_]]) => (universe: Universe) => makeImportance(defaultImportanceParticles, targets)(universe), universe, parameters: _*)(EMTerminationCriteria.maxIterations(10))
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
    new GeneralizedEM((targets: Seq[Element[_]]) => (universe: Universe) => makeImportance(defaultImportanceParticles, targets)(universe), universe, parameters: _*)(EMTerminationCriteria.maxIterations(emIterations))
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

  private val defaultMHParticles = 100000

  private def makeImportance(numParticles: Int, targets: Seq[Element[_]])(universe: Universe) = {
    Importance(numParticles, targets: _*)(universe)
  }

  def online(transition: () => Universe, p: Parameter[_]*)(implicit universe: Universe) = {
    new GeneralizedOnlineEM((targets: Seq[Element[_]]) => (universe: Universe) => makeMH(defaultMHParticles, ProposalScheme.default(universe), targets)(universe), universe, transition, p: _*)(EMTerminationCriteria.maxIterations(10))
  }

  def online(transition: () => Universe, p: ModelParameters)(implicit universe: Universe) = {
    new GeneralizedOnlineEM((targets: Seq[Element[_]]) => (universe: Universe) => makeMH(defaultMHParticles, ProposalScheme.default(universe), targets)(universe), universe, transition, p.convertToParameterList: _*)(EMTerminationCriteria.maxIterations(10))
  }

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
    new GeneralizedEM((targets: Seq[Element[_]]) => (universe: Universe) => makeMH(defaultMHParticles, ProposalScheme.default(universe), targets)(universe), universe, parameters: _*)(EMTerminationCriteria.maxIterations(10))
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

  def online(transition: () => Universe, p: Parameter[_]*)(implicit universe: Universe) = {
    new OnlineExpectationMaximizationWithFactors(universe, transition, p: _*)(EMTerminationCriteria.maxIterations(10))
  }

  def online(transition: () => Universe, p: ModelParameters)(implicit universe: Universe) = {
    new OnlineExpectationMaximizationWithFactors(universe, transition, p.convertToParameterList: _*)(EMTerminationCriteria.maxIterations(10))
  }

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
