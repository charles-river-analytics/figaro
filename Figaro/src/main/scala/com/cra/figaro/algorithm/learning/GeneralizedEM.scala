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
import com.cra.figaro.algorithm.{Algorithm, ParameterLearner, ProbQueryAlgorithm, OneTime}
import com.cra.figaro.algorithm.factored.beliefpropagation.BeliefPropagation
import com.cra.figaro.algorithm.sampling.{Importance, MetropolisHastings, ProposalScheme}
import com.cra.figaro.algorithm.factored.Factory

/*
 * @param inferenceAlgorithmConstructor
 */

class GeneralizedEM(inferenceAlgorithmConstructor: Seq[Element[_]] => Universe => ProbQueryAlgorithm with OneTime, universe: Universe, targetParameters: Parameter[_]*)(val numberOfIterations: Int) extends Algorithm with ParameterLearner {
  /*
   * Start the algorithm. After it returns, the algorithm must be ready to provide answers.
   */

  protected def doStart(): Unit = {
    em()
  }

  protected def em(): Unit = {
    for (iteration <- 1 to numberOfIterations) {
      /*
       * Obtain an estimate of sufficient statistics from expectation step
       */

      val sufficientStatistics = doExpectationStep()
      doMaximizationStep(sufficientStatistics)
    }
  }

  protected def doExpectationStep(): Map[Parameter[_], Seq[Double]] = {
    val inferenceTargets = 
      universe.activeElements.filter(_.isInstanceOf[Parameterized[_]]).map(_.asInstanceOf[Parameterized[_]])

    val algorithm = inferenceAlgorithmConstructor(inferenceTargets)(universe)
    algorithm.start()

    var result: Map[Parameter[_], Seq[Double]] = Map()
   
    for { parameter <- targetParameters } {
      var stats = parameter.zeroSufficientStatistics
      for { 
        target <- inferenceTargets
        if target.parameters.contains(parameter)
      } {
        val t: Parameterized[target.Value] = target.asInstanceOf[Parameterized[target.Value]]
        val distribution: Stream[(Double, target.Value)] = algorithm.distribution(t)
        val newStats = t.distributionToStatistics(parameter,distribution)
        stats = (stats.zip(newStats)).map(pair => pair._1 + pair._2)
      }
      result += parameter -> stats
    }
    algorithm.kill()
    result
  }

  protected def doMaximizationStep(parameterMapping: Map[Parameter[_], Seq[Double]]): Unit = {
   for (p <- targetParameters) yield {
      p.maximize(parameterMapping(p))
    }
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
 
object EMWithBP {
  private def makeBP(numIterations: Int, targets: Seq[Element[_]])(universe: Universe) = {    
    Factory.removeFactors()
    BeliefPropagation(numIterations, targets:_*)(universe)
  }
  
  /**
   * An expectation maximization algorithm which will run for the default of 10 iterations
   */
  def apply(p: Parameter[_]*)(implicit universe: Universe) =
    new GeneralizedEM((targets: Seq[Element[_]]) => (universe: Universe) => makeBP(10, targets)(universe), universe, p: _*)(10)

  /**
   * An expectation maximization algorithm which will run for the number of iterations specified
   */
  def apply(emIterations: Int, bpIterations: Int, p: Parameter[_]*)(implicit universe: Universe) =
    new GeneralizedEM((targets: Seq[Element[_]]) => (universe: Universe) => makeBP(bpIterations, targets)(universe), universe, p: _*)(emIterations)
}

object EMWithImportance {
  private def makeImportance(numParticles: Int, targets: Seq[Element[_]])(universe: Universe) = {
    Importance(numParticles, targets:_*)(universe)
  }
  
  /**
   * An expectation maximization algorithm using importance sampling for inference
   * 
   * @param emIterations number of iterations of the EM algorithm
   * @param importanceParticles number of particles of the importance sampling algorithm
   */
  def apply(emIterations: Int, importanceParticles: Int, p: Parameter[_]*)(implicit universe: Universe) =
    new GeneralizedEM((targets: Seq[Element[_]]) => (universe: Universe) => makeImportance(importanceParticles, targets)(universe), universe, p: _*)(emIterations)
}

object EMWithMH {
  private def makeMH(numParticles: Int, proposalScheme: ProposalScheme, targets: Seq[Element[_]])(universe: Universe) = {
    MetropolisHastings(numParticles, proposalScheme, targets:_*)(universe)
  }
  
  /**
   * An expectation maximization algorithm using Metropolis Hastings for inference.
   * 
   * @param emIterations number of iterations of the EM algorithm
   * @param mhParticles number of particles of the MH algorithm
   */
  def apply(emIterations: Int, mhParticles: Int, p: Parameter[_]*)(implicit universe: Universe) =
    new GeneralizedEM((targets: Seq[Element[_]]) => (universe: Universe) => makeMH(mhParticles, ProposalScheme.default(universe), targets)(universe), universe, p: _*)(emIterations)

  /**
   * An expectation maximization algorithm using Metropolis Hastings for inference.
   * 
   * @param emIterations number of iterations of the EM algorithm
   * @param mhParticles number of particles of the MH algorithm
   */
  def apply(emIterations: Int, mhParticles: Int, proposalScheme: ProposalScheme, p: Parameter[_]*)(implicit universe: Universe) =
    new GeneralizedEM((targets: Seq[Element[_]]) => (universe: Universe) => makeMH(mhParticles, proposalScheme, targets)(universe), universe, p: _*)(emIterations)
}
