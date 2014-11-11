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
import com.cra.figaro.algorithm.factored.Factory
import com.cra.figaro.patterns.learning.ModelParameters
import com.cra.figaro.algorithm.factored.SufficientStatisticsVariableElimination

/*
 * @param inferenceAlgorithmConstructor
 */

abstract class ExpectationMaximization(universe: Universe, targetParameters: Parameter[_]*) extends Algorithm with ParameterLearner {
  
  protected def doStart(): Unit = {
    em()
  }
  
  protected def doExpectationStep(): Map[Parameter[_], Seq[Double]]
  
  var sufficientStatistics: Map[Parameter[_], Seq[Double]] = Map.empty[Parameter[_],Seq[Double]]
  val debug = false
  protected def em(): Unit = {
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
    
  def iteration() : Unit = {
      sufficientStatistics = doExpectationStep()
      doMaximizationStep(sufficientStatistics)
      if (debug) println("Completed iteration")
  }

  private[learning] val shouldTerminate: EMTerminationCriteria

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
class ExpectationMaximizationWithFactors(universe: Universe, targetParameters: Parameter[_]*)(val numberOfIterations: Int) extends ExpectationMaximization(universe,targetParameters:_*) {
  val shouldTerminate = new MaxIterations(numberOfIterations,this)
  //Defines the length of the sequences corresponding to different parameters
  protected val paramMap : Map[Parameter[_],Seq[Double]]= Map(targetParameters.map(p => p -> p.zeroSufficientStatistics):_*)

  protected def doExpectationStep(): Map[Parameter[_], Seq[Double]] = {
    val algorithm = SufficientStatisticsVariableElimination(paramMap)(universe)
    algorithm.start
    val result = algorithm.getSufficientStatisticsForAllParameters
    algorithm.kill
    result
  }

}

class GeneralizedEM(inferenceAlgorithmConstructor: Seq[Element[_]] => Universe => ProbQueryAlgorithm with OneTime, universe: Universe, targetParameters: Parameter[_]*)(val numberOfIterations: Int) extends ExpectationMaximization(universe,targetParameters:_*){
  /*
   * Start the algorithm. After it returns, the algorithm must be ready to provide answers.
   */
  val shouldTerminate = new MaxIterations(numberOfIterations,this)

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

object EM {

  //If we are keeping sufficient statistics factors, why doesn't this implementation use them?
  def withBP(p: ModelParameters)(implicit universe: Universe) = EMWithBP(p)(universe)
  def withBP(emIterations: Int, bpIterations: Int, p: ModelParameters)(implicit universe: Universe) = EMWithBP(emIterations, bpIterations, p: ModelParameters)(universe)
  def withBP(p: Parameter[_]*)(implicit universe: Universe) = EMWithBP(p: _*)(universe)
  def witHBP(emIterations: Int, bpIterations: Int, p: Parameter[_]*)(implicit universe: Universe) = EMWithBP(emIterations, bpIterations, p: _*)(universe)

  def withVE(p: ModelParameters)(implicit universe: Universe) = EMWithVE(p)(universe)
  def withVE(emIterations: Int, p: ModelParameters)(implicit universe: Universe) = EMWithVE(emIterations, p: ModelParameters)(universe)
  def withVE(p: Parameter[_]*)(implicit universe: Universe) = EMWithVE(p: _*)(universe)
  def withVE(emIterations: Int, p: Parameter[_]*)(implicit universe: Universe) = EMWithVE(emIterations, p: _*)(universe)

  def withImportance(emIterations: Int, importanceParticles: Int, p: Parameter[_]*)(implicit universe: Universe) = EMWithImportance(emIterations, importanceParticles, p: _*)(universe)
  def withImportance(p: ModelParameters)(implicit universe: Universe) = EMWithImportance(p)(universe)
  def withImportance(emIterations: Int, importanceParticles: Int, p: ModelParameters)(implicit universe: Universe) = EMWithImportance(emIterations, importanceParticles, p)(universe)

  def withMH(emIterations: Int, mhParticles: Int, p: Parameter[_]*)(implicit universe: Universe) = EMWithMH(emIterations, mhParticles, p: _*)(universe)
  def withMH(p: ModelParameters)(implicit universe: Universe) = EMWithMH(p)(universe)
  def withMH(emIterations: Int, mhParticles: Int, proposalScheme: ProposalScheme, p: ModelParameters)(implicit universe: Universe) = EMWithMH(emIterations, mhParticles, proposalScheme, p)(universe)
  def withMH(emIterations: Int, mhParticles: Int, proposalScheme: ProposalScheme, p: Parameter[_]*)(implicit universe: Universe) = EMWithMH(emIterations, mhParticles, proposalScheme, p: _*)(universe)

  
}

object EMWithBP {
  private def makeBP(numIterations: Int, targets: Seq[Element[_]])(universe: Universe) = {
    Factory.removeFactors()
    BeliefPropagation(numIterations, targets: _*)(universe)
  }

  def apply(p: ModelParameters)(implicit universe: Universe) = {
    val parameters = p.convertToParameterList
    new GeneralizedEM((targets: Seq[Element[_]]) => (universe: Universe) => makeBP(10, targets)(universe), universe, parameters: _*)(10)
  }

  def apply(emIterations: Int, bpIterations: Int, p: ModelParameters)(implicit universe: Universe) = {
    val parameters = p.convertToParameterList
    new GeneralizedEM((targets: Seq[Element[_]]) => (universe: Universe) => makeBP(bpIterations, targets)(universe), universe, parameters: _*)(emIterations)
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
    Importance(numParticles, targets: _*)(universe)
  }

  /**
   * An expectation maximization algorithm using importance sampling for inference
   *
   * @param emIterations number of iterations of the EM algorithm
   * @param importanceParticles number of particles of the importance sampling algorithm
   */
  def apply(emIterations: Int, importanceParticles: Int, p: Parameter[_]*)(implicit universe: Universe) =
    new GeneralizedEM((targets: Seq[Element[_]]) => (universe: Universe) => makeImportance(importanceParticles, targets)(universe), universe, p: _*)(emIterations)

  def apply(p: ModelParameters)(implicit universe: Universe) = {
    val parameters = p.convertToParameterList
    new GeneralizedEM((targets: Seq[Element[_]]) => (universe: Universe) => makeImportance(100000, targets)(universe), universe, parameters: _*)(10)
  }

  def apply(emIterations: Int, importanceParticles: Int, p: ModelParameters)(implicit universe: Universe) = {
    val parameters = p.convertToParameterList
    new GeneralizedEM((targets: Seq[Element[_]]) => (universe: Universe) => makeImportance(100000, targets)(universe), universe, parameters: _*)(emIterations)
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
    new GeneralizedEM((targets: Seq[Element[_]]) => (universe: Universe) => makeMH(mhParticles, ProposalScheme.default(universe), targets)(universe), universe, p: _*)(emIterations)

  /**
   * An expectation maximization algorithm using Metropolis Hastings for inference.
   *
   * @param emIterations number of iterations of the EM algorithm
   * @param mhParticles number of particles of the MH algorithm
   */
  def apply(emIterations: Int, mhParticles: Int, proposalScheme: ProposalScheme, p: Parameter[_]*)(implicit universe: Universe) =
    new GeneralizedEM((targets: Seq[Element[_]]) => (universe: Universe) => makeMH(mhParticles, proposalScheme, targets)(universe), universe, p: _*)(emIterations)

  def apply(p: ModelParameters)(implicit universe: Universe) = {
    val parameters = p.convertToParameterList
    new GeneralizedEM((targets: Seq[Element[_]]) => (universe: Universe) => makeMH(100000, ProposalScheme.default(universe), targets)(universe), universe, parameters: _*)(10)
  }

  def apply(emIterations: Int, mhParticles: Int, proposalScheme: ProposalScheme, p: ModelParameters)(implicit universe: Universe) = {
    val parameters = p.convertToParameterList
    new GeneralizedEM((targets: Seq[Element[_]]) => (universe: Universe) => makeMH(mhParticles, proposalScheme, targets)(universe), universe, parameters: _*)(emIterations)
  }

}

object EMWithVE {
  /**
   * An expectation maximization algorithm which will run for the default of 10 iterations
   */
   def apply(p: Parameter[_]*)(implicit universe: Universe) =
     new ExpectationMaximizationWithFactors(universe, p: _*)(10)
     
   def apply(p: ModelParameters)(implicit universe: Universe) =
     new ExpectationMaximizationWithFactors(universe, p.convertToParameterList:_*)(10)

   def apply(iterations: Int, p: ModelParameters)(implicit universe: Universe) =
     new ExpectationMaximizationWithFactors(universe, p.convertToParameterList:_*)(iterations)
   /**
    * An expectation maximization algorithm which will run for the number of iterations specified
    */
   def apply(iterations: Int, p: Parameter[_]*)(implicit universe: Universe) =
     new ExpectationMaximizationWithFactors(universe, p: _*)(iterations)
 }
