/*
 * ExpectationMaximization.scala
 * Expectation maximization algorithm.
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

import com.cra.figaro.algorithm._
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.algorithm.factored._
import com.cra.figaro.algorithm.lazyfactored._
import com.cra.figaro.language._
import com.cra.figaro.library.atomic.continuous._
import com.cra.figaro.util._
import scala.collection._

/*
 *  When the algorithm is created, a mapping is constructed from all target parameters to their sufficient statistics vector.
 * This mapping is used in the inference algorithm to correctly define factors for the parameters.
 */
/**
 * Expectation maximization iteratively produces an estimate of sufficient statistics for learnable parameters,
 * then maximizes the parameters according to the estimate. It uses an factored inference algorithm, SufficientStatisticsVariableElimination,
 * to produce the estimate of the sufficient statistics. This class can be extended with a different expectation
 * or maximization algorithm; see the code for details.
 */
class ExpectationMaximization(universe: Universe, targetParameters: Parameter[_]*)(val numberOfIterations: Int) extends Algorithm with ParameterLearner {

  //Defines the length of the sequences corresponding to different parameters
  protected val paramMap : immutable.Map[Parameter[_],Seq[Double]]= immutable.Map(targetParameters.map(p => p -> p.zeroSufficientStatistics):_*)
  

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

    LazyValues.clear(universe)
    Variable.clearCache()
    ProbFactor.removeFactors()
    val algorithm = SufficientStatisticsVariableElimination(paramMap)(universe)
    algorithm.start
    val result = algorithm.getSufficientStatisticsForAllParameters
    algorithm.kill

    result
  }

  protected def doMaximizationStep(parameterMapping: Map[Parameter[_], Seq[Double]]): Unit = {

    for (p <- targetParameters) {
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

object ExpectationMaximization {

  /**
   * An expectation maximization algorithm which will run for the default of 10 iterations
   */
  def apply(p: Parameter[_]*)(implicit universe: Universe) =
    new ExpectationMaximization(universe, p: _*)(10)

  /**
   * An expectation maximization algorithm which will run for the number of iterations specified
   */
  def apply(iterations: Int, p: Parameter[_]*)(implicit universe: Universe) =
    new ExpectationMaximization(universe, p: _*)(iterations)

}

