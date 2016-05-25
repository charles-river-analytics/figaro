/*
 * GaussianProcessTraining.scala
 * Demonstrates use of externally trained models in Figaro.
 * 
 * Created By:      Dan Garant (dgarant@cra.com)
 * Creation Date:   May 20, 2016
 * 
 * Copyright 2016 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.example

import org.apache.commons.math3.linear._
import com.cra.figaro.language._
import com.cra.figaro.util.random
import com.cra.figaro.library.atomic._
import com.cra.figaro.library.atomic.continuous.Normal
import com.cra.figaro.algorithm.sampling.Importance

/**
 * Trains a GaussianProcess model and uses it as part of a chain
 */
object GaussianProcessTraining {

  def main(args:Array[String]) = {
    
    // set up the model
    // y = x^2 + eps, eps ~ N(0, 1)
    val x = Range.Double(1, 10, 1)
    val y = x.map(xi => math.pow(xi, 2) + random.nextGaussian())
  
    // wire together dependence structure
    val gp = new GaussianProcess(new GaussianCovarianceFunction(1 / 2.0))
    gp.train(x zip y toList)
    val xElement = continuous.Uniform(0, 11)
    val yElement = Chain(xElement, gp.model)
  
    // estimate conditional expectation
    xElement.observe(7.5)
    var importance = Importance(1000, yElement)
    importance.start()
    val expectedYVal = importance.computeExpectation(yElement, (v: Double) => v)
    importance.kill()
    println("E[Y|X=7.5] = " + expectedYVal)
    
    // now adding an effect of y
    val zElement = Chain(yElement, (v:Double) => Normal(v + 3, 1))
    
    importance = Importance(1000, zElement)
    importance.start()
    val expectedZVal = importance.computeExpectation(zElement, (v: Double) => v)
    importance.kill()
    println("E[Z|X=7.5] = " + expectedZVal)    
  }
}

/**
 * General form of a covariance function,
 * taking two items of type T and producing a measure
 */
trait CovarianceFunction[T] {
  def apply(v1: T, v2: T): Double
}

/** The Gaussian, or radial basis function kernel / covariance function between univariate observations */
class GaussianCovarianceFunction(var gamma: Double) extends CovarianceFunction[Double] {

  /** Computes covariance using the L2 norm */
  override def apply(v1: Double, v2: Double): Double = {
    Math.exp(-gamma * Math.pow(v1 - v2, 2))
  }

  override def toString = "GaussianCovarianceFunction(gamma=" + gamma + ")"
}

/**
 * Estimates and performs posterior prediction from a Gaussian process.
 * @param covarianceFunction Defines the inner product between feature vectors
 * @param noiseVariance Prior variance of noise at design points
 */
class GaussianProcess[Input](var covarianceFunction: CovarianceFunction[Input], noiseVariance: Double = 0.001) {

  var priorMean: Double = 0
  var covarianceInverse: RealMatrix = null
  var inputs: Seq[Input] = null
  var alpha: RealMatrix = null
  var responses: RealVector = null
  var numDimensions: Integer = null

  /**
   * Estimate the conditional density of a new point.
   * @param newInputs The point to evaluate at
   * @returns Posterior normal distribution
   */
  def model(newInput: Input): Element[Double] = {
    if (covarianceInverse == null) {
      throw new IllegalArgumentException("The Gaussian process must be fit before 'model' can be called")
    }

    val newCovariance = new ArrayRealVector((0 until inputs.length).map(i => covarianceFunction(inputs(i), newInput)).toArray)
    var variance = 1 - covarianceInverse.preMultiply(newCovariance).dotProduct(newCovariance)
    val mean = alpha.preMultiply(newCovariance).getEntry(0)

    Normal(priorMean + mean, Math.sqrt(variance))
  }

  /**
   * Estimates the parameters of the Gaussian process (the inverse of the covariance matrix) from data
   * @param data A sequence of input, output pairs to use when fitting the model
   */
  def train(data: List[(Input, Double)]) = {
    inputs = data map { _._1 }
    priorMean = (data map { _._2 } sum) / data.length
    responses = new ArrayRealVector(data map { _._2 - priorMean } toArray)

    // construct covariance matrix
    val rows = (0 until data.length).map(i => {
      (0 until data.length).map(j => {
        covarianceFunction(data(i)._1, data(j)._1)
      }).toArray
    }).toArray

    val covariance = new Array2DRowRealMatrix(rows, false)
    covarianceInverse = MatrixUtils.inverse(covariance.add(MatrixUtils.createRealIdentityMatrix(data.length).scalarMultiply(noiseVariance)))

    alpha = covarianceInverse.multiply(new Array2DRowRealMatrix(responses toArray))
  }
}
