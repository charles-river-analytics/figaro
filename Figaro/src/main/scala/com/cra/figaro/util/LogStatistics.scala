/*
 * LogStatistics.scala
 * Utilities for statistics in log space.
 *
 * Created By:      William Kretschmer (kretsch@mit.edu)
 * Creation Date:   Aug 16, 2016
 *
 * Copyright 2016 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.util

import org.apache.commons.math3.distribution.TDistribution

import scala.math._

/**
 * Represents basic statistics of a sample from a nonnegative-valued univariate distribution. All computations on these
 * statistics are done in log space to prevent underflow.
 * @param logMean The log of the mean of the samples.
 * @param logVariance The log of the variance of the samples.
 * @param count The number of samples taken.
 */
case class LogStatistics(logMean: Double, logVariance: Double, count: Int) {
  /**
   * Returns the statistics corresponding to the distribution wherein each sample is multiplied by the given constant.
   * @param logConstant The log of the constant to multiply by.
   * @return Statistics such that the mean and variance are updated, and the count is unchanged.
   */
  def multiplyByConstant(logConstant: Double) = {
    LogStatistics(logMean + logConstant, logVariance + logConstant * 2, count)
  }
}

object LogStatistics {
  /**
   * Performs a one-sided t-test for the comparison of v1.logMean and v2.logMean. This compares the smaller mean to the
   * larger mean, i.e. it computes a p-value for min(v1.logMean, v2.logMean) < max(v1.logMean, v2.logMean).
   * @param v1 Mean, variance, and sample count from first distribution. Requires v1.count > 1.
   * @param v2 Mean, variance, and sample count from second distribution. Requires v2.count > 1.
   * @return A p-value for the hypothesis. A small p-value indicates high confidence that the population mean of the
   * sample with lesser mean is less than the population mean of the other sample.
   */
  def oneSidedTTest(v1: LogStatistics, v2: LogStatistics): Double = {
    require(v1.count > 1 && v2.count > 1, "t-test requires counts > 1")
    // If both variances are 0, we return here to avoid NaNs.
    if(v1.logVariance == Double.NegativeInfinity && v2.logVariance == Double.NegativeInfinity) {
      if(v1.logMean == v2.logMean) 0.5
      else 0.0
    }
    // Otherwise, at least one of the variances is positive, so everything below is well-defined.
    else {
      // log(v1.variance / v1.count), respectively v2. These quantities are used multiple times in computing the degrees
      // of freedom and the t-score, so it's helpful not having to recompute them.
      val logVar1OverCount1 = v1.logVariance - log(v1.count)
      val logVar2OverCount2 = v2.logVariance - log(v2.count)

      // Compute log numerator and denominator of the degrees of freedom using the Welch-Satterthwaite equation.
      val logDoFNum = 2 * logSum(logVar1OverCount1, logVar2OverCount2)
      val logDoFDenom = logSum(2 * logVar1OverCount1 - log(v1.count - 1), 2 * logVar2OverCount2 - log(v2.count - 1))
      val degreesOfFreedom = exp(logDoFNum - logDoFDenom)

      // To get a negative t-score, take the positive difference in log space, then invert the sign after exponentiation.
      val logNegativeTScore = logDiff(v1.logMean max v2.logMean, v1.logMean min v2.logMean) -
        0.5 * logSum(logVar1OverCount1, logVar2OverCount2)
      val tScore = -exp(logNegativeTScore)
      new TDistribution(degreesOfFreedom).cumulativeProbability(tScore)
    }
  }
}

/**
 * Trait for computing mean in variance in log space in an online fashion.
 */
trait OnlineLogStatistics {

  // Fields for recording the mean and variance across runs. See Wikipedia, "Algorithms for calculating variance".
  // Modified to work in log space to prevent underflow.
  // Number of samples taken
  protected var count = 0
  // Log of mean of samples taken so far
  protected var logMean = Double.NegativeInfinity
  // Log of variance of samples taken so far, multiplied by (count - 1)
  protected var logM2 = Double.NegativeInfinity

  /**
   * Record the weight in the rolling mean and variance computation.
   * @param logWeight Log of the weight to record.
   */
  def record(logWeight: Double) = {
    // Online variance algorithm from Wikipedia
    count += 1
    if(logWeight >= logMean) {
      val logDelta = logDiff(logWeight, logMean)
      logMean = logSum(logMean, logDelta - math.log(count))
      // assert(logMean >= logWeight)
      logM2 = logSum(logM2, logDelta + logDiff(logWeight, logMean))
    }
    else {
      val logNegativeDelta = logDiff(logMean, logWeight)
      logMean = logDiff(logMean, logNegativeDelta - math.log(count))
      // assert(logMean < logWeight)
      // The negatives cancel in the right hand size
      logM2 = logSum(logM2, logNegativeDelta + logDiff(logMean, logWeight))
    }
  }

  /**
   * Return the combined statistics for the log probability of evidence over all runs of this sampler.
   * If the number of observations is 0, the returned log mean is -Infinity.
   * If the number of observations is 0 or 1, the returned log variance is NaN.
   */
  def totalLogStatistics = LogStatistics(logMean, logM2 - math.log(count - 1), count)
}
