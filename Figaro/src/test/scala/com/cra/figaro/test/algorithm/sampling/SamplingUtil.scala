package com.cra.figaro.test.algorithm.sampling

object SamplingUtil {

  /*
   * Compute the number of samples such that P(E[X] \in [p-epsilon, p+epsilon]) >= 1-lambda
   * Uses Chernoff bounds to compute
   */
  def computeNumberOfSamples(epsilon: Double, lambda: Double): Int = {
    if (lambda <= 0.0 || epsilon <= 0.0) throw new IllegalArgumentException
    
    val n = (2+epsilon)/(epsilon*epsilon) * math.log(2/lambda)
    if (n > 1000000) println("WARNING: LARGE NUMBER OF SAMPLES COMPUTED")
    n.toInt
  }
  
  
}