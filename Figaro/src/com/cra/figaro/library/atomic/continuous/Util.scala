/*
 * Util.scala
 * Utility functions for atomic continuous elements.
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Feb 25, 2011
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 */

package com.cra.figaro.library.atomic.continuous

import com.cra.figaro.util.random
import annotation.tailrec
import scala.math._

object Util {
  /**
   * Generate an exponentially distributed random variable.
   */
  def generateExponential(lambda: Double): Double = -log(random.nextDouble()) / lambda

  /**
   * Generate a Gamma distributed random variable.
   * Best's rejection algorithm XGB from Luc Devroye, Non-Uniform Random Variate Generation, p. 410.
   */
  def generateGamma(k: Double) = {
    
    val b = k - 1
    val c = 3 * k - 0.75

    @tailrec
    def generateGreaterThanOne(): Double = {
      val u = random.nextDouble()
      val v = random.nextDouble()
      val w = u * (1 - u)
      val y = sqrt(c / w) * (u - 0.5)
      val x = b + y
      val accept =
        if (x >= 0) {
          val z = 64 * w * w * w * v * v
          (z <= 1 - 2 * y * y / x) || (log(z) <= 2 * (b * log(x / b) - y))
        } else false
      if (accept) x; else generateGreaterThanOne()
    }

    // See Wikipedia, Gamma distribution
    @tailrec
    def generateLessThanOne(): Double = {
      val v0 = random.nextDouble()
      val v1 = random.nextDouble()
      val v2 = random.nextDouble()
      val (epsilon, eta) =
        if (v2 <= E / (E + k)) {
          val epsilon = pow(v1, 1 / k)
          val eta = v0 * pow(epsilon, k - 1)
          (epsilon, eta)
        } else {
          val epsilon = 1 - log(v1)
          val eta = v0 * exp(-epsilon)
          (epsilon, eta)
        }
      if (eta <= pow(epsilon, k - 1) * exp(-epsilon)) epsilon
      else generateLessThanOne()
    }

    if (k > 1.0) generateGreaterThanOne()
    else if (k < 1.0) generateLessThanOne()
    else generateExponential(1.0)
  }

  /**
   * Generate a Beta distributed random variable.
   * See Devroye, Non Uniform Random Variate Generation, p. 432
   */
  def generateBeta(a: Double, b: Double) = {
    val ga = generateGamma(a)
    val gb = generateGamma(b)
    ga / (ga + gb)
  }
}