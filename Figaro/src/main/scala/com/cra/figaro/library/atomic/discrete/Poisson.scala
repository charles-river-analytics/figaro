/*
 * Poisson.scala
 * Elements representing Poisson distributions.
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Feb 25, 2011
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.library.atomic.discrete

import com.cra.figaro.language._
import com.cra.figaro.util._
import annotation.tailrec
import scala.math.{ exp, pow }
import JSci.maths.ExtraMath.factorial

/**
 * A Poisson distribution in which the parameter is constant.
 */
class AtomicPoisson(name: Name[Int], lambda: Double, collection: ElementCollection)
  extends Element[Int](name, collection) with Atomic[Int] with OneShifter with Cacheable[Int] {
  protected lazy val lowerBound = 0
  protected lazy val upperBound = Int.MaxValue

  private lazy val expMinusLambda = exp(-lambda)

  // Devroye, Non Uniform Random Variate Generation, p. 505
  def generateRandomness() = {
    @tailrec
    def generateHelper(x: Int, prod: Double): Int = {
      val newProd = prod * random.nextDouble()
      if (newProd > expMinusLambda) generateHelper(x + 1, newProd)
      else x
    }

    generateHelper(0, 1)
  }

  /**
   * The Metropolis-Hastings proposal is to increase or decrease the value of by 1.
   */
  override def nextRandomness(rand: Randomness) = shiftOne(rand)

  def generateValue(rand: Int) = rand

  /**
   * Probability of a value.
   */
  def density(k: Int) = {
    if (k < lowerBound) 0.0 else {
      if (lambda > 10 || k > 10) { //Use approximation
        val logLambdaToK = k * Math.log(lambda)
        val logKFact = JSci.maths.ExtraMath.logFactorial(k)
        exp((logLambdaToK - logKFact) - lambda)
      } else { //Use exact
        pow(lambda, k) / factorial(k) * expMinusLambda
      }
    }
  }
  override def toString = "Poisson(" + lambda + ")"
}

/**
 * A Possion distribution in which the parameter is an element.
 */
class CompoundAtomic(name: Name[Int], lambda: Element[Double], collection: ElementCollection)
  extends NonCachingChain(
    name,
    lambda,
    (l: Double) => new AtomicPoisson("", l, collection),
    collection) {
  override def toString = "Poisson(" + lambda + ")"
}

object Poisson extends Creatable {
  /**
   * Create a Poisson distribution in which the parameter is a constant.
   */
  def apply(lambda: Double)(implicit name: Name[Int], collection: ElementCollection) =
    new AtomicPoisson(name, lambda, collection)

  /**
   * Create a Poisson distribution in which the parameter is an element.
   */
  def apply(lambda: Element[Double])(implicit name: Name[Int], collection: ElementCollection) =
    new CompoundAtomic(name, lambda, collection)

  type ResultType = Int

  def create(args: List[Element[_]]) = apply(args(0).asInstanceOf[Element[Double]])
}
