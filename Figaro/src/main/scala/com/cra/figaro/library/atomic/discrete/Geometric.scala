/*
 * Geometric.scala
 * Elements representing geometric distributions.
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Feb 2, 2011
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.library.atomic.discrete

import com.cra.figaro.language._
import com.cra.figaro.util._
import scala.math.{ log, ceil, pow }

/**
 * A geometric distribution representing the number of trials until the first success. The parameter,
 * which is a constant, represents the probability of failure of a trial.
 */
class AtomicGeometric(name: Name[Int], probFail: Double, collection: ElementCollection)
  extends Element[Int](name, collection) with Atomic[Int] with OneShifter with Cacheable[Int] {
  protected lazy val lowerBound = 1
  protected lazy val upperBound = Int.MaxValue

  private lazy val probSuccess = 1 - probFail
  private lazy val logProbFailInverse = 1 / log(probFail)

  // see Devroye, Non-Uniform Random Variate Generation, p. 500
  def generateRandomness() = ceil(log(random.nextDouble()) * logProbFailInverse).toInt

  def generateValue(rand: Randomness) = rand

  /**
   * The Metropolis-Hastings proposal is to increase or decrease the value of by 1.
   */
  override def nextRandomness(rand: Randomness): (Randomness, Double, Double) = shiftOne(rand)

  /**
   * Probability of a value.
   */
  def density(x: Int) = if (x < lowerBound) 0.0 else pow(probFail, x - 1) * probSuccess

  override def toString = "Geometric(" + probFail + ")"
}

/**
 * A geometric distribution representing the number of trials until the first success. The parameter,
 * which is an element, represents the probability of failure of a trial.
 */
class CompoundGeometric(name: Name[Int], probFail: Element[Double], collection: ElementCollection)
  extends NonCachingChain(
    name,
    probFail,
    (p: Double) => new AtomicGeometric("", p, collection),
    collection) {
  override def toString = "Geometric(" + probFail + ")"
}

object Geometric extends Creatable {
  /**
   * Create a geometric distribution representing the number of trials until the first success. The parameter,
   * which is a constant, represents the probability of failure of a trial.
   */
  def apply(probFail: Double)(implicit name: Name[Int], collection: ElementCollection) =
    new AtomicGeometric(name, probFail, collection)

  /**
   * Create a geometric distribution representing the number of trials until the first success. The parameter,
   * which is an element, represents the probability of failure of a trial.
   */
  def apply(probFail: Element[Double])(implicit name: Name[Int], collection: ElementCollection) =
    new CompoundGeometric(name, probFail, collection)

  type ResultType = Int

  def create(args: List[Element[_]]) = apply(args(0).asInstanceOf[Element[Double]])
}
