/*
 * Binomial.scala
 * Elements representing binomial distributions.
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

import com.cra.figaro.algorithm.ValuesMaker
import com.cra.figaro.algorithm.lazyfactored.ValueSet
import com.cra.figaro.algorithm.factored._
import com.cra.figaro.language._
import com.cra.figaro.library.atomic.continuous._
import annotation.tailrec
import scala.math.{ floor, pow }
import JSci.maths.ExtraMath.binomial

/**
 * A binomial distribution in which the parameters are constants.
 */
class AtomicBinomial(name: Name[Int], n: Int, p: Double, collection: ElementCollection)
  extends Element[Int](name, collection) with Atomic[Int] with ValuesMaker[Int] with ProbFactorMaker with Cacheable[Int]
  with OneShifter {
  protected lazy val lowerBound = 0
  protected lazy val upperBound = n

  private lazy val q = 1 - p

  // Devroye, p. 525
  @tailrec
  private def generateHelper(x: Int, sum: Int): Int = {
    val g = Util.generateGeometric(1 - p)
    val newSum = sum + g
    val newX = x + 1
    if (newSum <= n) generateHelper(newX, newSum)
    else newX
  }

  def generateRandomness() = if (p <= 0) 0; else if (p < 1) generateHelper(-1, 0); else n

  /**
   * The Metropolis-Hastings proposal is to increase or decrease the value of by 1.
   */
  override def nextRandomness(rand: Randomness) = shiftOne(rand)

  def generateValue(rand: Int) = rand

  /**
   * Probability of a value.
   */
  def density(k: Int) = binomial(n, k) * pow(p, k) * pow(q, n - k)

  /**
   * Return the range of values of the element.
   */
  def makeValues(depth: Int) = ValueSet.withoutStar((for { i <- 0 to n } yield i).toSet)

  /**
   * Convert an element into a list of factors.
   */
  def makeFactors = {
    val binVar = Variable(this)
    val factor = new Factor[Double](List(binVar))
    for { (xvalue, index) <- binVar.range.zipWithIndex } {
      factor.set(List(index), density(xvalue.value))
    }
    List(factor)
  }

  override def toString = "Binomial(" + n + ", " + p + ")"
}

/**
 * A binomial distribution in which the number of trials is fixed and the success probability is an element.
 */
class BinomialFixedNumTrials(name: Name[Int], n: Int, p: Element[Double], collection: ElementCollection)
  extends NonCachingChain[Double, Int](name, p, (p: Double) => new AtomicBinomial("", n, p, collection), collection)

/**
 * A binomial distribution in which the parameters are elements.
 */
class CompoundBinomial(name: Name[Int], n: Element[Int], p: Element[Double], collection: ElementCollection)
  extends CachingChain[Int, Int](
    name,
    n,
    (n: Int) => new NonCachingChain(
      "",
      p,
      (p: Double) => new AtomicBinomial("", n, p, collection),
      collection),
    collection) {
  override def toString = "Binomial(" + n + ", " + p + ")"
}

object Binomial extends Creatable {
  /**
   * Create a binomial distribution in which the parameters are constants.
   */
  def apply(n: Int, p: Double)(implicit name: Name[Int], collection: ElementCollection) =
    new AtomicBinomial(name, n, p, collection)

  /**
   * Create a binomial distribution in which the number of trials is fixed and the success probability is an element.
   */
  def apply(n: Int, p: Element[Double])(implicit name: Name[Int], collection: ElementCollection) =
    new BinomialFixedNumTrials(name, n, p, collection)
  /**
   * Create a binomial distribution in which the parameters are elements.
   */
  def apply(n: Element[Int], p: Element[Double])(implicit name: Name[Int], collection: ElementCollection) =
    new CompoundBinomial(name, n, p, collection)

  type ResultType = Int

  def create(args: List[Element[_]]) = apply(args(0).asInstanceOf[Element[Int]], args(1).asInstanceOf[Element[Double]])
}
