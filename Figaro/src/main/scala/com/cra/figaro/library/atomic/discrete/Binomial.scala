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

/**
 * A binomial distribution in which the parameters are constants.
 */
class AtomicBinomial(name: Name[Int], val numTrials: Int, val probSuccess: Double, collection: ElementCollection)
  extends Element[Int](name, collection) with Atomic[Int] with ValuesMaker[Int] with ProbFactorMaker with Cacheable[Int]
  with OneShifter {
  protected lazy val lowerBound = 0
  protected lazy val upperBound = numTrials

  private lazy val q = 1 - probSuccess

  // Devroye, p. 525
  @tailrec
  private def generateHelper(x: Int, sum: Int): Int = {
    val g = Util.generateGeometric(1 - probSuccess)
    val newSum = sum + g
    val newX = x + 1
    if (newSum <= numTrials) generateHelper(newX, newSum)
    else newX
  }

  def generateRandomness() = if (probSuccess <= 0) 0; else if (probSuccess < 1) generateHelper(-1, 0); else numTrials

  /**
   * The Metropolis-Hastings proposal is to increase or decrease the value of by 1.
   */
  override def nextRandomness(rand: Randomness) = shiftOne(rand)

  def generateValue(rand: Int) = rand

  /**
   * Probability of a value.
   */
  def density(k: Int) = {
    Util.binomialDensity(numTrials, probSuccess, k)
  }

  /**
   * Return the range of values of the element.
   */
  def makeValues(depth: Int) = ValueSet.withoutStar((for { i <- 0 to numTrials } yield i).toSet)

  /**
   * Convert an element into a list of factors.
   */
  def makeFactors = {
    val binVar = Variable(this)
    val factor = Factory.make[Double](List(binVar))
    for { (xvalue, index) <- binVar.range.zipWithIndex } {
      factor.set(List(index), density(xvalue.value))
    }
    List(factor)
  }

  override def toString = "Binomial(" + numTrials + ", " + probSuccess + ")"
}

/**
 * A binomial distribution in which the number of trials is fixed and the success probability is an element.
 */
class BinomialFixedNumTrials(name: Name[Int], val numTrials: Int, val probSuccess: Element[Double], collection: ElementCollection)
  extends NonCachingChain[Double, Int](name, probSuccess, (p: Double) => new AtomicBinomial("", numTrials, p, collection), collection) {
  override def toString = "Binomial(" + numTrials + ", " + probSuccess + ")"
}

 /**
 * A binomial with a fixed number of trials parameterized by a beta distribution.
 */
class ParameterizedBinomialFixedNumTrials(name: Name[Int], val numTrials: Int, val probSuccess: AtomicBeta, collection: ElementCollection)
  extends CachingChain[Double, Int](name, probSuccess, (p: Double) => new AtomicBinomial("", numTrials, p, collection), collection)
  with Parameterized[Int] {
  val parameter = probSuccess
  
  def distributionToStatistics(distribution: Stream[(Double, Int)]): Seq[Double] = {
    val distList = distribution.toList
    var totalPos = 0.0
    var totalNeg = 0.0
    for { i <- 0 to numTrials } {
      distList.find(_._2 == i) match {
        case Some((prob, _)) =>
          totalPos += prob * i
          totalNeg += prob * (numTrials - i)
        case None => ()
      }
    }
    List(totalPos, totalNeg)
  }
  
  def density(value: Int): Double = {
    val probSuccess = parameter.value
    if (value < 0 || value > numTrials) 0.0
    else Util.binomialDensity(numTrials, probSuccess, value)
  }

 override def toString = "ParameterizedBinomial(" + numTrials + ", " + probSuccess + ")"
} 

/**
 * A binomial distribution in which the parameters are elements.
 */
class CompoundBinomial(name: Name[Int], val numTrials: Element[Int], val probSuccess: Element[Double], collection: ElementCollection)
  extends CachingChain[Int, Int](
    name,
    numTrials,
    (n: Int) => new NonCachingChain(
      "",
      probSuccess,
      (p: Double) => new AtomicBinomial("", n, p, collection),
      collection),
    collection) {
  override def toString = "Binomial(" + numTrials + ", " + probSuccess + ")"
}

object Binomial extends Creatable {
  /**
   * Create a binomial distribution in which the parameters are constants.
   */
  def apply(n: Int, p: Double)(implicit name: Name[Int], collection: ElementCollection) =
    new AtomicBinomial(name, n, p, collection)

  /**
   * Create a binomial distribution in which the number of trials is fixed and the success probability is an element.
   *
   * If the element is an atomic beta element, the flip uses that element
   * as a learnable parameter.
   */
  def apply(n: Int, p: Element[Double])(implicit name: Name[Int], collection: ElementCollection) = {
    if (p.isInstanceOf[AtomicBeta])
    new ParameterizedBinomialFixedNumTrials(name, n, p.asInstanceOf[AtomicBeta], collection)
    else new BinomialFixedNumTrials(name, n, p, collection)
  }
  
  /**
   * Create a binomial distribution in which the parameters are elements.
   */
  def apply(n: Element[Int], p: Element[Double])(implicit name: Name[Int], collection: ElementCollection) =
    new CompoundBinomial(name, n, p, collection)

  type ResultType = Int

  def create(args: List[Element[_]]) = apply(args(0).asInstanceOf[Element[Int]], args(1).asInstanceOf[Element[Double]])
}
