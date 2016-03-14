/*
 * JointDistribution.scala 
 * Book example unit test.
 * 
 * Created By:      Michael Reposa (mreposa@cra.com), Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Feb 26, 2016
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.test.book.chap12

import com.cra.figaro.language.{Element, Flip, Universe}
import com.cra.figaro.library.atomic.discrete.Uniform
import com.cra.figaro.library.compound.{If, ^^}
import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.library.collection.Container
import com.cra.figaro.algorithm.sampling.Importance
import org.scalatest.Matchers
import org.scalatest.WordSpec
import com.cra.figaro.test.tags.BookExample
import com.cra.figaro.test.tags.NonDeterministic

object JointDistribution {
  val economicClimateGood = Flip(0.5)
  def makeSales: Element[Int] = If(economicClimateGood, Uniform(80, 120, 160), Uniform(60, 90, 120))
  val sales = Array.fill(20)(makeSales)
  val totalSales = Container(sales:_*).reduce(_ + _)

  def main(args: Array[String]) {
    val salesPair = ^^(sales(0), sales(1))
    val ve = VariableElimination(sales(0), sales(1), salesPair)
    ve.start()
    val imp = Importance(10000, totalSales)
    imp.start()
    println("Probability first sales will be less than 100 = " +
      ve.probability(sales(0), (i: Int) => i < 100))
    println("Probability second sales will be less than 100 = " +
      ve.probability(sales(1), (i: Int) => i < 100))
    println("Probability both sales will be less than 100 = " +
      ve.probability(salesPair, (pair: (Int, Int)) => pair._1 < 100 && pair._2 < 100))
    println("Probability total sales are less than 2000 = " +
      imp.probability(totalSales, (i: Int) => i < 2000))
    println("Mean individual sales = " +
      ve.expectation(sales(0), (i: Int) => i.toDouble))
    println("Mean total sales = " +
      imp.expectation(totalSales, (i: Int) => i.toDouble))
    ve.kill()
    imp.kill()
  }
}

class JointDistributionTest extends WordSpec with Matchers {
  Universe.createNew()
  val salesPair = ^^(JointDistribution.sales(0), JointDistribution.sales(1))
  val ve = VariableElimination(JointDistribution.sales(0), JointDistribution.sales(1), salesPair)
  ve.start()
  val imp = Importance(10000, JointDistribution.totalSales)
  imp.start()
  val firstSales = ve.probability(JointDistribution.sales(0), (i: Int) => i < 100)
  val secondSales = ve.probability(JointDistribution.sales(1), (i: Int) => i < 100)
  val bothSales = ve.probability(salesPair, (pair: (Int, Int)) => pair._1 < 100 && pair._2 < 100)
  val totalSales = imp.probability(JointDistribution.totalSales, (i: Int) => i < 2000)
  val meanIndSales = ve.expectation(JointDistribution.sales(0), (i: Int) => i.toDouble)
  val meanTotSales = imp.expectation(JointDistribution.totalSales, (i: Int) => i.toDouble)
  ve.kill()
  imp.kill()

  "Joint Distribution" should {
    "produce a probability first sales will be less than 100 = 0.5" taggedAs (BookExample) in {
      firstSales should be(0.5)
    }
    "produce a probability second sales will be less than 100 = 0.5" taggedAs (BookExample) in {
      secondSales should be(0.5)
    }
    "produce a probability both sales will be less than 100 = 0.2777777777777778" taggedAs (BookExample) in {
      bothSales should be(0.2777777777777778)
    }
    "produce a probability total sales are less than 2000 = 0.48 +- 0.02" taggedAs (BookExample, NonDeterministic) in {
      totalSales should be(0.48 +- 0.02)
    }
    "produce mean individual sales = 105.0" taggedAs (BookExample) in {
      meanIndSales should be(105.0)
    }
    "produce mean total sales = 2090 +- 20.0" taggedAs (BookExample, NonDeterministic) in {
      meanTotSales should be(2090.0 +- 20.0)
    }
  }
}
