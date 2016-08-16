/*
 * SalesPrediction.scala 
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

package com.cra.figaro.test.book.chap06

import com.cra.figaro.language.Universe
import com.cra.figaro.library.atomic.continuous.Beta
import com.cra.figaro.language.Flip
import com.cra.figaro.algorithm.sampling.Importance
import com.cra.figaro.library.collection.Container
import com.cra.figaro.library.atomic.discrete.Poisson
import com.cra.figaro.language.Chain
import org.scalatest.Matchers
import org.scalatest.WordSpec
import com.cra.figaro.test.tags.BookExample
import com.cra.figaro.test.tags.NonDeterministic

object SalesPrediction {

  def main(args: Array[String]) {
    val products : Array[String] = Array("A","B","C","D","E")
    val numProducts = products.length
    val numRegions = products(0).length

    /* The model */
    val productQuality = Array.fill(numProducts)(Beta(2,2))
    val regionPenetration = Array.fill(numRegions)(Beta(2,2))
    def makeSales(i: Int, j: Int) = Flip(productQuality(i) * regionPenetration(j))
    val highSalesLastYear = Array.tabulate(numProducts, numRegions)(makeSales _)
    val highSalesNextYear = Array.tabulate(numProducts, numRegions)(makeSales _)

    def getSalesByProduct(i: Int) =
      for { j <- 0 until numRegions } yield highSalesNextYear(i)(j)
    val salesPredictionByProduct =
      Array.tabulate(numProducts)(i => Container(getSalesByProduct(i):_*))

    val numHighSales =
      for { predictions <- salesPredictionByProduct }
      yield predictions.count(b => b)

    val numHiresByProduct =
//      for { i <- 0 until numProducts }
//      yield Chain(numHighSales(i), (n: Int) => Poisson(n  + 1))
      Container(numHighSales:_*).chain((n: Int) => Poisson(n + 1))

    /* Observe all the sales */
    for {
      i <- 0 until numProducts
      j <- 0 until numRegions
    } {
      val observation = products(i)(j) == 'T'
      highSalesLastYear(i)(j).observe(observation)
    }

    /* Run inference */
    val targets = numHiresByProduct.elements
    val algorithm = Importance(targets:_*)
    algorithm.start()
    Thread.sleep(10000)
    algorithm.stop()

    /* Report the results */
    for { i <- 0 until numProducts } {
      println("Number of hires for product " + i + ": " + algorithm.expectation(numHiresByProduct(i), (n: Int) => n.toDouble))
    }
    algorithm.kill()
  }
}

class SalesPredictionTest extends WordSpec with Matchers {
  Universe.createNew()
  "Sales Prediction" should {
    val products : Array[String] = Array("A","B","C","D","E")
    val numProducts = products.length
    val numRegions = products(0).length

    /* The model */
    val productQuality = Array.fill(numProducts)(Beta(2,2))
    val regionPenetration = Array.fill(numRegions)(Beta(2,2))
    def makeSales(i: Int, j: Int) = Flip(productQuality(i) * regionPenetration(j))
    val highSalesLastYear = Array.tabulate(numProducts, numRegions)(makeSales _)
    val highSalesNextYear = Array.tabulate(numProducts, numRegions)(makeSales _)

    def getSalesByProduct(i: Int) =
      for { j <- 0 until numRegions } yield highSalesNextYear(i)(j)
    val salesPredictionByProduct =
      Array.tabulate(numProducts)(i => Container(getSalesByProduct(i):_*))

    val numHighSales =
      for { predictions <- salesPredictionByProduct }
      yield predictions.count(b => b)

    val numHiresByProduct =
//      for { i <- 0 until numProducts }
//      yield Chain(numHighSales(i), (n: Int) => Poisson(n  + 1))
      Container(numHighSales:_*).chain((n: Int) => Poisson(n + 1))

    /* Observe all the sales */
    for {
      i <- 0 until numProducts
      j <- 0 until numRegions
    } {
      val observation = products(i)(j) == 'T'
      highSalesLastYear(i)(j).observe(observation)
    }

    /* Run inference */
    val targets = numHiresByProduct.elements
    val algorithm = Importance(targets:_*)
    algorithm.start()
    Thread.sleep(10000)
    algorithm.stop()

    val hiresProd0 = algorithm.expectation(numHiresByProduct(0), (n: Int) => n.toDouble)
    val hiresProd1 = algorithm.expectation(numHiresByProduct(1))(n => n.toDouble)
    val hiresProd2 = algorithm.expectation(numHiresByProduct(2), (n: Int) => n.toDouble)    
    val hiresProd3 = algorithm.expectation(numHiresByProduct(3))(n => n.toDouble)
    val hiresProd4 = algorithm.expectation(numHiresByProduct(4), (n: Int) => n.toDouble)

    algorithm.kill()

    "produce a number of hires for Product 0 of 1.15 +- 0.3" taggedAs (BookExample, NonDeterministic) in {
      hiresProd0 should be(1.15 +- 0.3)
    }
    "produce a number of hires for Product 1 of 1.15 +- 0.3" taggedAs (BookExample, NonDeterministic) in {
      hiresProd1 should be(1.15 +- 0.3)
    }
    "produce a number of hires for Product 2 of 1.15 +- 0.3" taggedAs (BookExample, NonDeterministic) in {
      hiresProd2 should be(1.15 +- 0.3)
    }     
    "produce a number of hires for Product 3 of 1.15 +- 0.3" taggedAs (BookExample, NonDeterministic) in {
      hiresProd3 should be(1.15 +- 0.3)
    }
    "produce a number of hires for Product 4 of 1.15 +- 0.3" taggedAs (BookExample, NonDeterministic) in {
      hiresProd4 should be(1.15 +- 0.3)
    }
  }
}
