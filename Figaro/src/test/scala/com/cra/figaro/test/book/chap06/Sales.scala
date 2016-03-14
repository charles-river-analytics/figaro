/*
 * Sales.scala 
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
import org.scalatest.Matchers
import org.scalatest.WordSpec
import com.cra.figaro.test.tags.BookExample
import com.cra.figaro.test.tags.NonDeterministic

object Sales {

  def main(args: Array[String]) {
    val regions : Array[String] = Array("A","B","C","D","E")
    val numProducts = regions.length
    val numRegions = regions(0).length

    /* The model */
    val productQuality = Array.fill(numProducts)(Beta(2,2))
    val regionPenetration = Array.fill(numRegions)(Beta(2,2))
    def makeSales(i: Int, j: Int) = Flip(productQuality(i) * regionPenetration(j))
    val highSales = Array.tabulate(numProducts, numRegions)(makeSales _)

    /* Observe all the sales */
    for {
      i <- 0 until numProducts
      j <- 0 until numRegions
    } {
      val observation = regions(i)(j) == 'T'
      highSales(i)(j).observe(observation)
    }

    /* Run inference */
    val targets = productQuality ++ regionPenetration
    val algorithm = Importance(targets:_*)
    algorithm.start()
    Thread.sleep(1000)
    algorithm.stop()

    /* Report the results */
    for { i <- 0 until numProducts } {
      println("Product " + i + " quality: " + algorithm.mean(productQuality(i)))
    }
    for { j <- 0 until numRegions } {
      println("Region " + j + " penetration: " + algorithm.mean(regionPenetration(j)))
    }
    algorithm.kill()
  }
}

class SalesTest extends WordSpec with Matchers {
  Universe.createNew()
  "Sales" should {
    val products : Array[String] = Array("A","B","C","D","E")
    val numProducts = products.length
    val numRegions = products(0).length
  
    /* The model */
    val productQuality = Array.fill(numProducts)(Beta(2,2))
    val regionPenetration = Array.fill(numRegions)(Beta(2,2))
    def makeSales(i: Int, j: Int) = Flip(productQuality(i) * regionPenetration(j))
    val highSales = Array.tabulate(numProducts, numRegions)(makeSales _)
  
    /* Observe all the sales */
    for {
      i <- 0 until numProducts
      j <- 0 until numRegions
    } {
      val observation = products(i)(j) == 'T'
      highSales(i)(j).observe(observation)
    }
  
    /* Run inference */
    val targets = productQuality ++ regionPenetration
    val algorithm = Importance(targets:_*)
    algorithm.start()
    Thread.sleep(1000)
    algorithm.stop()
    val prod0 = algorithm.mean(productQuality(0))
    val prod1 = algorithm.mean(productQuality(1))
    val prod2 = algorithm.mean(productQuality(2))
    val prod3 = algorithm.mean(productQuality(3))
    val prod4 = algorithm.mean(productQuality(4))
    val reg0 = algorithm.mean(regionPenetration(0))
    algorithm.kill()

    "produce a Product 0 quality of 0.47 +- 0.3" taggedAs (BookExample, NonDeterministic) in {
      prod0 should be(0.47 +- 0.3)
    }
    "produce a Product 1 quality of 0.47 +- 0.3" taggedAs (BookExample, NonDeterministic) in {
      prod1 should be(0.47 +- 0.3)
    }
    "produce a Product 2 quality of 0.47 +- 0.3" taggedAs (BookExample, NonDeterministic) in {
      prod2 should be(0.47 +- 0.3)
    }     
    "produce a Product 3 quality of 0.47 +- 0.3" taggedAs (BookExample, NonDeterministic) in {
      prod3 should be(0.47 +- 0.3)
    }
    "produce a Product 4 quality of 0.47 +- 0.3" taggedAs (BookExample, NonDeterministic) in {
      prod4 should be(0.47 +- 0.3)
    }
    "produce a Region 0 penetration of 0.35 +- 0.3" taggedAs (BookExample, NonDeterministic) in {
      reg0 should be(0.35 +- 0.3)
    }
  }
}
