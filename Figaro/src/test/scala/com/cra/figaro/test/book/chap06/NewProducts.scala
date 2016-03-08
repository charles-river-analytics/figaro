/*
 * NewProducts.scala 
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

import com.cra.figaro.library.atomic.discrete.Geometric
import com.cra.figaro.library.atomic.continuous.{Beta, Normal}
import com.cra.figaro.library.collection.VariableSizeArray
import com.cra.figaro.algorithm.sampling.Importance
import com.cra.figaro.language.Universe
import com.cra.figaro.language.Flip
import org.scalatest.Matchers
import org.scalatest.WordSpec
import com.cra.figaro.test.tags.BookExample
import com.cra.figaro.test.tags.NonDeterministic

object NewProducts {
  def runExperiment(rNDLevel: Double) {
    Universe.createNew()
    val numNewProducts = Geometric(rNDLevel)
    val productQuality = VariableSizeArray(numNewProducts, i => Beta(1, i + 1))
    val productSalesRaw = productQuality.chain(Normal(_, 0.5))
    val productSales = productSalesRaw.map(_.max(0))
    val totalSales = productSales.foldLeft(0.0)(_ + _)
    val algorithm = Importance(totalSales)
    algorithm.start()
    Thread.sleep(5000)
    algorithm.stop()
    println("With R&D at " + rNDLevel + ", expected sales will be " + algorithm.mean(totalSales))
    algorithm.kill()
  }

  def main(args: Array[String]) {
    var i : Double = 0.0
    for { i <- 0.05 to 1.0 by 0.1 } {
      println(i)
      runExperiment(i)
    }
  }
}

class NewProductsTest extends WordSpec with Matchers {
  def testFunc {
    for { i <- 0.05 to 1.0 by 0.1 } {
      NewProducts.runExperiment(i)
    }    
  }

  "New Products" should {
    "produce correct results at each step" taggedAs (BookExample, NonDeterministic) in {
      testFunc
    }
  }
}
