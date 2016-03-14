/*
 * ProductDistribution.scala 
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

package com.cra.figaro.test.book.chap05

import com.cra.figaro.language._
import com.cra.figaro.library.atomic.discrete.{Binomial, Poisson}
import com.cra.figaro.library.compound.If
import com.cra.figaro.algorithm.sampling.Importance
import org.scalatest.Matchers
import org.scalatest.WordSpec
import com.cra.figaro.test.tags.BookExample
import com.cra.figaro.test.tags.NonDeterministic

object ProductDistribution {
  class Network(popularity: Double) {
    val numNodes = Poisson(popularity)
  }

  class Model(targetPopularity: Double, productQuality: Double, affordability: Double) {
    def generateLikes(numFriends: Int, productQuality: Double): Element[Int] = {
      def helper(friendsVisited: Int, totalLikes: Int, unprocessedLikes: Int): Element[Int] = {
        if (unprocessedLikes == 0) Constant(totalLikes)
        else {
          val unvisitedFraction = //#C
            1.0 - (friendsVisited.toDouble - 1)/ (numFriends - 1) //#C
          val newlyVisited = Binomial(2, unvisitedFraction)
          val newlyLikes = Binomial(newlyVisited, Constant(productQuality))
          Chain(newlyVisited, newlyLikes,
                (visited: Int, likes: Int) =>
                  helper(friendsVisited + visited, totalLikes + likes, unprocessedLikes + likes - 1))
        }
      }
      helper(1, 1, 1)
    }

    val targetSocialNetwork = new Network(targetPopularity)
    val targetLikes = Flip(productQuality)
    val numberFriendsLike =
      Chain(targetLikes, targetSocialNetwork.numNodes,
            (l: Boolean, n: Int) =>
              if (l) generateLikes(n, productQuality); else Constant(0))
    val numberBuy = Binomial(numberFriendsLike, Constant(affordability))
  }

  def predict(targetPopularity: Double, productQuality: Double, affordability: Double): Double = {
    val model = new Model(targetPopularity, productQuality, affordability)
    val algorithm = Importance(1000, model.numberBuy)
    algorithm.start()
    val result = algorithm.expectation(model.numberBuy, (i: Int) => i.toDouble)
    algorithm.kill()
    result
  }

  def main(args: Array[String]) {
    println("Popularity\tProduct quality\tAffordability\tPredicted number of buyers")
    println("100       \t0.5            \t0.5          \t" + predict(100, 0.5, 0.5))
    println("100       \t0.5            \t0.9          \t" + predict(100, 0.5, 0.9))
    println("100       \t0.9            \t0.5          \t" + predict(100, 0.9, 0.5))
    println("100       \t0.9            \t0.9          \t" + predict(100, 0.9, 0.9))
    println("10        \t0.5            \t0.5          \t" + predict(10, 0.5, 0.5))
    println("10        \t0.5            \t0.9          \t" + predict(10, 0.5, 0.9))
    println("10        \t0.9            \t0.5          \t" + predict(10, 0.9, 0.5))
    println("10        \t0.9            \t0.9          \t" + predict(10, 0.9, 0.9))
  }
}

class ProductDistributionTest extends WordSpec with Matchers {
  Universe.createNew()
  "Product Distribution" should {
    "produce the correct results at each step" taggedAs (BookExample, NonDeterministic) in {
      ProductDistribution.predict(100, 0.5, 0.5) should be(2.0259999999999976 +- 1.0)
      ProductDistribution.predict(100, 0.5, 0.9) should be(3.562999999999997 +- 1.0)
      ProductDistribution.predict(100, 0.9, 0.5) should be(29.016999999999967 +- 1.0)
      ProductDistribution.predict(100, 0.9, 0.9) should be(52.58599999999993 +- 2.0)
      ProductDistribution.predict(10, 0.5, 0.5) should be(0.8219999999999981 +- 1.0)
      ProductDistribution.predict(10, 0.5, 0.9) should be(1.3489999999999978 +- 1.0)
      ProductDistribution.predict(10, 0.9, 0.5) should be(3.385999999999989 +- 1.0)
      ProductDistribution.predict(10, 0.9, 0.9) should be(6.143999999999982 +- 1.0)
    }
  }
}
