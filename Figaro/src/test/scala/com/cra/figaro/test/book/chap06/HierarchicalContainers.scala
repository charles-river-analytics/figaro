/*
 * HierarchicalContainers.scala 
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

import com.cra.figaro.language.{Flip, Constant, Universe}
import com.cra.figaro.library.atomic.continuous.{Uniform, Beta}
import com.cra.figaro.library.compound.If
import com.cra.figaro.algorithm.sampling.Importance
import com.cra.figaro.library.collection.FixedSizeArray
import org.scalatest.Matchers
import org.scalatest.WordSpec
import com.cra.figaro.test.tags.BookExample
import com.cra.figaro.test.tags.NonDeterministic

object HierarchicalContainers {
  def main(args: Array[String]) {
    val initialTosses : Array[String] = Array("0","1","0","1","0","1","0","1","0","1","0","1","0","1","0","1","0","1","0","1","0","1","0","1","0","1","0","1","0","1")
    val numCoins = initialTosses.length

    val fairProbability = Uniform(0.0, 1.0)

    val isFair = new FixedSizeArray(numCoins + 1, i => Flip(fairProbability))
    val biases = isFair.chain(if (_) Constant(0.5) else Beta(2,5))

    val tosses =
      for { coin <- 0 until numCoins } yield new FixedSizeArray(initialTosses(coin).length, i => Flip(biases(coin)))

    val hasHeads =
      for { coin <- 0 until numCoins } yield tosses(coin).exists(b => b)

    for {
      coin <- 0 until numCoins
      toss <- 0 until initialTosses(coin).length
    } {
      initialTosses(coin)(toss) match {
        case 'H' => tosses(coin)(toss).observe(true)
        case 'T' => tosses(coin)(toss).observe(false)
        case _ => ()
      }
    }

    val algorithm = Importance(fairProbability, hasHeads(2))
    algorithm.start()
    Thread.sleep(1000)
    algorithm.stop()
    println("Probability at least one of the tosses of the third coin was heads = " + algorithm.probability(hasHeads(2), true))
    algorithm.kill()
  }
}

class HierarchicalContainersTest extends WordSpec with Matchers {
  Universe.createNew()
  "Hierarchical Containers" should {
    val initialTosses : Array[String] = Array("0","1","0","1","0","1","0","1","0","1","0","1","0","1","0","1","0","1","0","1","0","1","0","1","0","1","0","1","0","1")
    val numCoins = initialTosses.length

    val fairProbability = Uniform(0.0, 1.0)

    val isFair = new FixedSizeArray(numCoins + 1, i => Flip(fairProbability))
    val biases = isFair.chain(if (_) Constant(0.5) else Beta(2,5))

    val tosses =
      for { coin <- 0 until numCoins } yield new FixedSizeArray(initialTosses(coin).length, i => Flip(biases(coin)))

    val hasHeads =
      for { coin <- 0 until numCoins } yield tosses(coin).exists(b => b)

    for {
      coin <- 0 until numCoins
      toss <- 0 until initialTosses(coin).length
    } {
      initialTosses(coin)(toss) match {
        case 'H' => tosses(coin)(toss).observe(true)
        case 'T' => tosses(coin)(toss).observe(false)
        case _ => ()
      }
    }

    val algorithm = Importance(fairProbability, hasHeads(2))
    algorithm.start()
    Thread.sleep(1000)
    algorithm.stop()
    val prob = algorithm.probability(hasHeads(2), true)
    algorithm.kill()

    "have a Probability at least one of the tosses of the third coin was heads equal to 0.36 +- 0.05" taggedAs (BookExample, NonDeterministic) in {
      prob should be(0.36 +- 0.05)
    }
  }
}

