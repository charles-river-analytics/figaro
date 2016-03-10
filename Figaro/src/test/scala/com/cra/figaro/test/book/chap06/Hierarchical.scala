/*
 * Hierarchical.scala 
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

import com.cra.figaro.language.{Flip, Constant, Universe} //#A
import com.cra.figaro.library.atomic.continuous.{Uniform, Beta} //#A
import com.cra.figaro.library.compound.If //#A
import com.cra.figaro.algorithm.sampling.Importance //#A
import org.scalatest.Matchers
import org.scalatest.WordSpec
import com.cra.figaro.test.tags.BookExample
import com.cra.figaro.test.tags.NonDeterministic

object Hierarchical {
  def main(args: Array[String]) {
    val initialTosses : Array[String] = Array("010101010101010101010101010101")
    val numCoins = initialTosses.length

    val fairProbability = Uniform(0.0, 1.0)

    val isFair =
      for { coin <- 0 until numCoins }
      yield Flip(fairProbability)
    val biases =
      for { coin <- 0 until numCoins }
      yield If(isFair(coin), Constant(0.5), Beta(2,5))

    val tosses =
      for { coin <- 0 until numCoins }
      yield {
        for { toss <- 0 until initialTosses(coin).length }
        yield Flip(biases(coin))
      }

    for {
      coin <- 0 until numCoins
      toss <- 0 until initialTosses(coin).length
    } {
      val outcome = initialTosses(coin)(toss) == 'H'
      tosses(coin)(toss).observe(outcome)
    }

    val algorithm = Importance(fairProbability, biases(0))
    algorithm.start()
    Thread.sleep(1000)
    algorithm.stop()

    val averageFairProbability = algorithm.mean(fairProbability)
    val firstCoinAverageBias = algorithm.mean(biases(0))
    println("Average fairness probability: " + averageFairProbability)
    println("First coin average bias: " + firstCoinAverageBias)

    algorithm.kill()
  }
}

class HierarchicalTest extends WordSpec with Matchers {
  Universe.createNew()
  "Hierarchical" should {
    val initialTosses : Array[String] = Array("010101010101010101010101010101")
    val numCoins = initialTosses.length

    val fairProbability = Uniform(0.0, 1.0)

    val isFair =
      for { coin <- 0 until numCoins }
      yield Flip(fairProbability)
    val biases =
      for { coin <- 0 until numCoins }
      yield If(isFair(coin), Constant(0.5), Beta(2,5))

    val tosses =
      for { coin <- 0 until numCoins }
      yield {
        for { toss <- 0 until initialTosses(coin).length }
        yield Flip(biases(coin))
      }

    for {
      coin <- 0 until numCoins
      toss <- 0 until initialTosses(coin).length
    } {
      val outcome = initialTosses(coin)(toss) == 'H'
      tosses(coin)(toss).observe(outcome)
    }

    val algorithm = Importance(fairProbability, biases(0))
    algorithm.start()
    Thread.sleep(1000)
    algorithm.stop()

    val averageFairProbability = algorithm.mean(fairProbability)
    val firstCoinAverageBias = algorithm.mean(biases(0))

    algorithm.kill()

    "have an average fairness probability equal to 0.03 +- 0.03" taggedAs (BookExample, NonDeterministic) in {
      averageFairProbability should be(0.34 +- 0.03)
    }
    "have a first coin average bias equal to 0.05 +- 0.03" taggedAs (BookExample, NonDeterministic) in {
      firstCoinAverageBias should be(0.05 +- 0.03)
    }
  }
}

