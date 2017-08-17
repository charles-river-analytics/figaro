/*
 * InfiniteExpectationTest.scala
 * Infinite expectation example tests.
 *
 * Created By:      William Kretschmer (kretsch@mit.edu)
 * Creation Date:   Aug 09, 2017
 *
 * Copyright 2017 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */
package com.cra.figaro.test.example.lazystructured

import com.cra.figaro.algorithm.lazyfactored.{Extended, Regular}
import com.cra.figaro.algorithm.structured.algorithm.laziness.AnytimeLSVE
import com.cra.figaro.algorithm.structured.strategy.range.{AtomicRanger, FiniteRanger, RangingStrategy}
import com.cra.figaro.language._
import com.cra.figaro.library.atomic.continuous.{AtomicBeta, Beta}
import com.cra.figaro.test.tags.Example
import org.apache.commons.math3.distribution.BetaDistribution
import org.scalatest.{Matchers, WordSpec}

class InfiniteExpectationTest extends WordSpec with Matchers {
  "Lazy inference on a model with infinite expected recursion" should {
    "converge to the posterior mean of the Beta" taggedAs Example in {
      val model = new InfiniteExpectationModel
      val depthIncrement = 5
      val valuesPerIteration = 5
      val alg = new AnytimeLSVE(depthIncrement, model.universe, model.prob) {
        override lazy val rangingStrategy = new RangingStrategy {
          override def apply[T](atomic: Atomic[T]): AtomicRanger[T] = {
            atomic match {
              case flip: AtomicFlip => new FiniteRanger(flip)
              case beta: AtomicBeta => new BetaBinningRanger(beta, valuesPerIteration)
            }
          }
        }
      }
      alg.start()
      Thread.sleep(10000)
      alg.stop()
      val (lower, upper) = alg.expectationBounds(model.prob, (d: Double) => d, Some(0.0), Some(1.0))
      // If the value of prob is x, the probability that the condition is satisfied is the sum from n=0 to infinity of
      // x*(1-x)^3n, which converges to 1/(x^2-3x+3)
      // The posterior density of e1 is proportional to (1-x)^4/(x^2-3x+3), which integrates to 0.0803
      // Thus, the mean is approximately the integral from 0 to 1 of x*(1-x)^4/(x^2-3x+3) / 0.0803 dx
      val actualMean = 0.1902
      lower should be < actualMean
      (actualMean - lower) should be < 0.02
      upper should be > actualMean
      (upper - actualMean) should be < 0.02
      alg.kill()
    }
  }

  class InfiniteExpectationModel {
    val universe = Universe.createNew()

    // Prior on the probability of termination at each iteration
    val prob = Beta(1, 5)

    // A simple recursive element that uses Chain function memoization
    def recursiveElement(): Element[Int] = Chain(Flip(prob), recursiveFunction)
    val recursiveFunction: Boolean => Element[Int] = (b: Boolean) => {
      if(b) Constant(0)
      else recursiveElement().map(i => (i + 1) % 3)
    }
    val elem = recursiveElement()
    elem.observe(0)
  }

  /**
   * Custom ranging for the continuous Beta parameter using a binning strategy
   */
  class BetaBinningRanger(beta: AtomicBeta, valuesPerIteration: Int) extends AtomicRanger(beta) {
    val dist = new BetaDistribution(beta.aValue, beta.bValue)

    // This is not fully refinable because it acts on an element that has infinite range
    override def fullyRefinable() = false

    // Total number of values (bins) to use at the current iteration
    var totalValues: Int = 0

    override def discretize() = {
      // Take additional values each iteration
      totalValues += valuesPerIteration
      // Make equally-spaced bins, each weighted by the prior probability of sampling from that bin
      val probs = for(i <- 0 until totalValues) yield {
        // Lower and upper bounds on this bin
        val lower = i.toDouble / totalValues
        val upper = (i+1).toDouble / totalValues
        // Assign the value to the middle of the bin
        val mid = (lower + upper) / 2
        Regular(mid) -> dist.probability(lower, upper)
      }
      // This returns a discrete distribution that approximates the Beta distribution given
      probs.toMap[Extended[Double], Double]
    }
  }
}
