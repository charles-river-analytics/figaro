/*
 * InfiniteExpectation.scala
 * A model where the expected depth of recursion is infinite.
 *
 * Created By:      William Kretschmer (kretsch@mit.edu)
 * Creation Date:   Aug 08, 2017
 *
 * Copyright 2017 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */
package com.cra.figaro.example.lazystructured

import com.cra.figaro.algorithm.lazyfactored.{Extended, Regular}
import com.cra.figaro.algorithm.structured.algorithm.laziness.AnytimeLSVE
import com.cra.figaro.algorithm.structured.strategy.range._
import com.cra.figaro.language._
import com.cra.figaro.library.atomic.continuous.{AtomicBeta, Beta}
import org.apache.commons.math3.distribution.BetaDistribution

/**
 * An example demonstrating refinement by expansion and range binning through lazy structured factored inference. This
 * model terminates with probability 1, but the expected depth of recursion is infinite. Thus, an inference strategy
 * that bounds the depth of recursive expansion is needed.
 */
object InfiniteExpectation {
  def main(args: Array[String]): Unit = {
    val model = new InfiniteExpectationModel
    // Amount by which to increment the depth of expansion at each iteration
    val depthIncrement = 5
    // Number of additional values to take at each iteration
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
    println(s"Estimated bounds on the posterior mean of ${model.prob}:")
    for(_ <- 1 to 20) {
      Thread.sleep(1000)
      // The bounds produced here are only approximate because the element is continuous
      println(alg.expectationBounds(model.prob, (d: Double) => d, Some(0.0), Some(1.0)))
    }
    alg.stop()
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
