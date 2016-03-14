/*
 * HealthProcess.scala 
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

import com.cra.figaro.language._
import com.cra.figaro.library.collection.Process
import com.cra.figaro.library.atomic.discrete.Uniform
import com.cra.figaro.library.compound.{^^, If}
import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.library.compound.If
import org.scalatest.Matchers
import org.scalatest.WordSpec
import com.cra.figaro.test.tags.BookExample
import com.cra.figaro.test.tags.NonDeterministic

object HealthProcess extends Process[Double, Boolean] {
  val healthyPrior = Uniform((0.05 to 0.95 by 0.1):_*)
  val healthChangeRate = Uniform((0.001 to 0.1 by 0.002):_*)

  def generate(time: Double): Element[Boolean] = Flip(healthyPrior)

  def generate(times: List[Double]): Map[Double, Element[Boolean]] = {
    val sortedTimes = times.sorted
    val healthy = sortedTimes.map(time => (time, generate(time))).toMap
    def makePairs(remaining: List[Double]) {
      if (remaining.length >= 2) {
        val time1 :: time2 :: rest = remaining
        val probChange = Apply(healthChangeRate, (d: Double) => 1 - math.exp(- (time2 - time1) / d))
        val equalHealth = healthy(time1) === healthy(time2)
        val healthStatusChecker = If(equalHealth, Constant(true), Flip(probChange))
        healthStatusChecker.observe(true)
        makePairs(time2 :: rest)
      }
    }
    makePairs(sortedTimes)
    healthy
  }

  def rangeCheck(time: Double) = time >= 0

  def main(args: Array[String]) {
    val data = Map(0.1 -> true, 0.25 -> true, 0.3 -> false, 0.31 -> false, 0.34 -> false, 0.36 -> false, 0.4 -> true, 0.5 -> true, 0.55 -> true)
    val queries = List(0.35, 0.37, 0.45, 0.6)
    val targets = queries ::: data.keys.toList

    val healthy = generate(targets)
    for { (time, value) <- data } {
      healthy(time).observe(value)
    }

    val queryElements = queries.map(healthy(_))
    val queryTargets = healthyPrior :: healthChangeRate :: queryElements
    val algorithm = VariableElimination(queryTargets:_*)
    algorithm.start()
    for { query <- queries } {
      println("Probability the patient is healthy at time " + query + " = " + algorithm.probability(healthy(query), true))
    }
    println("Expected prior probability of healthy = " + algorithm.mean(healthyPrior))
    println("Expected health change rate = " + algorithm.mean(healthChangeRate))
    algorithm.kill()
  }
}

class HealthProcessTest extends WordSpec with Matchers {
  Universe.createNew()
  "Health Process" should {
    val healthyPrior = Uniform((0.05 to 0.95 by 0.1):_*)
    val healthChangeRate = Uniform((0.001 to 0.1 by 0.002):_*)

    val data = Map(0.1 -> true, 0.25 -> true, 0.3 -> false, 0.31 -> false, 0.34 -> false, 0.36 -> false, 0.4 -> true, 0.5 -> true, 0.55 -> true)
    val queries = List(0.35, 0.37, 0.45, 0.6)
    val targets = queries ::: data.keys.toList

    val healthy = HealthProcess.generate(targets)
    for { (time, value) <- data } {
      healthy(time).observe(value)
    }

    val queryElements = queries.map(healthy(_))
    val queryTargets = healthyPrior :: healthChangeRate :: queryElements
    val algorithm = VariableElimination(queryTargets:_*)
    algorithm.start()
    for { query <- queries } {
      println("Probability the patient is healthy at time " + query + " = " + algorithm.probability(healthy(query), true))
    }
    val pHealthy = algorithm.mean(healthyPrior)
    val expectedHcr = algorithm.mean(healthChangeRate)
    algorithm.kill()

    "have an expected prior probability of healthy equal to 0.449 +- 0.003" taggedAs (BookExample, NonDeterministic) in {
      pHealthy should be(0.449 +- 0.003)
    }
    "have an expected health change rate equal to 0.050 +- 0.003" taggedAs (BookExample, NonDeterministic) in {
      expectedHcr should be(0.050 +- 0.003)
    }
  }
}
