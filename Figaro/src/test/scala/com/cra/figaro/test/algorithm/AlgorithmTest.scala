/*
 * AlgorithmTest.scala 
 * Algorithm tests.
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Jan 1, 2009
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.test.algorithm

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.WordSpec
import com.cra.figaro.algorithm._
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.language._
import scala.collection.mutable.Map

class AlgorithmTest extends WordSpec with ShouldMatchers {
  "An algorithm" should {
    "not allow queries before starting" in {
      Universe.createNew()
      val c = Flip(0.3)
      val a = new SimpleAlgorithm(c)
      evaluating { a.distribution(c) } should produce[AlgorithmInactiveException]
      evaluating { a.expectation(c, (b: Boolean) => 1.0) } should produce[AlgorithmInactiveException]
      evaluating { a.probability(c, (b: Boolean) => true) } should produce[AlgorithmInactiveException]
      evaluating { a.probability(c, true) } should produce[AlgorithmInactiveException]
    }

    "allow queries after starting, stopping, and resuming" in {
      Universe.createNew()
      val c = Flip(0.3)
      val a = new SimpleAlgorithm(c)
      a.start()
      a.distribution(c)
      a.expectation(c, (b: Boolean) => 1.0)
      a.probability(c, (b: Boolean) => true)
      a.probability(c, true)
      a.stop()
      a.distribution(c)
      a.expectation(c, (b: Boolean) => 1.0)
      a.probability(c, (b: Boolean) => true)
      a.probability(c, true)
      a.resume()
      a.distribution(c)
      a.expectation(c, (b: Boolean) => 1.0)
      a.probability(c, (b: Boolean) => true)
      a.probability(c, true)
    }

    "not allow queries after killing" in {
      Universe.createNew()
      val c = Flip(0.3)
      val a = new SimpleAlgorithm(c)
      a.start()
      a.kill()
      evaluating { a.distribution(c) } should produce[AlgorithmInactiveException]
      evaluating { a.expectation(c, (b: Boolean) => 1.0) } should produce[AlgorithmInactiveException]
      evaluating { a.probability(c, (b: Boolean) => true) } should produce[AlgorithmInactiveException]
      evaluating { a.probability(c, true) } should produce[AlgorithmInactiveException]
    }

    "not allow start after starting" in {
      Universe.createNew()
      val c = Flip(0.3)
      val a = new SimpleAlgorithm(c)
      a.start()
      evaluating { a.start() } should produce[AlgorithmActiveException]
    }

    "not allow stop, resume, or kill before starting" in {
      Universe.createNew()
      val c = Flip(0.3)
      val a = new SimpleAlgorithm(c)
      evaluating { a.stop() } should produce[AlgorithmInactiveException]
      evaluating { a.resume() } should produce[AlgorithmInactiveException]
      evaluating { a.kill() } should produce[AlgorithmInactiveException]
    }

    "allow start after starting and killing" in {
      Universe.createNew()
      val c = Flip(0.3)
      val a = new SimpleAlgorithm(c)
      a.start()
      a.kill()
      a.start()
    }

    "compute the probability of a query as the sum of probabilities of values that satisfy the query" in {
      Universe.createNew()
      val f = Flip(0.3)
      val a = new SimpleAlgorithm(f)
      a.start()
      a.probability(f, (b: Boolean) => b) should equal(0.3)
    }

    "compute the probability of a value" in {
      Universe.createNew()
      val f = Flip(0.3)
      val a = new SimpleAlgorithm(f)
      a.start()
      a.probability(f, true) should equal(0.3)
    }
  }

  "An anytime algorithm" should {
    "have an answer immediately after initialization" in {
      Universe.createNew()
      val c = Flip(0.3)
      val a = new SimpleAnytime(c)
      a.start()
      a.expectation(c, (b: Boolean) => -1.0) should be > (0.0)
      a.kill()
    }

    "have a stable answer after stopping" in {
      Universe.createNew()
      val c = Flip(0.3)
      val a = new SimpleAnytime(c)
      a.start()
      a.stop()
      val x = a.expectation(c, (b: Boolean) => -1.0)
      a.expectation(c, (b: Boolean) => -1.0) should equal(x)
      a.kill()
    }

    "have an unstable answer after resuming" in {
      Universe.createNew()
      val c = Flip(0.3)
      val a = new SimpleAnytime(c)
      a.start()
      a.stop()
      a.resume()
      val x = a.expectation(c, (b: Boolean) => -1.0)
      a.expectation(c, (b: Boolean) => -1.0) should be > (x)
      a.kill()
    }
  }

  "A one time sampler" should {
    "generate the given number of samples" in {
      Universe.createNew()
      val myFlip = Flip(0.8)
      val s = new SimpleWeighted(myFlip)
      s.start()
      s.count should equal(3)
    }
  }

  "A weighted sampler" should {
    "produce a distribution where each sampled value is associated with its total normalized weight" in {
      Universe.createNew()
      val myFlip = Flip(0.8)
      val s = new SimpleWeighted(myFlip)
      s.start()
      val d: List[(Double, Boolean)] = s.distribution(myFlip).toList.sorted
      d should equal(List((1.0 / 3, true), (2.0 / 3, false)))
    }

    "compute the query of a predicate as the normalized weighted sum of elements that satisfy the query" in {
      Universe.createNew()
      val myFlip = Flip(0.8)
      val s = new SimpleWeighted(myFlip)
      s.start()
      s.probability(myFlip, (b: Boolean) => b) should equal(1.0 / 3)
    }
  }

  class SimpleAlgorithm(val f: AtomicFlip) extends ProbQueryAlgorithm(Universe.universe, f) {
    val p = f.prob

    def doStart() = ()

    def doStop() = ()

    def doResume() = ()

    def doKill() = ()

    def computeDistribution[T](target: Element[T]): Stream[(Double, T)] = Stream()

    // Assumes a uniform distribution over the values
    def computeExpectation[T](target: Element[T], function: T => Double): Double =
      p * function(true.asInstanceOf[T]) + (1 - p) * function(false.asInstanceOf[T])

    def doDistribution[T](target: Element[T]): Stream[(Double, T)] = computeDistribution(target)

    def doExpectation[T](target: Element[T], function: T => Double) = computeExpectation(target, function)

    def doProbability[T](target: Element[T], predicate: T => Boolean) =
      doExpectation(target, (t: T) => if (predicate(t)) 1.0; else 0.0)
  }

  class SimpleAnytime(targets: Element[_]*) extends ProbQueryAlgorithm(Universe.universe, targets: _*)
    with AnytimeProbQuery {
    var count = -1000000000

    override def initialize() = count = 0

    def runStep() = count += 1

    def computeDistribution[T](target: Element[T]): Stream[(Double, T)] =
      Stream(Values(Universe.universe)(target).toList: _*) map ((1.0, _))

    def computeExpectation[T](target: Element[T], function: T => Double): Double = count.toDouble
  }

  class SimpleWeighted(myFlip: Flip) extends WeightedSampler(Universe.universe, myFlip) with OneTimeProbQuerySampler {
    val numSamples = 3

    var count = 0

    def sample() = {
      count += 1
      (count.toDouble, Map(myFlip -> (count % 2 == 0)))
    }
  }
}
