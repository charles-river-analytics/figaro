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

import org.scalatest.Matchers
import org.scalatest.WordSpec
import com.cra.figaro.algorithm._
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.language._
import com.cra.figaro.library.atomic.continuous._
import com.cra.figaro.library.compound._
import scala.collection.mutable.Map
import com.cra.figaro.library.atomic.discrete.FromRange
import com.cra.figaro.library.atomic.discrete.Binomial

class AlgorithmTest extends WordSpec with Matchers {
  "An algorithm" should {
    "not allow queries before starting" in {
      Universe.createNew()
      val c = Flip(0.3)
      val a = new SimpleAlgorithm(c)
      an[AlgorithmInactiveException] should be thrownBy { a.distribution(c) }
      an[AlgorithmInactiveException] should be thrownBy { a.expectation(c, (b: Boolean) => 1.0) }
      an[AlgorithmInactiveException] should be thrownBy { a.probability(c)(b => true) }
      an[AlgorithmInactiveException] should be thrownBy { a.probability(c, true) }
    }

    "allow queries after starting, stopping, and resuming" in {
      Universe.createNew()
      val c = Flip(0.3)
      val a = new SimpleAlgorithm(c)
      a.start()
      a.distribution(c)
      a.expectation(c, (b: Boolean) => 1.0)
      a.probability(c)(b => true)
      a.probability(c, true)
      a.stop()
      a.distribution(c)
      a.expectation(c)(b => 1.0)
      a.probability(c)(b => true)
      a.probability(c, true)
      a.resume()
      a.distribution(c)
      a.expectation(c)(b => 1.0)
      a.probability(c)(b => true)
      a.probability(c, true)
    }

    "not allow queries after killing" in {
      Universe.createNew()
      val c = Flip(0.3)
      val a = new SimpleAlgorithm(c)
      a.start()
      a.kill()
      an[AlgorithmInactiveException] should be thrownBy { a.distribution(c) }
      an[AlgorithmInactiveException] should be thrownBy { a.expectation(c, (b: Boolean) => 1.0) }
      an[AlgorithmInactiveException] should be thrownBy { a.probability(c)(b => true) }
      an[AlgorithmInactiveException] should be thrownBy { a.probability(c, true) }
    }

    "not allow start after starting" in {
      Universe.createNew()
      val c = Flip(0.3)
      val a = new SimpleAlgorithm(c)
      a.start()
      an[AlgorithmActiveException] should be thrownBy { a.start() }
    }

    "not allow stop, resume, or kill before starting" in {
      Universe.createNew()
      val c = Flip(0.3)
      val a = new SimpleAlgorithm(c)
      an[AlgorithmInactiveException] should be thrownBy { a.stop() }
      an[AlgorithmInactiveException] should be thrownBy { a.resume() }
      an[AlgorithmInactiveException] should be thrownBy { a.kill() }
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
      a.probability(f)(b => b) should equal(0.3)
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
      a.shutdown
    }

    "have a stable answer after stopping" in {
      Universe.createNew()
      val c = Flip(0.3)
      val a = new SimpleAnytime(c)
      a.start()
      a.stop()
      val x = a.expectation(c)(b => -1.0)
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
      a.expectation(c)(b => -1.0) should be > (x)
      a.kill()
    }

    "block on stop, kill or queries" in {
      Universe.createNew()
      val num = 400
      def makeModel(depth: Int, elems: List[Element[_]]): List[Element[_]] = {
        if (depth > 0) {
          val e = Chain(FromRange(2, 10), (i: Int) => Binomial(i, 0.2))
          makeModel(depth - 1, elems :+ e)
        } else elems
      }
      val l = makeModel(num, List())

      for { _ <- 0 until 20 } {
        val alg = Importance(l: _*)
        alg.start
        Thread.sleep(1000)
        alg.stop
        val init = Universe.universe.activeElements.size
        alg.kill
        val after = Universe.universe.activeElements.size
        after should equal (400*2)
      }

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
      d(0)._1 should be(1.0 / 3 +- 0.001)
      d(0)._2 should equal(true)
      d(1)._1 should be(2.0 / 3 +- 0.001)
      d(1)._2 should equal(false)
    }

    "compute the query of a predicate as the normalized weighted sum of elements that satisfy the query" in {
      Universe.createNew()
      val myFlip = Flip(0.8)
      val s = new SimpleWeighted(myFlip)
      s.start()
      s.probability(myFlip)(b => b) should be(1.0 / 3 +- 0.001)
    }
  }

  "A probability of query algorithm" should {
    "return the correct mean of an element" in {
      Universe.createNew()
      val u = Normal(0.5, 1)
      val n = Normal(u, 1)
      val imp = Importance(100000, n)
      imp.start()
      imp.mean(n) should be(0.5 +- 0.01)
    }

    "return the correct variance of an element" in {
      Universe.createNew()
      val u = Normal(0.5, 1)
      val n = Normal(u, 1)
      val imp = Importance(100000, n)
      imp.start()
      imp.variance(n) should be(2.0 +- 0.05)
      imp.kill()
    }

    "return the correct element representing the posterior probability distribution of an element" in {
      val u1 = Universe.createNew()
      val x = Flip(0.6)
      val y = If(x, Flip(0.6), Flip(0.3))
      y.observe(true)
      val alg1 = VariableElimination(x)
      alg1.start()
      val u2 = Universe.createNew()
      val z = alg1.posteriorElement(x, u2)
      alg1.kill()
      val alg2 = VariableElimination(z)
      alg2.start()
      alg2.probability(z, true) should be(0.75 +- 0.00000001)
      alg2.kill()
    }
  }

  class SimpleAlgorithm(val f: AtomicFlip) extends ProbQueryAlgorithm {
    lazy val universe = Universe.universe
    lazy val queryTargets = List(f)

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

  class SimpleAnytime(targets: Element[_]*) extends AnytimeProbQuery {
    lazy val universe = Universe.universe
    lazy val queryTargets = targets.toList

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
      (math.log(count.toDouble), Map(myFlip -> (count % 2 == 0)))
    }
  }
}
