/*
 * ImportanceTest.scala  
 * Importance sampling tests.
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Jan 1, 2009
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.test.algorithm.sampling

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers
import com.cra.figaro.algorithm._
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.language._
import com.cra.figaro.library.atomic.continuous._
import com.cra.figaro.library.compound._
import com.cra.figaro.test._

class ImportanceTest extends WordSpec with ShouldMatchers with PrivateMethodTester {

  "Sampling a value of a single element" should {
    "for a Constant return the constant with probability 1" in {
      Universe.createNew()
      val c = Constant(8)
      sampleOneTest(c, (i: Int) => i == 8, 1.0)
    }

    "for a Uniform return a range with probability proportional to the size of the range" in {
      Universe.createNew()
      val u = Uniform(0.2, 1.0)
      sampleOneTest(u, (d: Double) => 0.3 <= d && d < 0.5, 0.25)
    }

    "for a simple Flip return true with probability of the argument" in {
      Universe.createNew()
      val f = Flip(0.3)
      sampleOneTest(f, (b: Boolean) => b, 0.3)
    }

    "for a complex Flip return true with probability equal to the expectation of the argument" in {
      Universe.createNew()
      val f = Flip(Uniform(0.2, 1.0))
      sampleOneTest(f, (b: Boolean) => b, 0.6)
    }

    "for a Select with simple probabilities return an outcome with the correct probability" in {
      Universe.createNew()
      val s = Select(0.2 -> 1, 0.3 -> 2, 0.5 -> 3)
      sampleOneTest(s, (i: Int) => i == 2, 0.3)
    }

    "for a Select with complex probabilities return an outcome with the correct probability" in {
      Universe.createNew()
      val s = Select(Select(0.25 -> 0.2, 0.75 -> 0.8) -> 1, Constant(0.4) -> 2)
      sampleOneTest(s, (i: Int) => i == 2, 0.25 * 0.4 / 0.6 + 0.75 * 0.4 / 1.2)
    }

    "for a Dist with simple probabilities return an outcome with the correct probability" in {
      Universe.createNew()
      val d = Dist(0.2 -> Constant(true), 0.8 -> Flip(0.3))
      sampleOneTest(d, (b: Boolean) => b, 0.2 + 0.8 * 0.3)
    }

    "for a Dist with complex probabilities return an outcome with the correct probability" in {
      Universe.createNew()
      val d = Dist(Select(0.25 -> 0.2, 0.75 -> 0.8) -> Constant(true), Constant(0.4) -> Flip(0.3))
      val p = 0.25 * (0.2 / 0.6 + (0.4 / 0.6) * 0.3) + 0.75 * (0.8 / 1.2 + (0.4 / 1.2) * 0.3)
      sampleOneTest(d, (b: Boolean) => b, p)
    }

    "for an Apply with one argument return an outcome with probability equal to the sum of its inverse images" in {
      Universe.createNew()
      val a = Apply(Select(0.2 -> 1, 0.3 -> 2, 0.5 -> 3), (i: Int) => i > 1)
      sampleOneTest(a, (b: Boolean) => b, 0.8)
    }

    "for an Apply with two arguments return an outcome with probability equal to the sum of its inverse images" in {
      Universe.createNew()
      val a = Apply(Select(0.5 -> 1, 0.5 -> 2), Select(0.2 -> 1, 0.3 -> 2, 0.5 -> 3),
        (i: Int, j: Int) => j > i)
      sampleOneTest(a, (b: Boolean) => b, 0.5 * (0.3 + 0.5) + 0.5 * 0.5)
    }

    "for an NonCaching two parent Chain return an outcome with probability equal to the expectation over the parent" in {
      Universe.createNew()
      val i1 = Select(0.4 -> 1, 0.6 -> 2)
      val i2 = Constant(2)
      val c = NonCachingChain(Flip(0.3), Flip(0.8), (b1: Boolean, b2: Boolean) => if (b1 && b2) i1; else i2)
      sampleOneTest(c, (i: Int) => i == 2, (0.3 * 0.8) * 0.6 + (0.7 * 0.2 + 0.3 * 0.2 + 0.7 * 0.8))
    }

    "for an NonCachingChain return an outcome with probability equal to the expectation over the parents" in {
      Universe.createNew()
      val i1 = Select(0.4 -> 1, 0.6 -> 2)
      val i2 = Constant(2)
      val c = NonCachingChain(Flip(0.3), (b: Boolean) => if (b) i1; else i2)
      sampleOneTest(c, (i: Int) => i == 2, 0.3 * 0.6 + 0.7)
    }

    "for a Caching two parent Chain return an outcome with probability equal to the expectation over the parent" in {
      Universe.createNew()
      val i1 = Select(0.4 -> 1, 0.6 -> 2)
      val i2 = Constant(2)
      val c = CachingChain(Flip(0.3), Flip(0.8), (b1: Boolean, b2: Boolean) => if (b1 && b2) i1; else i2)
      sampleOneTest(c, (i: Int) => i == 2, (0.3 * 0.8) * 0.6 + (0.7 * 0.2 + 0.3 * 0.2 + 0.7 * 0.8))
    }

    "for a CachingChain return an outcome with probability equal to the expectation over the parent" in {
      Universe.createNew()
      val i1 = Select(0.4 -> 1, 0.6 -> 2)
      val i2 = Constant(2)
      val c = CachingChain(Flip(0.3), (b: Boolean) => if (b) i1; else i2)
      sampleOneTest(c, (i: Int) => i == 2, 0.3 * 0.6 + 0.7)
    }

    "for an Inject return an outcome with probability equal to the probability of its inverse image" in {
      Universe.createNew()
      val u1 = Uniform(0.0, 2.0)
      val u2 = Constant(1.5)
      val i = Inject(u1, u2)
      sampleOneTest(i, (d: Seq[Double]) => d.length == 2 && 0.5 <= d(0) && d(0) < 1.0 && d(1) == 1.5, 0.25)
    }

    "for an If with simple consequents return a consequent with probability equal to the test" in {
      Universe.createNew()
      val i = If(Flip(0.3), 1, 2)
      sampleOneTest(i, (i: Int) => i == 2, 0.7)
    }

    "with a condition on the element return the correct conditional probability" in {
      Universe.createNew()
      val u = Uniform(0.2, 1.0)
      u.setCondition((d: Double) => 0.25 <= d && d < 0.65)
      sampleOneTest(u, (d: Double) => 0.3 <= d && d < 0.5, 0.5)
    }

    "with a constraint on the element return the correct probability taking into account the constraint" in {
      Universe.createNew()
      val u = Uniform(0.2, 1.0)
      def constraint(d: Double) = if (0.3 <= d && d < 0.5) 3.0; else 1.0
      u.setConstraint(constraint)
      sampleOneTest(u, (d: Double) => 0.3 <= d && d < 0.5, 0.5)
    }

    "with a condition on a related element return the correct conditional probability" in {
      Universe.createNew()
      val u = Uniform(0.2, 1.0)
      u.setCondition((d: Double) => 0.25 <= d && d < 0.65)
      val f = Flip(u)
      sampleOneTest(f, (b: Boolean) => b, 0.45)
    }

    "with a constraint on a related element return the correct probability taking into account the constraint" in {
      Universe.createNew()
      val u = Uniform(0.0, 1.0)
      u.setConstraint((d: Double) => d)
      val f = Flip(u)
      // Expected value of flip argument is (\int_0^1 x^2 dx) / (\int_0^1 x dx) = 2/3
      sampleOneTest(f, (b: Boolean) => b, 2.0 / 3.0)
    }
  }

  "Producing a weighted sample of an element in a universe" should {
    "with no conditions or constraints produce the same result as sampling the element individually" in {
      Universe.createNew()
      val u = Uniform(0.2, 1.0)
      val f = Flip(u)
      val a = If(f, Select(0.3 -> 1, 0.7 -> 2), Constant(2))
      weightedSampleTest(f, (b: Boolean) => b, 0.6)
    }

    "with a condition on a dependent element produce the result with the correct probability" in {
      Universe.createNew()
      val u = Uniform(0.2, 1.0)
      val f = Flip(u)
      val a = If(f, Select(0.3 -> 1, 0.7 -> 2), Constant(2))
      a.setCondition((i: Int) => i == 2)
      // U(true) = \int_{0.2}^{1.0) 0.7 p = 0.35 * 0.96
      // U(false) = \int_{0.2}^{1.0) (1-p)
      val u1 = 0.35 * 0.96
      val u2 = 0.32
      weightedSampleTest(f, (b: Boolean) => b, u1 / (u1 + u2))
    }

    "with a constraint on a dependent element produce the result with the correct probability" in {
      Universe.createNew()
      val u = Uniform(0.2, 1.0)
      val f = Flip(u)
      val a = If(f, Select(0.3 -> 1, 0.7 -> 2), Constant(2))
      a.setConstraint((i: Int) => i.toDouble)
      // U(true) = \int_{0.2}^{1.0} (0.3 + 2 * 0.7) p = 0.85 * 0.96
      // U(false) = \int_{0.2}^(1.0) (2 * (1-p)) = 0.64
      val u1 = 0.85 * 0.96
      val u2 = 0.64
      weightedSampleTest(f, (b: Boolean) => b, u1 / (u1 + u2))
    }

    "with an element that uses another element multiple times, " +
      "always produce the same value for the different uses" in {
        Universe.createNew()
        val f = Flip(0.5)
        val e = f === f
        weightedSampleTest(e, (b: Boolean) => b, 1.0)
      }

    "with a constraint on an element that is used multiple times, only factor in the constraint once" in {
      Universe.createNew()
      val f1 = Flip(0.5)
      val f2 = Flip(0.3)
      val e1 = f1 === f1
      val e2 = f1 === f2
      val d = Dist(0.5 -> e1, 0.5 -> e2)
      f1.setConstraint((b: Boolean) => if (b) 3.0; else 2.0)
      // Probability that f1 is true = 0.6
      // Probability that e1 is true = 1.0
      // Probability that e2 is true = 0.6 * 0.3 + 0.4 * 0.7 = 0.46
      // Probability that d is true = 0.5 * 1 + 0.5 * 0.46 = 0.73
      weightedSampleTest(d, (b: Boolean) => b, 0.73)
    }
  }

  "Running importance sampling" should {
    "produce the correct answer each time when run twice with different conditions" in {
      Universe.createNew()
      val s = Select(0.5 -> 0.3, 0.5 -> 0.6)
      val f = Flip(s)
      val i = Importance(20000, f)
      s.observe(0.3)
      i.start()
      i.probability(f, true) should be(0.3 plusOrMinus 0.01)
      i.kill()
      s.observe(0.6)
      i.start()
      i.probability(f, true) should be(0.6 plusOrMinus 0.01)
      i.kill()
    }

    "not suffer from stack overflow with small probability of success" taggedAs (PerformanceTest) in {
      Universe.createNew()
      val f = Flip(0.000001)
      f.observe(true)
      val i = Importance(1, f)
      i.start()
    }

    "not suffer from memory leaks" taggedAs (PerformanceTest) in {
      Universe.createNew()
      val c = NonCachingChain(Uniform(0.2, 1.0), (d: Double) => Flip(d))
      val i = Importance(1000000, c)
      i.start()
    }
  }

  def weightedSampleTest[T](target: Element[T], predicate: T => Boolean, prob: Double) {
    val numTrials = 100000
    val tolerance = 0.01
    val algorithm = Importance(numTrials, target)
    algorithm.start()
    algorithm.probability(target, predicate) should be(prob plusOrMinus tolerance)
    algorithm.kill()
  }

  def sampleOneTest[T](target: Element[T], predicate: T => Boolean, prob: Double) {
    val numTrials = 100000
    val tolerance = 0.01
    val imp = Importance(target)

    def attempt(): (Double, T) = {
      try {
        val state = Importance.State()
        val value = imp.sampleOne(state, target)
        (state.weight, value.asInstanceOf[T])
      } catch {
        case Importance.Reject => attempt()
      }
    }

    var totalWeight = 0.0
    var successWeight = 0.0
    for { i <- 1 to numTrials } {
      val (weight, value) = attempt()
      if (predicate(value)) successWeight += weight
      totalWeight += weight
    }
    (successWeight / totalWeight) should be(prob plusOrMinus tolerance)
  }
}
