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
import org.scalatest.Matchers
import com.cra.figaro.algorithm._
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.language._
import com.cra.figaro.library.atomic.continuous._
import com.cra.figaro.library.atomic.discrete.Binomial
import com.cra.figaro.library.compound._
import com.cra.figaro.test._
import com.cra.figaro.util.logSum
import JSci.maths.statistics._
import com.cra.figaro.test.tags.Performance
import com.cra.figaro.test.tags.NonDeterministic
import scala.language.reflectiveCalls 

class ImportanceTest extends WordSpec with Matchers with PrivateMethodTester {

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
    
    "with an observation on a compound flip, terminate quickly and produce the correct result" taggedAs(NonDeterministic) in {
      // Tests the likelihood weighting implementation for compound flip
      Universe.createNew()
      val b = Uniform(0.0, 1.0)
      val f1 = Flip(b)
      val f2 = Flip(b)
      val f3 = Flip(b)
      val f4 = Flip(b)
      val f5 = Flip(b)
      val f6 = Flip(b)
      val f7 = Flip(b)
      val f8 = Flip(b)
      val f9 = Flip(b)
      val f10 = Flip(b)
      val f11 = Flip(b)
      val f12 = Flip(b)
      val f13 = Flip(b)
      val f14 = Flip(b)
      val f15 = Flip(b)
      val f16 = Flip(b)
      val f17 = Flip(b)
      val f18 = Flip(b)
      val f19 = Flip(b)
      val f20 = Flip(b)
      f1.observe(true)
      f2.observe(true)
      f3.observe(true)
      f4.observe(true)
      f5.observe(true)
      f6.observe(true)
      f7.observe(true)
      f8.observe(true)
      f9.observe(true)
      f10.observe(true)
      f11.observe(true)
      f12.observe(true)
      f13.observe(true)
      f14.observe(true)
      f15.observe(true)
      f16.observe(true)
      f17.observe(false)
      f18.observe(false)
      f19.observe(false)
      f20.observe(false)
      val alg = Importance(b)
      alg.start()
      Thread.sleep(100)
      val time0 = System.currentTimeMillis()
      alg.stop()
      // Uniform(0,1) is beta(1,1)
      // Result is beta(1 + 16,1 + 4)
      // Expectation is (alpha) / (alpha + beta) = 17/22
      alg.expectation(b, (d: Double) => d) should be ((17.0/22.0) +- 0.02)
      val time1 = System.currentTimeMillis()
      // If likelihood weighting is working, stopping and querying the algorithm should be almost instantaneous
      // If likelihood weighting is not working, stopping and querying the algorithm requires waiting for a non-rejected sample
      (time1 - time0) should be <= (500L)
      alg.shutdown
    }
 
    "with an observation on a parameterized flip, terminate quickly and produce the correct result" taggedAs(NonDeterministic) in {
      // Tests the likelihood weighting implementation for compound flip
      Universe.createNew()
      val b = BetaParameter(2.0, 5.0)
      val f1 = Flip(b)
      val f2 = Flip(b)
      val f3 = Flip(b)
      val f4 = Flip(b)
      val f5 = Flip(b)
      val f6 = Flip(b)
      val f7 = Flip(b)
      val f8 = Flip(b)
      val f9 = Flip(b)
      val f10 = Flip(b)
      val f11 = Flip(b)
      val f12 = Flip(b)
      val f13 = Flip(b)
      val f14 = Flip(b)
      val f15 = Flip(b)
      val f16 = Flip(b)
      val f17 = Flip(b)
      val f18 = Flip(b)
      val f19 = Flip(b)
      val f20 = Flip(b)
      f1.observe(true)
      f2.observe(true)
      f3.observe(true)
      f4.observe(true)
      f5.observe(true)
      f6.observe(true)
      f7.observe(true)
      f8.observe(true)
      f9.observe(true)
      f10.observe(true)
      f11.observe(true)
      f12.observe(true)
      f13.observe(true)
      f14.observe(true)
      f15.observe(true)
      f16.observe(true)
      f17.observe(false)
      f18.observe(false)
      f19.observe(false)
      f20.observe(false)
      val alg = Importance(b)
      alg.start()
      Thread.sleep(100)
      val time0 = System.currentTimeMillis()
      alg.stop()
      // Result is beta(2 + 16,5 + 4)
      // Expectation is (alpha) / (alpha + beta) = 18/27
      alg.expectation(b, (d: Double) => d) should be ((18.0/27.0) +- 0.02)
      val time1 = System.currentTimeMillis()
      // If likelihood weighting is working, stopping and querying the algorithm should be almost instantaneous
      // If likelihood weighting is not working, stopping and querying the algorithm requires waiting for a non-rejected sample
      (time1 - time0) should be <= (500L)
      alg.shutdown
    }
 
    "with an observation on a parameterized binomial, terminate quickly and produce the correct result" in {
      // Tests the likelihood weighting implementation for chain
      Universe.createNew()
      val beta = Beta(2.0, 5.0)
      val bin = Binomial(2000, beta)
      bin.observe(1600)
      val alg = Importance(beta)
      alg.start()
      Thread.sleep(1000)
      val time0 = System.currentTimeMillis()
      alg.stop()
      // Result is beta(2 + 1600,5 + 400)
      // Expectation is (alpha) / (alpha + beta) = 1602/2007
      alg.expectation(beta, (d: Double) => d) should be ((1602.0/2007.0) +- 0.02)
      val time1 = System.currentTimeMillis()
      // If likelihood weighting is working, stopping and querying the algorithm should be almost instantaneous
      // If likelihood weighting is not working, stopping and querying the algorithm requires waiting for a non-rejected sample
      (time1 - time0) should be <= (500L)
      alg.shutdown
    }

    "with an observation on a chain, terminate quickly and produce the correct result" in {
      // Tests the likelihood weighting implementation for chain
      Universe.createNew()
      val beta = Uniform(0.0, 1.0)
      val bin = Binomial(2000, beta)
      bin.observe(1600)
      val alg = Importance(beta)
      alg.start()
      Thread.sleep(1000)
      val time0 = System.currentTimeMillis()
      alg.stop()
      // uniform(0,1) is beta(1,1)
      // Result is beta(1 + 1600,1 + 400)
      // Expectation is (alpha) / (alpha + beta) = 1601/2003
      alg.expectation(beta, (d: Double) => d) should be ((1601.0/2003.0) +- 0.02)
      val time1 = System.currentTimeMillis()
      // If likelihood weighting is working, stopping and querying the algorithm should be almost instantaneous
      // If likelihood weighting is not working, stopping and querying the algorithm requires waiting for a non-rejected sample
      (time1 - time0) should be <= (500L)
      alg.shutdown
    }

    "with an observation on a dist, terminate quickly and produce the correct result" in {
      // Tests the likelihood weighting implementation for dist
      Universe.createNew()
      val beta = Beta(2.0, 5.0)
      val dist = Dist(0.5 -> Constant(1000), 0.5 -> Binomial(2000, beta))
      dist.observe(1600) // forces it to choose bin, and observation should propagate to it
      val alg = Importance(beta)
      alg.start()
      Thread.sleep(1000)
      val time0 = System.currentTimeMillis()
      alg.stop()
      // Result is beta(2 + 1600,5 + 400)
      // Expectation is (alpha) / (alpha + beta) = 1602/2007
      alg.expectation(beta, (d: Double) => d) should be ((1602.0/2007.0) +- 0.02)
      val time1 = System.currentTimeMillis()
      // If likelihood weighting is working, stopping and querying the algorithm should be almost instantaneous
      // If likelihood weighting is not working, stopping and querying the algorithm requires waiting for a non-rejected sample
      (time1 - time0) should be <= (500L)
      alg.shutdown
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
      i.probability(f, true) should be(0.3 +- 0.01)
      i.kill()
      s.observe(0.6)
      i.start()
      i.probability(f, true) should be(0.6 +- 0.01)
      i.kill()
    }

    "resample elements inside class defined in a chain" in {
      Universe.createNew()
      class temp {
        val t1 = Flip(0.9)
      }
      val a = CachingChain(Constant(0), (i: Int) => Constant(new temp))
      val b = Apply(a, (t: temp) => t.t1.value)
      val alg = Importance(10000, b)
      alg.start
      alg.probability(b, true) should be (0.9 +- .01)
      alg.kill
    }

    "resample elements inside class defined in a chain for foward sampling" taggedAs(NonDeterministic) in {
      Universe.createNew()
      class temp {
        val t1 = Flip(0.9)
      }
      val a = CachingChain(Constant(0), (i: Int) => Constant(new temp))
      val b = Apply(a, (t: temp) => t.t1.value)
      val prob = List.fill(1000){Forward(Universe.universe); b.value}
      prob.count(_ == true).toDouble/1000.0 should be (0.9 +- .01)
      //alg.probability(b, true) should be (0.9 +- .01)

    }

   "not suffer from stack overflow with small probability of success" taggedAs (Performance) in {
      Universe.createNew()
      val f = Flip(0.000001)
      f.observe(true)
      val i = Importance(1, f)
      i.start
    }

    "not suffer from memory leaks" taggedAs (Performance) in {
      Universe.createNew()
      val c = NonCachingChain(Uniform(0.2, 1.0), (d: Double) => Flip(d))
      val i = Importance(1000000, c)
      i.start
    }
  }

  "Computing probability of evidence using importance sampling" when {

    "given a vanilla model with one condition" should {
      "return the probability the condition is satisfied" in {
        val universe = Universe.createNew()
        val f = Flip(0.7)("f", universe)
        probEvidenceTest(0.7, List(NamedEvidence("f", Observation(true))))
      }
    }

    "given a vanilla model with two independent conditions" should {
      "return the probability both conditions are satisfied" in {
        val universe = Universe.createNew()
        val f1 = Flip(0.7)("f1", universe)
        val f2 = Flip(0.4)("f2", universe)
        probEvidenceTest(0.7 * 0.4, List(NamedEvidence("f1", Observation(true)), NamedEvidence("f2", Observation(true))))
      }
    }

    "given a vanilla mode with two dependent conditions" should {
      "return the probability both conditions are jointly satisfied" in {
        val universe = Universe.createNew()
        val d = Select(0.2 -> 0.6, 0.8 -> 0.9)
        val f1 = Flip(d)("f1", universe)
        val f2 = Flip(d)("f2", universe)
        probEvidenceTest(0.2 * 0.6 * 0.6 + 0.8 * 0.9 * 0.9, List(NamedEvidence("f1", Observation(true)), NamedEvidence("f2", Observation(true))))
      }
    }

    "given a vanilla model with two dependent conditions and a constraint" should {
      "return the probability both conditions are satisfied, taking into account the constraint" in {
        val universe = Universe.createNew()
        val d = Select(0.5 -> 0.6, 0.5 -> 0.9)("d", universe)
        d.setConstraint((d: Double) => if (d > 0.7) 0.8; else 0.2)
        val f1 = Flip(d)("f1", universe)
        val f2 = Flip(d)("f2", universe)
        probEvidenceTest(0.2 * 0.6 * 0.6 + 0.8 * 0.9 * 0.9, List(NamedEvidence("f1", Observation(true)), NamedEvidence("f2", Observation(true))))
      }
    }

    "given a simple dist with a condition on the result" should {
      "return the expectation over the clauses of the probability the result satisfies the condition" in {
        val universe = Universe.createNew()
        val d = Dist(0.3 -> Flip(0.6), 0.7 -> Flip(0.9))("d", universe)
        probEvidenceTest(0.3 * 0.6 + 0.7 * 0.9, List(NamedEvidence("d", Observation(true))))
      }
    }

    "given a complex dist with a condition on the result" should {
      "return the expectation over the clauses of the probability the result satisfies the condition" in {
        val universe = Universe.createNew()
        val p1 = Select(0.2 -> 0.4, 0.8 -> 0.6)
        val p2 = Constant(0.4)
        val d = Dist(p1 -> Flip(0.6), p2 -> Flip(0.9))("d", universe)
        probEvidenceTest(0.2 * (0.5 * 0.6 + 0.5 * 0.9) + 0.8 * (0.6 * 0.6 + 0.4 * 0.9), List(NamedEvidence("d", Observation(true))))
      }
    }

    "given a continuous uniform with a condition" should {
      "return the uniform probability of the condition" in {
        val universe = Universe.createNew()
        val u = Uniform(0.0, 1.0)("u", universe)
        val condition = (d: Double) => d < 0.4
        probEvidenceTest(0.4, List(NamedEvidence("u", Condition(condition))))
      }
    }

    "given a caching chain with a condition on the result" should {
      "return the expectation over the parent of the probability the result satisfies the condition" in {
        val universe = Universe.createNew()
        val p1 = Select(0.4 -> 0.3, 0.6 -> 0.9)
        val c = CachingChain(p1, (d: Double) => if (d < 0.4) Flip(0.3); else Flip(0.8))("c", universe)
        probEvidenceTest(0.4 * 0.3 + 0.6 * 0.8, List(NamedEvidence("c", Observation(true))))
      }
    }

    "given a non-caching chain with a condition on the result" should {

      "return the expectation over the parent of the probability the result satisfies the condition" in {
        val universe = Universe.createNew()
        val p1 = Uniform(0.0, 1.0)
        val c = NonCachingChain(p1, (d: Double) => if (d < 0.4) Flip(0.3); else Flip(0.8))("c", universe)
        probEvidenceTest(0.4 * 0.3 + 0.6 * 0.8, List(NamedEvidence("c", Observation(true))))
      }
    }

    "given a chain of two arguments whose result is a different element with a condition on the result" should {
      "return the correct probability of evidence in the result" in {
        val universe = Universe.createNew()
        val x = Constant(false)
        val y = Constant(false)
        val u1 = Uniform(0.0, 1.0)
        val u2 = Uniform(0.0, 2.0)
        val a = CachingChain(x, y, (x: Boolean, y: Boolean) => if (x || y) u1; else u2)("a", universe)
        def condition(d: Double) = d < 0.5
        probEvidenceTest(0.25, List(NamedEvidence("a", Condition(condition))))
      }
    }

  }

  def weightedSampleTest[T](target: Element[T], predicate: T => Boolean, prob: Double) {
    val numTrials = 100000
    val tolerance = 0.01
    val algorithm = Importance(numTrials, target)
    algorithm.start()
    algorithm.probability(target, predicate) should be(prob +- tolerance)
  }

  def sampleOneTest[T](target: Element[T], predicate: T => Boolean, prob: Double) {
    val numTrials = 100000
    val tolerance = 0.01
    val imp = Importance(target)

    def attempt(): (Double, T) = {
      try {
        val state = Importance.State()
        val value = imp.sampleOne(state, target, None)
        (state.weight, value.asInstanceOf[T])
      } catch {
        case Importance.Reject => attempt()
      } 
      
    }

    var totalWeight = Double.NegativeInfinity
    var successWeight = Double.NegativeInfinity
    for { i <- 1 to numTrials } {
      val (weight, value) = attempt()
      if (predicate(value)) successWeight = logSum(weight, successWeight)
      totalWeight = logSum(weight, totalWeight)
    }    
    math.exp(successWeight - totalWeight) should be(prob +- tolerance)
    
    imp.shutdown
  }
  
  def probEvidenceTest(prob: Double, evidence: List[NamedEvidence[_]]) {
    val alg = Importance(10000)
    alg.start()
    alg.probabilityOfEvidence(evidence) should be (prob +- 0.01)
  }

  
}
