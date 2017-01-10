/*
 * ParImportanceTest.scala  
 * ParImportance sampling tests.
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
import com.cra.figaro.library.atomic._
import com.cra.figaro.library.atomic.discrete.Binomial
import com.cra.figaro.library.compound._
import com.cra.figaro.test._
import JSci.maths.statistics._
import com.cra.figaro.test.tags.Performance
import com.cra.figaro.test.tags.NonDeterministic
import org.scalatest.Matchers
import org.scalatest.{ PrivateMethodTester, WordSpec }
import com.cra.figaro.language.Name.stringToName
import com.cra.figaro.language.Reference.stringToReference
class ParImportanceTest extends WordSpec with Matchers with PrivateMethodTester {
  
  val numThreads = 3
  
  "Producing a weighted sample of an element in a universe" should {
    "with no conditions or constraints produce the same result as sampling the element individually" in {
      val gen: Function0[Universe] = () => {
        val universe = Universe.createNew()
        val u = Uniform(0.2, 1.0)
        val f = Flip(u)("f", universe)
        val a = If(f, Select(0.3 -> 1, 0.7 -> 2)(Name.default, universe), Constant(2)(Name.default, universe))
        universe
      }
      weightedSampleTest(gen, "f", (b: Boolean) => b, 0.6)
    }

    "with a condition on a dependent element produce the result with the correct probability" in {
      val gen = () => {
        val universe = Universe.createNew()
        val u = Uniform(0.2, 1.0)
        val f = Flip(u)("f", universe)
        val a = If(f, Select(0.3 -> 1, 0.7 -> 2)(Name.default, universe), Constant(2)(Name.default, universe))
        a.setCondition((i: Int) => i == 2)
        universe
      }
      // U(true) = \int_{0.2}^{1.0) 0.7 p = 0.35 * 0.96
      // U(false) = \int_{0.2}^{1.0) (1-p)
      val u1 = 0.35 * 0.96
      val u2 = 0.32
      weightedSampleTest(gen, "f", (b: Boolean) => b, u1 / (u1 + u2))
    }

    "with a constraint on a dependent element produce the result with the correct probability" in {
      val gen = () => {
        val universe = Universe.createNew()
        val u = Uniform(0.2, 1.0)
        val f = Flip(u)("f", universe)
        val a = If(f, Select(0.3 -> 1, 0.7 -> 2)(Name.default, universe), Constant(2)(Name.default, universe))
        a.setConstraint((i: Int) => i.toDouble)
        universe
      }
      // U(true) = \int_{0.2}^{1.0} (0.3 + 2 * 0.7) p = 0.85 * 0.96
      // U(false) = \int_{0.2}^(1.0) (2 * (1-p)) = 0.64
      val u1 = 0.85 * 0.96
      val u2 = 0.64
      weightedSampleTest(gen, "f", (b: Boolean) => b, u1 / (u1 + u2))
    }

    "with an element that uses another element multiple times, " +
      "always produce the same value for the different uses" in {
        val gen = () => {
          val universe = Universe.createNew()
          val f = Flip(0.5)
          val e = new Eq("e", f, f, universe)
          universe
        }
        weightedSampleTest(gen, "e", (b: Boolean) => b, 1.0)
      }

    "with a constraint on an element that is used multiple times, only factor in the constraint once" in {
      val gen = () => {
        val universe = Universe.createNew()
        val f1 = Flip(0.5)
        val f2 = Flip(0.3)
        val e1 = f1 === f1
        val e2 = f1 === f2
        val d = Dist(0.5 -> e1, 0.5 -> e2)("d", universe)
        f1.setConstraint((b: Boolean) => if (b) 3.0; else 2.0)
        universe
      }
      // Probability that f1 is true = 0.6
      // Probability that e1 is true = 1.0
      // Probability that e2 is true = 0.6 * 0.3 + 0.4 * 0.7 = 0.46
      // Probability that d is true = 0.5 * 1 + 0.5 * 0.46 = 0.73
      weightedSampleTest(gen, "d", (b: Boolean) => b, 0.73)
    }

    "with an observation on a compound flip, terminate quickly and produce the correct result" taggedAs (NonDeterministic) in {
      // Tests the likelihood weighting implementation for compound flip
      val gen = () => {
        val universe = new Universe
        val b = Uniform(0.0, 1.0)("b", universe)
        for (_ <- 1 to 16) { Flip(b)("", universe).observe(true) }
        for (_ <- 1 to 4) { Flip(b)("", universe).observe(false) }
        universe
      }
      val alg = Importance.par(gen, numThreads, "b")
      alg.start()
      Thread.sleep(100)
      val time0 = System.currentTimeMillis()
      alg.stop()
      // Uniform(0,1) is beta(1,1)
      // Result is beta(1 + 16,1 + 4)
      // Expectation is (alpha) / (alpha + beta) = 17/22
      val exp = alg.expectation("b", (d: Double) => d)
      val time1 = System.currentTimeMillis()
      // If likelihood weighting is working, stopping and querying the algorithm should be almost instantaneous
      // If likelihood weighting is not working, stopping and querying the algorithm requires waiting for a non-rejected sample
      alg.shutdown
      (time1 - time0) should be <= (500L)
      exp should be((17.0 / 22.0) +- 0.02)
    }

    "with an observation on a parameterized flip, terminate quickly and produce the correct result" taggedAs (NonDeterministic) in {
      // Tests the likelihood weighting implementation for compound flip
      val gen = () => {
        val universe = new Universe
        val b = Beta(2.0, 5.0)("b", universe)
        for (_ <- 1 to 16) { Flip(b)("", universe).observe(true) }
        for (_ <- 1 to 4) { Flip(b)("", universe).observe(false) }
        universe
      }
      val alg = Importance.par(gen, numThreads, "b")
      alg.start()
      Thread.sleep(100)
      val time0 = System.currentTimeMillis()
      alg.stop()
      // Result is beta(2 + 16,5 + 4)
      // Expectation is (alpha) / (alpha + beta) = 18/27
      val exp = alg.expectation("b")((d: Double) => d)
      val time1 = System.currentTimeMillis()
      // If likelihood weighting is working, stopping and querying the algorithm should be almost instantaneous
      // If likelihood weighting is not working, stopping and querying the algorithm requires waiting for a non-rejected sample
      alg.shutdown
      (time1 - time0) should be <= (500L)
      exp should be((18.0 / 27.0) +- 0.02)
    }

    "with an observation on a parameterized binomial, terminate quickly and produce the correct result" in {
      // Tests the likelihood weighting implementation for chain
      val gen = () => {
        val universe = new Universe
        val beta = Beta(2.0, 5.0)("beta", universe)
        val bin = Binomial(2000, beta)("", universe)
        bin.observe(1600)
        universe
      }
      val alg = Importance.par(gen, numThreads, "beta")
      alg.start()
      Thread.sleep(1000)
      val time0 = System.currentTimeMillis()
      alg.stop()
      // Result is beta(2 + 1600,5 + 400)
      // Expectation is (alpha) / (alpha + beta) = 1602/2007
      val exp = alg.expectation("beta")((d: Double) => d)
      val time1 = System.currentTimeMillis()
      // If likelihood weighting is working, stopping and querying the algorithm should be almost instantaneous
      // If likelihood weighting is not working, stopping and querying the algorithm requires waiting for a non-rejected sample
      alg.shutdown
      (time1 - time0) should be <= (500L)
      exp should be((1602.0 / 2007.0) +- 0.02)
    }

    "with an observation on a chain, terminate quickly and produce the correct result" in {
      // Tests the likelihood weighting implementation for chain
      val gen = () => {
        val universe = new Universe
        val beta = Uniform(0.0, 1.0)("beta", universe)
        val bin = Binomial(2000, beta)("", universe)
        bin.observe(1600)
        universe
      }
      val alg = Importance.par(gen, numThreads, "beta")
      alg.start()
      Thread.sleep(1000)
      val time0 = System.currentTimeMillis()
      alg.stop()
      // uniform(0,1) is beta(1,1)
      // Result is beta(1 + 1600,1 + 400)
      // Expectation is (alpha) / (alpha + beta) = 1601/2003
      val exp = alg.expectation("beta")((d: Double) => d)
      val time1 = System.currentTimeMillis()
      // If likelihood weighting is working, stopping and querying the algorithm should be almost instantaneous
      // If likelihood weighting is not working, stopping and querying the algorithm requires waiting for a non-rejected sample
      alg.shutdown
      (time1 - time0) should be <= (500L)
      exp should be((1601.0 / 2003.0) +- 0.02)
    }

    "with an observation on a dist, terminate quickly and produce the correct result" in {
      // Tests the likelihood weighting implementation for dist
      val gen = () => {
        val universe = new Universe
        val beta = Beta(2.0, 5.0)("beta", universe)
        val dist = Dist(0.5 -> Constant(1000)(Name.default, universe), 0.5 -> Binomial(2000, beta)(Name.default, universe))("", universe)
        dist.observe(1600) // forces it to choose bin, and observation should propagate to it
        universe
      }
      val alg = Importance.par(gen, numThreads, "beta")
      alg.start()
      Thread.sleep(1000)
      val time0 = System.currentTimeMillis()
      alg.stop()
      // Result is beta(2 + 1600,5 + 400)
      // Expectation is (alpha) / (alpha + beta) = 1602/2007
      val exp = alg.expectation("beta")((d: Double) => d)
      val time1 = System.currentTimeMillis()
      // If likelihood weighting is working, stopping and querying the algorithm should be almost instantaneous
      // If likelihood weighting is not working, stopping and querying the algorithm requires waiting for a non-rejected sample
      alg.shutdown
      (time1 - time0) should be <= (500L)
      exp should be((1602.0 / 2007.0) +- 0.02)
    }
  }

  "Running ParImportance sampling" should {
    "produce the correct answer each time when run twice with different conditions" in {
      def gen(obs: Double*) = () => {
        val universe = Universe.createNew()
        val s = Select(0.5 -> 0.3, 0.5 -> 0.6)
        val f = Flip(s)("f", universe)
        for (o <- obs) {
          s.observe(o)
        }
        universe
      }
      
      val i1 = Importance.par(gen(0.3), numThreads, 20000, "f")
      i1.start()
      i1.probability("f", true) should be(0.3 +- 0.01)
      i1.kill()
      
      val i2 = Importance.par(gen(0.3, 0.6), numThreads, 20000, "f")
      i2.start()
      i2.probability("f", true) should be(0.6 +- 0.01)
      i2.kill()
    }

    /* Test is not valid 
    "resample elements inside class defined in a chain" in {
      val gen = () => {
        val universe = Universe.createNew()
        class temp {
          val t1 = Flip(0.9)
        }
        val a = CachingChain(Constant(0), (i: Int) => Constant(new temp))
        val b = Apply(a, (t: temp) => t.t1.value)("b", universe)
        universe
      }
      val alg = Importance.par(gen, numThreads, 10000, "b")
      alg.start
      alg.probability("b", true) should be(0.9 +- .01)
      alg.kill
    }
    * 
    */

    "not suffer from stack overflow with small probability of success" taggedAs (Performance) in {
      val gen = () => {
        val universe = Universe.createNew()
        val f = Flip(0.000001)("f", universe)
        f.observe(true)
        universe
      }
      val i = Importance.par(gen, numThreads, 1, "f")
      i.start
    }

    "not suffer from memory leaks" taggedAs (Performance) in {
      val gen = () => {
        val universe = Universe.createNew()
        val c = NonCachingChain(Uniform(0.2, 1.0), (d: Double) => Flip(d)(Name.default, universe))("c", universe)
        universe
      }
      val i = Importance.par(gen, numThreads, 1000000, "c")
      i.start
    }
  }

  "Computing probability of evidence using ParImportance sampling" when {

    "given a vanilla model with one condition" should {
      "return the probability the condition is satisfied" in {
        val gen = () => {
          val universe = Universe.createNew()
          val f = Flip(0.7)("f", universe)
          universe
        }
        probEvidenceTest(gen, 0.7, List(NamedEvidence("f", Observation(true))))
      }
    }

    "given a vanilla model with two independent conditions" should {
      "return the probability both conditions are satisfied" in {
        val gen = () => {
          val universe = Universe.createNew()
          val f1 = Flip(0.7)("f1", universe)
          val f2 = Flip(0.4)("f2", universe)
          universe
        }
        val prob = 0.7 * 0.4
        val evidence = List(NamedEvidence("f1", Observation(true)), NamedEvidence("f2", Observation(true)))
        
        val alg = Importance.par(gen, numThreads, 10000)
        alg.start()
        alg.probabilityOfEvidence(evidence) should be(prob +- 0.01)
      }
    }

    "given a vanilla mode with two dependent conditions" should {
      "return the probability both conditions are jointly satisfied" in {
        val gen = () => {
          val universe = Universe.createNew()
          val d = Select(0.2 -> 0.6, 0.8 -> 0.9)
          val f1 = Flip(d)("f1", universe)
          val f2 = Flip(d)("f2", universe)
          universe
        }
        probEvidenceTest(gen, 0.2 * 0.6 * 0.6 + 0.8 * 0.9 * 0.9, List(NamedEvidence("f1", Observation(true)), NamedEvidence("f2", Observation(true))))
      }
    }

    "given a vanilla model with two dependent conditions and a constraint" should {
      "return the probability both conditions are satisfied, taking into account the constraint" in {
        val gen = () => {
          val universe = Universe.createNew()
          val d = Select(0.5 -> 0.6, 0.5 -> 0.9)("d", universe)
          d.setConstraint((d: Double) => if (d > 0.7) 0.8; else 0.2)
          val f1 = Flip(d)("f1", universe)
          val f2 = Flip(d)("f2", universe)
          universe
        }
        probEvidenceTest(gen, 0.2 * 0.6 * 0.6 + 0.8 * 0.9 * 0.9, List(NamedEvidence("f1", Observation(true)), NamedEvidence("f2", Observation(true))))
      }
    }

    "given a simple dist with a condition on the result" should {
      "return the expectation over the clauses of the probability the result satisfies the condition" in {
        val gen = () => {
          val universe = Universe.createNew()
          val d = Dist(0.3 -> Flip(0.6), 0.7 -> Flip(0.9))("d", universe)
          universe
        }
        probEvidenceTest(gen, 0.3 * 0.6 + 0.7 * 0.9, List(NamedEvidence("d", Observation(true))))
      }
    }

    "given a complex dist with a condition on the result" should {
      "return the expectation over the clauses of the probability the result satisfies the condition" in {
        val gen = () => {
          val universe = Universe.createNew()
          val p1 = Select(0.2 -> 0.4, 0.8 -> 0.6)
          val p2 = Constant(0.4)
          val d = Dist(p1 -> Flip(0.6), p2 -> Flip(0.9))("d", universe)
          universe
        }
        probEvidenceTest(gen, 0.2 * (0.5 * 0.6 + 0.5 * 0.9) + 0.8 * (0.6 * 0.6 + 0.4 * 0.9), List(NamedEvidence("d", Observation(true))))
      }
    }

    "given a continuous uniform with a condition" should {
      "return the uniform probability of the condition" in {
        val gen = () => {
          val universe = Universe.createNew()
          val u = Uniform(0.0, 1.0)("u", universe)
          universe
        }
        val condition = (d: Double) => d < 0.4
        probEvidenceTest(gen, 0.4, List(NamedEvidence("u", Condition(condition))))
      }
    }

    "given a caching chain with a condition on the result" should {
      "return the expectation over the parent of the probability the result satisfies the condition" in {
        val gen = () => {
          val universe = Universe.createNew()
          val p1 = Select(0.4 -> 0.3, 0.6 -> 0.9)
          val c = CachingChain(p1, (d: Double) => if (d < 0.4) Flip(0.3)(Name.default, universe); else Flip(0.8)(Name.default, universe))("c", universe)
          universe
        }
        probEvidenceTest(gen, 0.4 * 0.3 + 0.6 * 0.8, List(NamedEvidence("c", Observation(true))))
      }
    }

    "given a non-caching chain with a condition on the result" should {

      "return the expectation over the parent of the probability the result satisfies the condition" in {
        val gen = () => {
          val universe = Universe.createNew()
          val p1 = Uniform(0.0, 1.0)
          val c = NonCachingChain(p1, (d: Double) => if (d < 0.4) Flip(0.3)(Name.default, universe); else Flip(0.8)(Name.default, universe))("c", universe)
          universe
        }
        probEvidenceTest(gen, 0.4 * 0.3 + 0.6 * 0.8, List(NamedEvidence("c", Observation(true))))
      }
    }

    "given a chain of two arguments whose result is a different element with a condition on the result" should {
      "return the correct probability of evidence in the result" in {
        val gen = () => {
          val universe = Universe.createNew()
          val x = Constant(false)
          val y = Constant(false)
          val u1 = Uniform(0.0, 1.0)
          val u2 = Uniform(0.0, 2.0)
          val a = CachingChain(x, y, (x: Boolean, y: Boolean) => if (x || y) u1; else u2)("a", universe)
          universe
        }
        def condition(d: Double) = d < 0.5
        probEvidenceTest(gen, 0.25, List(NamedEvidence("a", Condition(condition))))
      }
    }

  }

  def weightedSampleTest[T](gen: Function0[Universe], target: Reference[T], predicate: T => Boolean, prob: Double) {
    val numTrials = 100000
    val tolerance = 0.01
    val algorithm = Importance.par(gen, numThreads, numTrials, target)
    algorithm.start()
    algorithm.probability(target, predicate) should be(prob +- tolerance)
  }
  
  def probEvidenceTest(gen: Function0[Universe], prob: Double, evidence: List[NamedEvidence[_]]) {
    val alg = Importance.par(gen, numThreads, 10000)
    alg.start()
    alg.probabilityOfEvidence(evidence) should be(prob +- 0.01)
  }

}
  
