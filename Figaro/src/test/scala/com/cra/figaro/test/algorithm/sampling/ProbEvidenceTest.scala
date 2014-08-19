/*
 * ProbEvidenceTest.scala
 * Probability of evidence computation tests.
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

import org.scalatest.Matchers
import org.scalatest.WordSpec
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.language._
import com.cra.figaro.library.atomic.continuous._
import com.cra.figaro.test._
import com.cra.figaro.util._
import com.cra.figaro.library.compound._
import com.cra.figaro.test.tags.Performance
import com.cra.figaro.test.tags.NonDeterministic

class ProbEvidenceTest extends WordSpec with Matchers {

  "Computing probability of evidence" when {

    "given a vanilla model with one condition" should {
      "return the probability the condition is satisfied" in {
        val universe = Universe.createNew()
        val f = Flip(0.7)("f", universe)
        sampleTest(0.7, List(NamedEvidence("f", Observation(true))))
      }

      "return the log probability the condition is satisfied" in {
        val universe = Universe.createNew()
        val f = Flip(0.7)("f", universe)
        logProbabilitySampleTest(Math.log(0.7), List(NamedEvidence("f", Observation(true))))
      }

    }

    "given a vanilla model with two independent conditions" should {
      "return the probability both conditions are satisfied" in {
        val universe = Universe.createNew()
        val f1 = Flip(0.7)("f1", universe)
        val f2 = Flip(0.4)("f2", universe)
        sampleTest(0.7 * 0.4, List(NamedEvidence("f1", Observation(true)), NamedEvidence("f2", Observation(true))))
      }

      "return the log probability both conditions are satisfied" taggedAs(NonDeterministic) in {
        val universe = Universe.createNew()
        val f1 = Flip(0.7)("f1", universe)
        val f2 = Flip(0.4)("f2", universe)
        logProbabilitySampleTest(Math.log(0.7 * 0.4), List(NamedEvidence("f1", Observation(true)), NamedEvidence("f2", Observation(true))))
      }
    }

    "given a vanilla mode with two dependent conditions" should {
      "return the probability both conditions are jointly satisfied" in {
        val universe = Universe.createNew()
        val d = Select(0.2 -> 0.6, 0.8 -> 0.9)
        val f1 = Flip(d)("f1", universe)
        val f2 = Flip(d)("f2", universe)
        sampleTest(0.2 * 0.6 * 0.6 + 0.8 * 0.9 * 0.9, List(NamedEvidence("f1", Observation(true)), NamedEvidence("f2", Observation(true))))
      }

      "return the log probability both conditions are jointly satisfied" in {
        val universe = Universe.createNew()
        val d = Select(0.2 -> 0.6, 0.8 -> 0.9)
        val f1 = Flip(d)("f1", universe)
        val f2 = Flip(d)("f2", universe)
        logProbabilitySampleTest(Math.log((0.2 * 0.6 * 0.6 + 0.8 * 0.9 * 0.9)), List(NamedEvidence("f1", Observation(true)), NamedEvidence("f2", Observation(true))))
      }

    }

    "given a vanilla model with two dependent conditions and a constraint" should {
      "return the probability both conditions are satisfied, taking into account the constraint" in {
        val universe = Universe.createNew()
        val d = Select(0.5 -> 0.6, 0.5 -> 0.9)("d", universe)
        d.setConstraint((d: Double) => if (d > 0.7) 0.8; else 0.2)
        val f1 = Flip(d)("f1", universe)
        val f2 = Flip(d)("f2", universe)
        sampleTest(0.2 * 0.6 * 0.6 + 0.8 * 0.9 * 0.9, List(NamedEvidence("f1", Observation(true)), NamedEvidence("f2", Observation(true))))
      }

      "return the log probability both conditions are satisfied, taking into account the constraint" in {
        val universe = Universe.createNew()
        val d = Select(0.5 -> 0.6, 0.5 -> 0.9)("d", universe)
        d.setConstraint((d: Double) => if (d > 0.7) 0.8; else 0.2)
        val f1 = Flip(d)("f1", universe)
        val f2 = Flip(d)("f2", universe)
        logProbabilitySampleTest(Math.log((0.2 * 0.6 * 0.6 + 0.8 * 0.9 * 0.9)), List(NamedEvidence("f1", Observation(true)), NamedEvidence("f2", Observation(true))))
      }

    }

    "given a constant whose condition is not satisfied" should {
      "return 0" in {
        val universe = Universe.createNew()
        val c = Constant(8)("c", universe)
        sampleTest(0, List(NamedEvidence("c", Observation(7))))
      }

      "return negative infinity for log probability" in {
        val universe = Universe.createNew()
        val c = Constant(8)("c", universe)
        logProbabilitySampleTest(Double.NegativeInfinity, List(NamedEvidence("c", Observation(7))))
      }

    }

    "given a simple dist with a condition on the result" should {
      "return the expectation over the clauses of the probability the result satisfies the condition" in {
        val universe = Universe.createNew()
        val d = Dist(0.3 -> Flip(0.6), 0.7 -> Flip(0.9))("d", universe)
        sampleTest(0.3 * 0.6 + 0.7 * 0.9, List(NamedEvidence("d", Observation(true))))
      }

      "return the expectation over the clauses of the log probability the result satisfies the condition" in {
        val universe = Universe.createNew()
        val d = Dist(0.3 -> Flip(0.6), 0.7 -> Flip(0.9))("d", universe)
        logProbabilitySampleTest(Math.log((0.3 * 0.6) + (0.7 * 0.9)), List(NamedEvidence("d", Observation(true))))
      }
    }

    "given a complex dist with a condition on the result" should {
      "return the expectation over the clauses of the probability the result satisfies the condition" in {
        val universe = Universe.createNew()
        val p1 = Select(0.2 -> 0.4, 0.8 -> 0.6)
        val p2 = Constant(0.4)
        val d = Dist(p1 -> Flip(0.6), p2 -> Flip(0.9))("d", universe)
        sampleTest(0.2 * (0.5 * 0.6 + 0.5 * 0.9) + 0.8 * (0.6 * 0.6 + 0.4 * 0.9), List(NamedEvidence("d", Observation(true))))
      }

      "return the expectation over the clauses of the log probability the result satisfies the condition" in {
        val universe = Universe.createNew()
        val p1 = Select(0.2 -> 0.4, 0.8 -> 0.6)
        val p2 = Constant(0.4)
        val d = Dist(p1 -> Flip(0.6), p2 -> Flip(0.9))("d", universe)
        logProbabilitySampleTest(Math.log((0.2 * (0.5 * 0.6 + 0.5 * 0.9)) + (0.8 * (0.6 * 0.6 + 0.4 * 0.9))), List(NamedEvidence("d", Observation(true))))
      }
    }

    "given a continuous uniform with a condition" should {
      "return the uniform probability of the condition" in {
        val universe = Universe.createNew()
        val u = Uniform(0.0, 1.0)("u", universe)
        val condition = (d: Double) => d < 0.4
        sampleTest(0.4, List(NamedEvidence("u", Condition(condition))))
      }

      "return the log of uniform probability of the condition" in {
        val universe = Universe.createNew()
        val u = Uniform(0.0, 1.0)("u", universe)
        val condition = (d: Double) => d < 0.4
        logProbabilitySampleTest(Math.log(0.4), List(NamedEvidence("u", Condition(condition))))
      }
    }

    "given a caching chain with a condition on the result" should {
      "return the expectation over the parent of the probability the result satisfies the condition" in {
        val universe = Universe.createNew()
        val p1 = Select(0.4 -> 0.3, 0.6 -> 0.9)
        val c = CachingChain(p1, (d: Double) => if (d < 0.4) Flip(0.3); else Flip(0.8))("c", universe)
        sampleTest(0.4 * 0.3 + 0.6 * 0.8, List(NamedEvidence("c", Observation(true))))
      }

      "return the expectation over the parent of the log probability the result satisfies the condition" in {
        val universe = Universe.createNew()
        val p1 = Select(0.4 -> 0.3, 0.6 -> 0.9)
        val c = CachingChain(p1, (d: Double) => if (d < 0.4) Flip(0.3); else Flip(0.8))("c", universe)
        logProbabilitySampleTest(Math.log(0.4 * 0.3 + 0.6 * 0.8), List(NamedEvidence("c", Observation(true))))
      }
    }

    "given a non-caching chain with a condition on the result" should {

      "return the expectation over the parent of the probability the result satisfies the condition" in {
        val universe = Universe.createNew()
        val p1 = Uniform(0.0, 1.0)
        val c = NonCachingChain(p1, (d: Double) => if (d < 0.4) Flip(0.3); else Flip(0.8))("c", universe)
        sampleTest(0.4 * 0.3 + 0.6 * 0.8, List(NamedEvidence("c", Observation(true))))
      }
      "return the expectation over the parent of the log probability the result satisfies the condition" in {
        val universe = Universe.createNew()
        val p1 = Uniform(0.0, 1.0)
        val c = NonCachingChain(p1, (d: Double) => if (d < 0.4) Flip(0.3); else Flip(0.8))("c", universe)
        //Ensure this line is correct.
        logProbabilitySampleTest(Math.log(0.4 * 0.3 + 0.6 * 0.8), List(NamedEvidence("c", Observation(true))))

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
        sampleTest(0.25, List(NamedEvidence("a", Condition(condition))))
      }

      "return the correct log probability of evidence in the result" taggedAs(NonDeterministic) in {
        val universe = Universe.createNew()
        val x = Constant(false)
        val y = Constant(false)
        val u1 = Uniform(0.0, 1.0)
        val u2 = Uniform(0.0, 2.0)
        val a = CachingChain(x, y, (x: Boolean, y: Boolean) => if (x || y) u1; else u2)("a", universe)
        def condition(d: Double) = d < 0.5
        logProbabilitySampleTest(Math.log(0.25), List(NamedEvidence("a", Condition(condition))))
      }
    }

  }

  "Anytime computing probability of evidence" should {
    "produce an answer after the algorithm has started" in {
      val universe = Universe.createNew()
      val f = Flip(0.3)("f", universe)
      val alg = ProbEvidenceSampler(200L, List(NamedEvidence("f", Observation(true))))
      alg.start()
      Thread.sleep(200L)
      alg.probEvidence should be(0.3 +- 0.01)
      alg.kill()
    }

    "sets evidence appropriately and cleans up after itself" in {
      val universe = Universe.createNew()
      val u = Uniform(0.0, 1.0)("u", universe)
      val f = Flip(0.7)("f", universe)
      val contingency0: Element.Contingency = List(Element.ElemVal(f, false))
      val contingency1: Element.Contingency = List(Element.ElemVal(f, true))
      u.addCondition((d: Double) => d <= 0.5, contingency0)
      u.addCondition((d: Double) => d > 0.5, contingency1)
      u.addConstraint((d: Double) => d, contingency0)
      u.addConstraint((d: Double) => 1 - d, contingency1)
      val uConditions = u.allConditions
      val uConstraints = u.allConstraints
      val alg = ProbEvidenceSampler(200L, List(NamedEvidence("f", Observation(true)), NamedEvidence("u", Observation(0.7))))
      alg.start()
      Thread.sleep(100)
      alg.stop()
      f.condition(false) should equal(false)
      f.condition(true) should equal(true)
      alg.kill()
      f.condition(false) should equal(true)
      f.condition(true) should equal(true)

    }
  }

  "Computing probability of evidence" should {
    "not suffer from memory leaks" taggedAs (Performance) in {
      Universe.createNew()
      val c = NonCachingChain(Uniform(0.2, 1.0), (d: Double) => Flip(d))
      val alg = ProbEvidenceSampler(1000000, List())
      alg.start
      alg.stop
      alg.kill
    }
  }

  def sampleTest(prob: Double, evidence: List[NamedEvidence[_]]) {
    ProbEvidenceSampler.computeProbEvidence(60000, evidence) should be(prob +- 0.01)
  }

  def logProbabilitySampleTest(logProb: Double, evidence: List[NamedEvidence[_]]) {
    val alg = ProbEvidenceSampler(60000, evidence)
    alg.start
    val result = alg.logProbEvidence
    alg.stop
    alg.kill
    result should be(logProb +- 0.01)

  }
}
