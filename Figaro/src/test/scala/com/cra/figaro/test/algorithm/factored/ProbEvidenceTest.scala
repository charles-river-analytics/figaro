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

package com.cra.figaro.test.algorithm.factored

import org.scalatest.Matchers
import org.scalatest.WordSpec
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.language._
import com.cra.figaro.library.atomic.continuous._
import com.cra.figaro.test._
import com.cra.figaro.algorithm.factored.beliefpropagation.BeliefPropagation

class ProbEvidenceTest extends WordSpec with Matchers {

  "Computing probability of evidence" when {
    "given a vanilla model with one condition" should {
      "return the probability the condition is satisfied" in {
        val universe = Universe.createNew()
        val f = Flip(0.7)("f", universe)
        sampleTest(0.7, List(NamedEvidence("f", Observation(true))), universe)
      }
    }

    "given a vanilla model with two independent conditions" should {
      "return the probability both conditions are satisfied" in {
        val universe = Universe.createNew()
        val f1 = Flip(0.7)("f1", universe)
        val f2 = Flip(0.4)("f2", universe)
        sampleTest(0.7 * 0.4, List(NamedEvidence("f1", Observation(true)), NamedEvidence("f2", Observation(true))), universe)
      }
    }

    "given a vanilla mode with two dependent conditions" should {
      "return the probability both conditions are jointly satisfied" in {
        val universe = Universe.createNew()
        val d = Select(0.2 -> 0.6, 0.8 -> 0.9)
        val f1 = Flip(d)("f1", universe)
        val f2 = Flip(d)("f2", universe)
        sampleTest(0.2 * 0.6 * 0.6 + 0.8 * 0.9 * 0.9, List(NamedEvidence("f1", Observation(true)), NamedEvidence("f2", Observation(true))), universe)
      }
    }

    "given a vanilla model with two dependent conditions and a constraint" should {
      "return the probability both conditions are satisfied, taking into account the constraint" in {
        val universe = Universe.createNew()
        val d = Select(0.5 -> 0.6, 0.5 -> 0.9)("d", universe)
        val f1 = Flip(d)("f1", universe)
        val f2 = Flip(d)("f2", universe)
        sampleTest(0.2 * 0.6 * 0.6*0.5 + 0.8 * 0.9 * 0.9*0.5, 
            List(NamedEvidence("f1", Observation(true)), NamedEvidence("f2", Observation(true)), NamedEvidence("d", Constraint((d: Double) => if (d > 0.7) 0.8; else 0.2))), 
            universe)
      }
    }

    "given a constant whose condition is not satisfied" should {
      "return 0" in {
        val universe = Universe.createNew()
        val c = Constant(8)("c", universe)
        sampleTest(0, List(NamedEvidence("c", Observation(7))), universe)
      }
    }

    "given a simple dist with a condition on the result" should {
      "return the expectation over the clauses of the probability the result satisfies the condition" in {
        val universe = Universe.createNew()
        val d = Dist(0.3 -> Flip(0.6), 0.7 -> Flip(0.9))("d", universe)
        sampleTest(0.3 * 0.6 + 0.7 * 0.9, List(NamedEvidence("d", Observation(true))), universe)
      }
    }

    "given a complex dist with a condition on the result" should {
      "return the expectation over the clauses of the probability the result satisfies the condition" in {
        val universe = Universe.createNew()
        val p1 = Select(0.2 -> 0.4, 0.8 -> 0.6)
        val p2 = Constant(0.4)
        val d = Dist(p1 -> Flip(0.6), p2 -> Flip(0.9))("d", universe)
        sampleTest(0.2 * (0.5 * 0.6 + 0.5 * 0.9) + 0.8 * (0.6 * 0.6 + 0.4 * 0.9), List(NamedEvidence("d", Observation(true))), universe)
      }
    }


    "given a caching chain with a condition on the result" should {
      "return the expectation over the parent of the probability the result satisfies the condition" in {
        val universe = Universe.createNew()
        val p1 = Select(0.4 -> 0.3, 0.6 -> 0.9)
        val c = CachingChain(p1, (d: Double) => if (d < 0.4) Flip(0.3); else Flip(0.8))("c", universe)
        sampleTest(0.4 * 0.3 + 0.6 * 0.8, List(NamedEvidence("c", Observation(true))), universe)
      }
    }

  }

 

  def sampleTest(prob: Double, evidence: List[NamedEvidence[_]], universe: Universe) {
    universe.assertEvidence(evidence)
    val alg = BeliefPropagation(100, universe.activeElements.toList:_*)
    alg.start
    alg.computeEvidence should be(prob +- 0.01)
    
    alg.kill
  }
}
