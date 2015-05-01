/*
 * ProbEvidenceSamplingTest.scala
 * Probability of evidence sampling tests.
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
import com.cra.figaro.ndtest._

class ProbEvidenceSamplingTest extends WordSpec with Matchers {
  
  val alpha: Double = 0.05

  "Computing probability of evidence" when {

    "given a vanilla model with one condition" should {
      "return the probability the condition is satisfied" taggedAs (NonDeterministic) in {
         val ndtest = new NDTest {
            override def oneTest = {
              val target = 0.7
              val universe = Universe.createNew()
              val f = Flip(target)("f", universe)
              val result = sampleTest(target, List(NamedEvidence("f", Observation(true))))
              update(result, NDTest.TTEST, "SampleTestResults", target, alpha)
            }
          }

        ndtest.run(10)
      }

      "return the log probability the condition is satisfied" taggedAs (NonDeterministic) in {
        val ndtest = new NDTest {
          override def oneTest = {
            val value = 0.7
            val target = Math.log(value)
            val universe = Universe.createNew()
            val f = Flip(value)("f", universe)
            val result = logProbabilitySampleTest(target, List(NamedEvidence("f", Observation(true))))
            update(result, NDTest.TTEST, "LogProbabilitySampleTestResults", target, alpha)
          }
        }

        ndtest.run(10)
      }

    }

    "given a vanilla model with two independent conditions" should {
      "return the probability both conditions are satisfied" taggedAs (NonDeterministic) in {
        val ndtest = new NDTest {
          override def oneTest = {
            val value1 = 0.7
            val value2 = 0.4
            val target = value1 * value2
            val universe = Universe.createNew()
            val f1 = Flip(value1)("f1", universe)
            val f2 = Flip(value2)("f2", universe)
            val result = sampleTest(target, List(NamedEvidence("f1", Observation(true)), NamedEvidence("f2", Observation(true))))
            update(result, NDTest.TTEST, "SampleTestResults", target, alpha)
          }
        }

        ndtest.run(10)
      }

      "return the log probability both conditions are satisfied" taggedAs(NonDeterministic) in {
        val ndtest = new NDTest {
          override def oneTest = {
            val value1 = 0.7
            val value2 = 0.4
            val target = Math.log((value1 * value2))
            val universe = Universe.createNew()
            val f1 = Flip(value1)("f1", universe)
            val f2 = Flip(value2)("f2", universe)
            val result = logProbabilitySampleTest(target, List(NamedEvidence("f1", Observation(true)), NamedEvidence("f2", Observation(true))))
            update(result, NDTest.TTEST, "LogProbabilitySampleTestResults", target, alpha)
          }
        }

        ndtest.run(10)        
      }
    }

    "given a vanilla model with two dependent conditions" should {
      "return the probability both conditions are jointly satisfied" taggedAs (NonDeterministic) in {
        val ndtest = new NDTest {
          override def oneTest = {
            val target = 0.2 * 0.6 * 0.6 + 0.8 * 0.9 * 0.9
            val universe = Universe.createNew()
            val d = Select(0.2 -> 0.6, 0.8 -> 0.9)
            val f1 = Flip(d)("f1", universe)
            val f2 = Flip(d)("f2", universe)
            val result = sampleTest(target, List(NamedEvidence("f1", Observation(true)), NamedEvidence("f2", Observation(true))))
            update(result, NDTest.TTEST, "SampleTestResults", target, alpha)
          }
        }

        ndtest.run(10)
      }

      "return the log probability both conditions are jointly satisfied" taggedAs (NonDeterministic) in {
        val ndtest = new NDTest {
          override def oneTest = {
            val target = Math.log((0.2 * 0.6 * 0.6 + 0.8 * 0.9 * 0.9))
            val universe = Universe.createNew()
            val d = Select(0.2 -> 0.6, 0.8 -> 0.9)
            val f1 = Flip(d)("f1", universe)
            val f2 = Flip(d)("f2", universe)
            val result = logProbabilitySampleTest(target, List(NamedEvidence("f1", Observation(true)), NamedEvidence("f2", Observation(true))))
            update(result, NDTest.TTEST, "LogProbabilitySampleTestResults", target, alpha)
          }
        }

        ndtest.run(10)        
      }

    }

    "given a vanilla model with two dependent conditions and a constraint" should {
      "return the probability both conditions are satisfied, taking into account the constraint" taggedAs (NonDeterministic) in {
        val ndtest = new NDTest {
          override def oneTest = {
            val target = 0.2 * 0.6 * 0.6 + 0.8 * 0.9 * 0.9
            val universe = Universe.createNew()
            val d = Select(0.5 -> 0.6, 0.5 -> 0.9)("d", universe)
            d.setConstraint((d: Double) => if (d > 0.7) 0.8; else 0.2)
            val f1 = Flip(d)("f1", universe)
            val f2 = Flip(d)("f2", universe)
            val result = sampleTest(target, List(NamedEvidence("f1", Observation(true)), NamedEvidence("f2", Observation(true))))
            update(result, NDTest.TTEST, "SampleTestResults", target, alpha)
          }
        }

        ndtest.run(10)      
      }

      "return the log probability both conditions are satisfied, taking into account the constraint" taggedAs (NonDeterministic) in {
        val ndtest = new NDTest {
          override def oneTest = {
            val target = Math.log((0.2 * 0.6 * 0.6 + 0.8 * 0.9 * 0.9))
            val universe = Universe.createNew()
            val d = Select(0.5 -> 0.6, 0.5 -> 0.9)("d", universe)
            d.setConstraint((d: Double) => if (d > 0.7) 0.8; else 0.2)
            val f1 = Flip(d)("f1", universe)
            val f2 = Flip(d)("f2", universe)
            val result = logProbabilitySampleTest(target, List(NamedEvidence("f1", Observation(true)), NamedEvidence("f2", Observation(true))))
            update(result, NDTest.TTEST, "LogProbabilitySampleTestResults", target, alpha)
          }
        }

        ndtest.run(10)        
      }

    }

    "given a constant whose condition is not satisfied" should {
      "return 0" taggedAs (NonDeterministic) in {
        val ndtest = new NDTest {
          override def oneTest = {
            val target = 0.0
            val universe = Universe.createNew()
            val c = Constant(8)("c", universe)
            val result = sampleTest(target, List(NamedEvidence("c", Observation(7))))
            update(result, NDTest.TTEST, "SampleTestResults", target, alpha)
          }
        }

        ndtest.run(10)      
      }

      "return negative infinity for log probability" taggedAs (NonDeterministic) in {
        val ndtest = new NDTest {
          override def oneTest = {
            val target = Double.NegativeInfinity
            val universe = Universe.createNew()
            val c = Constant(8)("c", universe)
            val result = logProbabilitySampleTest(target, List(NamedEvidence("c", Observation(7))))
            update(result, NDTest.TTEST, "LogProbabilitySampleTestResults", target, alpha)
          }
        }

        ndtest.run(10)      
      }

    }

    "given a simple dist with a condition on the result" should {
      "return the expectation over the clauses of the probability the result satisfies the condition" taggedAs (NonDeterministic) in {
        val ndtest = new NDTest {
          override def oneTest = {
            val target = 0.3 * 0.6 + 0.7 * 0.9
            val universe = Universe.createNew()
            val d = Dist(0.3 -> Flip(0.6), 0.7 -> Flip(0.9))("d", universe)
            val result = sampleTest(target, List(NamedEvidence("d", Observation(true))))
            update(result, NDTest.TTEST, "SampleTestResults", target, alpha)
          }
        }

        ndtest.run(10)      
      }

      "return the expectation over the clauses of the log probability the result satisfies the condition" taggedAs (NonDeterministic) in {
        val ndtest = new NDTest {
          override def oneTest = {
            val target = Math.log((0.3 * 0.6) + (0.7 * 0.9))
            val universe = Universe.createNew()
            val d = Dist(0.3 -> Flip(0.6), 0.7 -> Flip(0.9))("d", universe)
            val result = logProbabilitySampleTest(target, List(NamedEvidence("d", Observation(true))))
            update(result, NDTest.TTEST, "LogProbabilitySampleTestResults", target, alpha)
          }
        }

        ndtest.run(10)
      }
    }

    "given a complex dist with a condition on the result" should {
      "return the expectation over the clauses of the probability the result satisfies the condition" taggedAs (NonDeterministic) in {
        val ndtest = new NDTest {
          override def oneTest = {
            val target = 0.2 * (0.5 * 0.6 + 0.5 * 0.9) + 0.8 * (0.6 * 0.6 + 0.4 * 0.9)
            val universe = Universe.createNew()
            val p1 = Select(0.2 -> 0.4, 0.8 -> 0.6)
            val p2 = Constant(0.4)
            val d = Dist(p1 -> Flip(0.6), p2 -> Flip(0.9))("d", universe)
            val result = sampleTest(target, List(NamedEvidence("d", Observation(true))))
            update(result, NDTest.TTEST, "SampleTestResults", target, alpha)
          }
        }

        ndtest.run(10)      
      }

      "return the expectation over the clauses of the log probability the result satisfies the condition" taggedAs (NonDeterministic) in {
        val ndtest = new NDTest {
          override def oneTest = {
            val target = Math.log((0.2 * (0.5 * 0.6 + 0.5 * 0.9)) + (0.8 * (0.6 * 0.6 + 0.4 * 0.9)))
            val universe = Universe.createNew()
            val p1 = Select(0.2 -> 0.4, 0.8 -> 0.6)
            val p2 = Constant(0.4)
            val d = Dist(p1 -> Flip(0.6), p2 -> Flip(0.9))("d", universe)
            val result = logProbabilitySampleTest(target, List(NamedEvidence("d", Observation(true))))
            update(result, NDTest.TTEST, "LogProbabilitySampleTestResults", target, alpha)
          }
        }

        ndtest.run(10)
      }
    }

    "given a continuous uniform with a condition" should {
      "return the uniform probability of the condition" taggedAs (NonDeterministic) in {
        val ndtest = new NDTest {
          override def oneTest = {
            val target = 0.4
            val universe = Universe.createNew()
            val u = Uniform(0.0, 1.0)("u", universe)
            val condition = (d: Double) => d < target
            val result = sampleTest(target, List(NamedEvidence("u", Condition(condition))))
            update(result, NDTest.TTEST, "SampleTestResults", target, alpha)
          }
        }

        ndtest.run(10)      
      }

      "return the log of uniform probability of the condition" taggedAs (NonDeterministic) in {
        val ndtest = new NDTest {
          override def oneTest = {
            val value = 0.4
            val target = Math.log(value)
            val universe = Universe.createNew()
            val u = Uniform(0.0, 1.0)("u", universe)
            val condition = (d: Double) => d < value
            val result = logProbabilitySampleTest(target, List(NamedEvidence("u", Condition(condition))))
            update(result, NDTest.TTEST, "LogProbabilitySampleTestResults", target, alpha)
          }
        }

        ndtest.run(10)
      }
    }

    "given a caching chain with a condition on the result" should {
      "return the expectation over the parent of the probability the result satisfies the condition" taggedAs (NonDeterministic) in {
        val ndtest = new NDTest {
          override def oneTest = {
            val target = 0.4 * 0.3 + 0.6 * 0.8
            val universe = Universe.createNew()
            val p1 = Select(0.4 -> 0.3, 0.6 -> 0.9)
            val c = CachingChain(p1, (d: Double) => if (d < 0.4) Flip(0.3); else Flip(0.8))("c", universe)
            val result = sampleTest(target, List(NamedEvidence("c", Observation(true))))
            update(result, NDTest.TTEST, "SampleTestResults", target, alpha)
          }
        }

        ndtest.run(10)      
      }

      "return the expectation over the parent of the log probability the result satisfies the condition" taggedAs (NonDeterministic) in {
        val ndtest = new NDTest {
          override def oneTest = {
            val target = Math.log(0.4 * 0.3 + 0.6 * 0.8)
            val universe = Universe.createNew()
            val p1 = Select(0.4 -> 0.3, 0.6 -> 0.9)
            val c = CachingChain(p1, (d: Double) => if (d < 0.4) Flip(0.3); else Flip(0.8))("c", universe)
            val result = logProbabilitySampleTest(target, List(NamedEvidence("c", Observation(true))))
            update(result, NDTest.TTEST, "LogProbabilitySampleTestResults", target, alpha)
          }
        }

        ndtest.run(10)      
      }
    }

    "given a non-caching chain with a condition on the result" should {

      "return the expectation over the parent of the probability the result satisfies the condition" taggedAs (NonDeterministic) in {
        val ndtest = new NDTest {
          override def oneTest = {
            val target = 0.4 * 0.3 + 0.6 * 0.8
            val universe = Universe.createNew()
            val p1 = Uniform(0.0, 1.0)
            val c = NonCachingChain(p1, (d: Double) => if (d < 0.4) Flip(0.3); else Flip(0.8))("c", universe)
            val result = sampleTest(target, List(NamedEvidence("c", Observation(true))))
            update(result, NDTest.TTEST, "SampleTestResults", target, alpha)
          }
        }

        ndtest.run(10)      
      }
      
      "return the expectation over the parent of the log probability the result satisfies the condition" taggedAs (NonDeterministic) in {
        val ndtest = new NDTest {
          override def oneTest = {
            val target = Math.log(0.4 * 0.3 + 0.6 * 0.8)
            val universe = Universe.createNew()
            val p1 = Uniform(0.0, 1.0)
            val c = NonCachingChain(p1, (d: Double) => if (d < 0.4) Flip(0.3); else Flip(0.8))("c", universe)
            val result = logProbabilitySampleTest(target, List(NamedEvidence("c", Observation(true))))
            update(result, NDTest.TTEST, "LogProbabilitySampleTestResults", target, alpha)
          }
        }

        ndtest.run(10)
      }

    }

    "given a chain of two arguments whose result is a different element with a condition on the result" should {
      "return the correct probability of evidence in the result" taggedAs (NonDeterministic) in {
        val ndtest = new NDTest {
          override def oneTest = {
            val target = 0.25
            val universe = Universe.createNew()
            val x = Constant(false)
            val y = Constant(false)
            val u1 = Uniform(0.0, 1.0)
            val u2 = Uniform(0.0, 2.0)
            val a = CachingChain(x, y, (x: Boolean, y: Boolean) => if (x || y) u1; else u2)("a", universe)
            def condition(d: Double) = d < 0.5
            val result = sampleTest(target, List(NamedEvidence("a", Condition(condition))))
            update(result, NDTest.TTEST, "SampleTestResults", target, alpha)
          }
        }

        ndtest.run(10)
      }

      "return the correct log probability of evidence in the result" taggedAs (NonDeterministic) in {
        val ndtest = new NDTest {
          override def oneTest = {
            val target = Math.log(0.25)
            val universe = Universe.createNew()
            val x = Constant(false)
            val y = Constant(false)
            val u1 = Uniform(0.0, 1.0)
            val u2 = Uniform(0.0, 2.0)
            val a = CachingChain(x, y, (x: Boolean, y: Boolean) => if (x || y) u1; else u2)("a", universe)
            def condition(d: Double) = d < 0.5
            val result = logProbabilitySampleTest(target, List(NamedEvidence("a", Condition(condition))))
            update(result, NDTest.TTEST, "LogProbabilitySampleTestResults", target, alpha)
          }
        }

        ndtest.run(10)
      }
    }

  }

  "Anytime computing probability of evidence" should {
    "produce an answer after the algorithm has started" taggedAs (NonDeterministic) in {
      val ndtest = new NDTest {
        override def oneTest = {
          val target = 0.3
          val universe = Universe.createNew()
          val f = Flip(target)("f", universe)
          val alg = ProbEvidenceSampler(200L, List(NamedEvidence("f", Observation(true))))
          alg.start()
          Thread.sleep(200L)
          val result = alg.probEvidence
          alg.kill()
          update(result, NDTest.TTEST, "ProbabilityOfEvidenceTestResults", target, alpha)
        }
      }

      ndtest.run(10)    
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

  def sampleTest(prob: Double, evidence: List[NamedEvidence[_]]): Double = {
    ProbEvidenceSampler.computeProbEvidence(60000, evidence)
  }

  def logProbabilitySampleTest(logProb: Double, evidence: List[NamedEvidence[_]]): Double = {
    val alg = ProbEvidenceSampler(60000, evidence)
    alg.start
    val result = alg.logProbEvidence
    alg.stop
    alg.kill
    result
  }
}
