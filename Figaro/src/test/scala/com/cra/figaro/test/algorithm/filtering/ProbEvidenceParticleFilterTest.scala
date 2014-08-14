/*
 * ProbEvidenceTest.scala
 * Probability of evidence computation tests.
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   April 15, 2014
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.test.algorithm.filtering;

import org.scalatest.Matchers
import org.scalatest.WordSpec
import com.cra.figaro.algorithm.filtering._
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.language._
import com.cra.figaro.library.atomic.continuous._
import com.cra.figaro.library.compound.If
import com.cra.figaro.test._
import com.cra.figaro.test.tags.Performance

class ProbEvidenceTest extends WordSpec with Matchers {
  
    "Computing probability of evidence" when {
    "given a vanilla model with one condition" should {
      "return the probability the condition is satisfied" in {
        val initial = Universe.createNew()
        val f = Flip(0.7)("f", initial)
        
        def trans(previousUniverse : Universe) : Universe = {
          val newU = Universe.createNew()
          val f = Flip(0.4)("f", newU)
          newU
        }
        val evidence = List(NamedEvidence("f", Observation(true)))
        sampleTest(0.4, initial, trans, 60000, evidence)
      } 
    }
    
    "given a vanilla model with one condition and 2 time steps" should {
      "return the probability the condition is satisfied" in {
        val initial = Universe.createNew()
        val f = Flip(0.7)("f", initial)
        
        def trans(previousUniverse : Universe) : Universe = {
          val newU = Universe.createNew()
          val f = Flip(0.4)("f", newU)
          newU
        }
        val evidence = List(NamedEvidence("f", Observation(true)))
        sampleTest2(0.4*0.4, initial, trans, 60000, evidence)
      } 
    }

    "given a vanilla model with two independent conditions" should {
      "return the probability both conditions are satisfied" in {
        val initial = Universe.createNew()
        val f1 = Flip(0.9)("f1", initial)
        val f2 = Flip(0.6)("f2", initial)
        
        def trans(previousUniverse : Universe) : Universe = {
          val newU = Universe.createNew()
          val f1 = Flip(0.5)("f1", newU)
          val f2 = Flip(0.5)("f2", newU)
          newU
        }
        val evidence = List(NamedEvidence("f1", Observation(true)), NamedEvidence("f2", Observation(true)))
        sampleTest(0.5 * 0.5, initial, trans, 60000, evidence) 
        //.25
      } 
    }

    "given a vanilla mode with two dependent conditions" should {
      "return the probability both conditions are jointly satisfied" in {
        val initial = Universe.createNew()
        val d = Select(0.5 -> 0.6, 0.5 -> 0.9)
        val f1 = Flip(d)("f1", initial)
        val f2 = Flip(d)("f2",  initial)
        
         def trans(previousUniverse : Universe) : Universe = {
          val newU = Universe.createNew()
          val d = Select(0.2 -> 0.6, 0.8 -> 0.9)
          val f1 = Flip(d)("f1", newU)
          val f2 = Flip(d)("f2", newU)
          newU
        }
        
        val evidence = List(NamedEvidence("f1", Observation(true)), NamedEvidence("f2", Observation(true)))
        sampleTest(0.2 * 0.6 * 0.6 + 0.8 * 0.9 * 0.9, initial, trans, 60000, evidence) 
        //.72
      } 
    }
    
       "given a vanilla mode with two dependent conditions and 2 time steps" should {
      "return the probability both conditions are jointly satisfied" in {
        val initial = Universe.createNew()
        val d = Select(0.5 -> 0.6, 0.5 -> 0.9)
        val f1 = Flip(d)("f1", initial)
        val f2 = Flip(d)("f2",  initial)
        
         def trans(previousUniverse : Universe) : Universe = {
          val newU = Universe.createNew()
          val d = Select(0.2 -> 0.6, 0.8 -> 0.9)
          val f1 = Flip(d)("f1", newU)
          val f2 = Flip(d)("f2", newU)
          newU
        }
        
        val evidence = List(NamedEvidence("f1", Observation(true)), NamedEvidence("f2", Observation(true)))
        sampleTest2((0.2 * 0.6 * 0.6 + 0.8 * 0.9 * 0.9) * (0.2 * 0.6 * 0.6 + 0.8 * 0.9 * 0.9), initial, trans, 60000, evidence) 
        //.72 * .72
      } 
    }

    "given a vanilla model with two dependent conditions and a constraint" should {
      "return the probability both conditions are satisfied, taking into account the constraint" in { 
        val initial = Universe.createNew()
        val d = Select(0.9 -> 0.6, 0.1 -> 0.9)("d", initial)
        val f1 = Flip(d)("f1", initial)
        val f2 = Flip(d)("f2", initial)
        
         def trans(previousUniverse : Universe) : Universe = {
          val newU = Universe.createNew()
          val d = Select(0.5 -> 0.6, 0.5 -> 0.9)("d", newU)
          d.setConstraint((d: Double) => if (d > 0.7) 0.8; else 0.2)
          val f1 = Flip(d)("f1", newU)
          val f2 = Flip(d)("f2", newU)
          newU
        }
        
        val evidence = List(NamedEvidence("f1", Observation(true)), NamedEvidence("f2", Observation(true)))
        sampleTest(0.2 * 0.6 * 0.6*0.5 + 0.8 * 0.9 * 0.9*0.5, initial, trans, 60000, evidence)
        //0.36
      }
    }
    
       "given a vanilla model with two dependent conditions and a constraint and 2 time steps" should { 
      "return the probability both conditions are satisfied, taking into account the constraint" in { 
        val initial = Universe.createNew()
        val d = Select(0.9 -> 0.6, 0.1 -> 0.9)("d", initial)
        val f1 = Flip(d)("f1", initial)
        val f2 = Flip(d)("f2", initial)
        
         def trans(previousUniverse : Universe) : Universe = {
          val newU = Universe.createNew()
          val d = Select(0.5 -> 0.6, 0.5 -> 0.9)("d", newU)
          d.setConstraint((d: Double) => if (d > 0.7) 0.8; else 0.2)
          val f1 = Flip(d)("f1", newU)
          val f2 = Flip(d)("f2", newU)
          newU
        }
        
        val evidence = List(NamedEvidence("f1", Observation(true)), NamedEvidence("f2", Observation(true)))
        sampleTest2((0.2 * 0.6 * 0.6*0.5 + 0.8 * 0.9 * 0.9*0.5) * (0.2 * 0.6 * 0.6*0.5 + 0.8 * 0.9 * 0.9*0.5), initial, trans, 60000, evidence)
        //0.36*0.36
      }
    }

    "given a constant whose condition is not satisfied" should {
      "return 0" in {
        val initial = Universe.createNew()
        val c = Constant(8)("c", initial)
        
          def trans(previousUniverse : Universe) : Universe = {
          val newU = Universe.createNew()
          val c = Constant(10)("c", newU)
          newU
        }
        
         val evidence =List(NamedEvidence("c", Observation(7))) 
        sampleTest(0, initial, trans, 60000, evidence)
      }
    }

    "given a simple dist with a condition on the result" should {
      "return the expectation over the clauses of the probability the result satisfies the condition" in {
        val initial = Universe.createNew()
        val d = Dist(0.3 -> Flip(0.5), 0.7 -> Flip(0.5))("d", initial )
           
        def trans(previousUniverse : Universe) : Universe = {
          val newU = Universe.createNew()
          val d = Dist(0.3 -> Flip(0.6), 0.7 -> Flip(0.9))("d", newU)
          newU
        }
        
        val evidence = List(NamedEvidence("d", Observation(true)))
        sampleTest(0.3 * 0.6 + 0.7 * 0.9, initial, trans, 60000, evidence)
        //.81
      } 
    }
    
       "given a simple dist with a condition on the result and 2 time steps" should {
      "return the expectation over the clauses of the probability the result satisfies the condition" in {
        val initial = Universe.createNew()
        val d = Dist(0.3 -> Flip(0.5), 0.7 -> Flip(0.5))("d", initial )
           
        def trans(previousUniverse : Universe) : Universe = {
          val newU = Universe.createNew()
          val d = Dist(0.3 -> Flip(0.6), 0.7 -> Flip(0.9))("d", newU)
          newU
        }
        
        val evidence = List(NamedEvidence("d", Observation(true)))
        sampleTest2((0.3 * 0.6 + 0.7 * 0.9) * (0.3 * 0.6 + 0.7 * 0.9), initial, trans, 60000, evidence)
        //.81 * .81
      } 
    }

    "given a complex dist with a condition on the result" should {
      "return the expectation over the clauses of the probability the result satisfies the condition" in {
        val initial = Universe.createNew()
        val p1 = Select(0.2 -> 0.4, 0.8 -> 0.6)("p1", initial)
        val p2 = Constant(0.4)("p2", initial)
        val d = Dist(p1 -> Flip(0.6), p2 -> Flip(0.9))("d", initial)
        
        def trans(previousUniverse : Universe) : Universe = {
          val newU = Universe.createNew()
          val p1 = previousUniverse.get[Double]("p1")
          val p2 = previousUniverse.get[Double]("p2")
          val d = Dist(p1 -> Flip(0.6), p2 -> Flip(0.9))("d", newU)
          newU
        }
        val evidence = List(NamedEvidence("d", Observation(true)))
        sampleTest(0.2 * (0.5 * 0.6 + 0.5 * 0.9) + 0.8 * (0.6 * 0.6 + 0.4 * 0.9), initial, trans, 60000, evidence)
        //.726
      } 
    }

    "given a continuous uniform with a condition" should {
      "return the uniform probability of the condition" in {
        val initial = Universe.createNew()
        val u = Uniform(1.0, 0.01)("u", initial)
        val condition = (d: Double) => d < 0.4
        
         def trans(previousUniverse : Universe) : Universe = {
          val newU = Universe.createNew()
          val u = Uniform(0.0, 1.0)("u", newU)
          newU
        }
        
        val evidence = List(NamedEvidence("u", Condition(condition)))
        sampleTest(0.4, initial, trans, 60000, evidence)
      }
    }
    
        "given a continuous uniform with a condition and 3 time steps" should {
      "return the uniform probability of the condition" in {
        val initial = Universe.createNew()
        val u = Uniform(1.0, 0.01)("u", initial)
        val condition = (d: Double) => d < 0.4
        
         def trans(previousUniverse : Universe) : Universe = {
          val newU = Universe.createNew()
          val u = Uniform(0.0, 1.0)("u", newU)
          newU
        }
        
        val evidence = List(NamedEvidence("u", Condition(condition)))
        sampleTest3(0.4 * 0.4 * 0.4, initial, trans, 60000, evidence)
      }
    }

    "given a caching chain with a condition on the result" should {
      "return the expectation over the parent of the probability the result satisfies the condition" in {
        val initial = Universe.createNew()
        val p1 = Select(0.4 -> 0.3, 0.6 -> 0.9)("p1", initial)
        val c = CachingChain(p1, (d: Double) => if (d < 0.4) Flip(0.3); else Flip(0.8))("c", initial)
        
         def trans(previousUniverse : Universe) : Universe = {
          val newU = Universe.createNew()
           val p1 = previousUniverse.get[Double]("p1")
           val c = CachingChain(p1, (d: Double) => if (d < 0.4) Flip(0.3); else Flip(0.8))("c", newU)
          newU
        }
        
        val evidence = List(NamedEvidence("c", Observation(true)))
        sampleTest(0.4 * 0.3 + 0.6 * 0.8, initial, trans, 60000, evidence)
        //.6
      } 
    }

    "given a non-caching chain with a condition on the result" should {
      "return the expectation over the parent of the probability the result satisfies the condition" in {
        val initial = Universe.createNew()
        val p1 = Uniform(0.0, 1.0)("p1", initial)
        val c = NonCachingChain(p1, (d: Double) => if (d < 0.4) Flip(0.3); else Flip(0.8))("c", initial)
        
        def trans(previousUniverse : Universe) : Universe = {
          val newU = Universe.createNew()
           val p1 = Uniform(0.0, 1.0)("p1", newU)
           val c = NonCachingChain(p1, (d: Double) => if (d < 0.4) Flip(0.3); else Flip(0.8))("c",  newU)
          newU
        }
        
        val evidence = List(NamedEvidence("c", Observation(true)))
        sampleTest(0.4 * 0.3 + 0.6 * 0.8, initial, trans, 60000, evidence)
        //.6
      }
    }

    "given a chain of two arguments whose result is a different element with a condition on the result" should {
      "return the correct probability of evidence in the result" in {
        val initial = Universe.createNew()
        val x = Constant(false)("x", initial)
        val y = Constant(false)("y", initial)
        val u1 = Uniform(0.0, 1.0)("u1", initial)
        val u2 = Uniform(0.0, 2.0)("u2", initial)
        val a = CachingChain(x, y, (x: Boolean, y: Boolean) => if (x || y) u1; else u2)("a", initial)
        def condition(d: Double) = d < 0.5
        
        def trans(previousUniverse : Universe) : Universe = {
          val newU = Universe.createNew()
          val x = previousUniverse.get[Boolean]("x")
          val y = previousUniverse.get[Boolean]("y")
          val u1 = previousUniverse.get[Double]("u1")
          val u2 = previousUniverse.get[Double]("u2")
          val a = CachingChain(x, y, (x: Boolean, y: Boolean) => if (x || y) u1; else u2)("a", newU)
          newU
        }
        
        val evidence = List(NamedEvidence("a", Condition(condition)))
        sampleTest(0.25, initial, trans, 60000, evidence)
      } 
    }
   
  }
   

  "Anytime computing probability of evidence" should {
    "produce an answer after the algorithm has started" in {
      val initial = Universe.createNew()
      val f = Flip(0.5)("f", initial)
      
      def trans(previousUniverse : Universe) : Universe = {
        val newU = Universe.createNew()
        val f = Flip(0.3)("f", newU)
        newU
      }
      
      val alg = ParticleFilter(initial, trans, 60000)
      alg.start()
      alg.advanceTime(List(NamedEvidence("f", Observation(true))))
      Thread.sleep(200L)
      alg.probEvidence should be (0.3 +- 0.01)
      alg.kill()
    }

  }

  "Computing probability of evidence" should {
    "not suffer from memory leaks" taggedAs (Performance) in {
      val initial = Universe.createNew()
      val c = NonCachingChain(Uniform(0.2, 1.0), (d: Double) => Flip(d))
      
      def trans(previousUniverse : Universe) : Universe = {
        val newU = Universe.createNew()
       val c = NonCachingChain(Uniform(0.2, 1.0), (d: Double) => Flip(d))
        newU
      }
      
      val alg = ParticleFilter(initial, trans, 6000)
      alg.start()
    } 
  }
    
  def sampleTest(prob: Double, initial: Universe, transition: Universe => Universe, numParticles: Int,  evidence: List[NamedEvidence[_]]) {
     val alg = ParticleFilter(initial, transition, numParticles)
     alg.start()
     alg.advanceTime(evidence)
     val probEvidence = alg.probEvidence	
     alg.stop()
     alg.kill()
     probEvidence should be (prob +- 0.01)
  }
  
    def sampleTest2(prob: Double, initial: Universe, transition: Universe => Universe, numParticles: Int,  evidence: List[NamedEvidence[_]]) {
     val alg = ParticleFilter(initial, transition, numParticles)
     alg.start()
     alg.advanceTime(evidence)
     alg.advanceTime(evidence)
     val probEvidence = alg.probEvidence	
     alg.stop()
     alg.kill()
     probEvidence should be (prob +- 0.01)
  }
    
     def sampleTest3(prob: Double, initial: Universe, transition: Universe => Universe, numParticles: Int,  evidence: List[NamedEvidence[_]]) {
     val alg = ParticleFilter(initial, transition, numParticles)
     alg.start()
     alg.advanceTime(evidence)
     alg.advanceTime(evidence)
     alg.advanceTime(evidence)
     val probEvidence = alg.probEvidence	
     alg.stop()
     alg.kill()
     probEvidence should be (prob +- 0.01)
  }
	

}
