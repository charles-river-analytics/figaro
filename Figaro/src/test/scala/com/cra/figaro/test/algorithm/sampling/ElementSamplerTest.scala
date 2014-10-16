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

class ElementSamplerTest extends WordSpec with Matchers with PrivateMethodTester {

  val epsilon = 0.01
  val lambda = 0.05
  
  "Sampling a single element" should {
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
   
  }


  def sampleOneTest[T](target: Element[T], predicate: T => Boolean, prob: Double) {    
    val numTrials = SamplingUtil.computeNumberOfSamples(epsilon, lambda)
    val es = ElementSampler(target, numTrials)
    es.start
    val predProb = es.probability(target, predicate)
    predProb should be(prob +- epsilon)    
    es.kill
  }
  

  
}
