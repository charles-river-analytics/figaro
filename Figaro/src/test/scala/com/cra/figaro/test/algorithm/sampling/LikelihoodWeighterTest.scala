/*
 * LikelihoodWeighterTest.scala  
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
import com.cra.figaro.algorithm.sampling.Importance.Reject
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.language._
import com.cra.figaro.library.atomic.continuous._
import com.cra.figaro.library.atomic._
import com.cra.figaro.library.atomic.discrete.Binomial
import com.cra.figaro.library.compound._
import com.cra.figaro.test._
import com.cra.figaro.util.logSum
import JSci.maths.statistics._
import com.cra.figaro.test.tags.Performance
import com.cra.figaro.test.tags.NonDeterministic
import scala.language.reflectiveCalls
import org.scalatest.Matchers
import org.scalatest.{ PrivateMethodTester, WordSpec }
import com.cra.figaro.algorithm.sampling.LikelihoodWeighter
import com.cra.figaro.library.cache.NoCache
class LikelihoodWeighterTest extends WordSpec with Matchers with PrivateMethodTester {

  // Note, many test cases are covered under the Importance sampling tests
  val normalizer = 1.0 / math.sqrt(2.0 * math.Pi)

  "Running likelihood weighting" should {
    "Correctly build dependencies" in {
      Universe.createNew()
      val result1 = Flip(0.7)
      val result2 = Flip(0.5)

      val c1 = If(Flip(0.1), Flip(0.2), result1)
      val c2 = If(Flip(0.1), Flip(0.2), result2)
      val c3 = If(Flip(0.5), c1, c2)

      val lw = new LikelihoodWeighter(Universe.universe, new NoCache(Universe.universe))
      for { _ <- 0 until 500 } lw.traverse(List(), Universe.universe.activeElements, 0.0, scala.collection.mutable.Set())
      lw.dependencies.contains(result1) should equal(true)
      lw.dependencies.contains(result2) should equal(true)
    }

    "Undo the weight if a result element was sampled before its parent when the parent has an observation" in {
      Universe.createNew()
      val result1 = Normal(0, 1)
      val result2 = Normal(1, 1)
      val c = Chain(Constant(true), (b: Boolean) => if (b) result1 else result2)
      result1.addConstraint((d: Double) => Normal.density(1.0, 1.0, 1.0)(d))
      c.observe(0.0)

      val lw = new LikelihoodWeighter(Universe.universe, new NoCache(Universe.universe))
      val weight = lw.traverse(List(), List(result1, result2, c), 0.0, scala.collection.mutable.Set())
      val correct = result1.density(0.0) * Normal.density(1.0, 1, 1)(0)
      math.exp(weight) should be(correct +- .00001)
    }

    "Not have invalid states for a chain if sampled in the wrong order" in {
      Universe.createNew()
      val result1 = Normal(0, 1)
      val result2 = Normal(1, 1)
      val c = Chain(Constant(true), (b: Boolean) => if (b) result1 else result2)
      c.observe(0.0)
      val f = result1 ++ Constant(0.0)
      val lw = new LikelihoodWeighter(Universe.universe, new NoCache(Universe.universe))
      val weight = lw.traverse(List(), List(f, result1, result2, c), 0.0, scala.collection.mutable.Set())
      f.value should equal(0.0)
    }

    "Not have invalid states for a dist if sampled in the wrong order" in {
      Universe.createNew()
      val result1 = Normal(0, 1)
      val result2 = Normal(1, 1)
      val c = Dist(1.0 -> result1, 0.0 -> result2)
      c.observe(0.0)
      val f = result1 ++ Constant(0.0)
      val lw = new LikelihoodWeighter(Universe.universe, new NoCache(Universe.universe))
      val weight = lw.traverse(List(), List(f, result1, result2, c), 0.0, scala.collection.mutable.Set())
      f.value should equal(0.0)
    }

    "Not overflow the stack" in {
      def next(count: Int): Element[Boolean] = {
        if (count == 0) Constant(true)
        else Chain(Flip(0.5), (b: Boolean) => next(count - 1))
      }
      Universe.createNew()
      val start = next(2000)
      val lw = new LikelihoodWeighter(Universe.universe, new NoCache(Universe.universe))
      lw.computeWeight(List(start))
      //an[StackOverflowError] should not be thrownBy { lw.computeWeight(List(start)) }
    }

  }

}
