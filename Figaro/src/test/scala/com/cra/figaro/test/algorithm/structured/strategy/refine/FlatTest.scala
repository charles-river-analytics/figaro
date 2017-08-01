/*
 * FlatTest.scala
 * Tests for flat strategies.
 *
 * Created By:      William Kretschmer (kretsch@mit.edu)
 * Creation Date:   Nov 29, 2016
 *
 * Copyright 2016 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */
package com.cra.figaro.test.algorithm.structured.strategy.refine

import com.cra.figaro.algorithm.structured.strategy.refine._
import com.cra.figaro.algorithm.structured._
import com.cra.figaro.algorithm.structured.strategy.range.SamplingRanger
import com.cra.figaro.language._
import com.cra.figaro.library.atomic.continuous.{Normal, Uniform}
import com.cra.figaro.library.compound.If
import org.scalatest.{Matchers, WordSpec}

class FlatTest extends WordSpec with Matchers {
  "A top-down flat refining strategy" should {
    "update ranges to be consistent" in {
      Universe.createNew()
      val e1 = Normal(0.0, 1.0)
      val e2 = e1.map(_ + 1)
      val cc = new ComponentCollection
      val pr = new Problem(cc, List(e1, e2))
      val c1 = cc(e1)
      val c2 = cc(e2)
      c1.ranger.asInstanceOf[SamplingRanger[Double]].samplesPerIteration = 10
      new ExpansionStrategy(pr, pr.targetComponents).execute()
      FlatStrategy.topDown(cc, c1).execute()

      val c2ExpectedValues = c1.range.regularValues.map(_ + 1)
      c2.range.regularValues should equal(c2ExpectedValues)
      c2ExpectedValues should have size 20
    }

    "update from globals through Chain outcomes" in {
      Universe.createNew()
      val e1 = Flip(0.5)
      val e2 = Normal(0.0, 1.0)
      val e3 = If(e1, e2.map(_ + 1), Select(0.2 -> -1.0, 0.8 -> 1.0))
      val cc = new ComponentCollection
      val pr = new Problem(cc, List(e3))
      pr.add(e2)
      val c2 = cc(e2)
      val c3 = cc(e3)
      c2.ranger.asInstanceOf[SamplingRanger[Double]].samplesPerIteration = 10
      new ExpansionStrategy(pr, pr.targetComponents).execute()
      FlatStrategy.topDown(cc, c2).execute()

      val c3ExpectedValues = c2.range.regularValues.map(_ + 1) + (-1.0, 1.0)
      c3.range.regularValues should equal(c3ExpectedValues)
      c3ExpectedValues should have size 22
    }

    "update the factors where ranges changed" in {
      Universe.createNew()
      val e1 = Normal(0.0, 1.0)
      val e2 = e1.map(_ + 1)
      e2.addConstraint((d: Double) => d * d)
      val cc = new ComponentCollection
      val pr = new Problem(cc, List(e1, e2))
      val c1 = cc(e1)
      val c2 = cc(e2)
      c1.ranger.asInstanceOf[SamplingRanger[Double]].samplesPerIteration = 15
      new ExpansionStrategy(pr, pr.targetComponents).execute()
      FlatStrategy.topDown(cc, c1).execute()

      // Factor lists each over one variable; each factor should have size equal to the range of the variable
      // The range should have 30 values because we take 15 samples twice
      val singleVarFactors = List(c1.nonConstraintFactors(), c2.constraintFactors(Lower), c2.constraintFactors(Upper))
      for(list <- singleVarFactors) {
        list should have size 1
        list.head should have size 30
      }
      // The non-constraint factor for c2 is over two variables, each with range 30, giving size 30*30=900
      c2.nonConstraintFactors() should have size 1
      c2.nonConstraintFactors().head should have size 900
    }

    "expand, but not recurse on, Chain subproblems" in {
      Universe.createNew()
      val e1 = Normal(0.0, 1.0)
      val e2 = Normal(e1, 1.0)
      val cc = new ComponentCollection
      val pr = new Problem(cc, List(e1, e2))
      val c1 = cc(e1)
      val c2 = cc(e2)
      c1.ranger.asInstanceOf[SamplingRanger[Double]].samplesPerIteration = 10
      new ExpansionStrategy(pr, pr.targetComponents).execute()
      val initialSubproblems = c2.subproblems
      FlatStrategy.topDown(cc, c1).execute()

      val newSubproblems = c2.subproblems -- initialSubproblems.keySet
      // Make sure it creates the correct number of new subproblems
      newSubproblems should have size 10
      // No components in the new subproblems should be refined yet
      for((_, subproblem) <- newSubproblems ; comp <- subproblem.components) {
        comp.nonConstraintFactors() should be(empty)
        comp.range.regularValues should be(empty)
        comp.range.hasStar should equal(true)
      }
    }

    "stop at fully refined components" in {
      Universe.createNew()
      val e1 = Uniform(0.0, 1.0)
      val e2 = Flip(e1)
      val e3 = If(e2, 1.0, 2.0)
      val e4 = e1 ++ e3

      val cc = new ComponentCollection
      val pr = new Problem(cc, List(e4))
      new ExpansionStrategy(pr, pr.targetComponents).execute()
      val c1 = cc(e1)
      val c2 = cc(e2)
      val c4 = cc(e4)
      val strategy = FlatStrategy.topDown(cc, c1)
      strategy.execute()

      // e2 should be fully enumerated, which makes e3 fully refined
      // This should not stop e4 from getting refined, since it also uses e1 directly
      strategy.done should equal(Set(c1, c2, c4))
    }

    "ignore dependent elements not in the collection" in {
      Universe.createNew()
      val e1 = Uniform(0.0, 1.0)
      val e2 = Flip(e1)
      val cc = new ComponentCollection
      val pr = new Problem(cc, List(e1))
      // Because we start at e1, e2 should not get added to the collection here
      new ExpansionStrategy(pr, pr.targetComponents).execute()
      val c1 = cc(e1)
      FlatStrategy.topDown(cc, c1).execute()
      cc.contains(e2) should equal(false)
    }
  }
}
