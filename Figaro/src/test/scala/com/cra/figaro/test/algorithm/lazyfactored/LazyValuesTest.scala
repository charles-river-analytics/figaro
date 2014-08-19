/*
 * LazyValuesTest.scala
 * Lazy range computation tests.
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Dec 27, 2013
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.test.algorithm.lazyfactored

import org.scalatest.WordSpec
import com.cra.figaro.algorithm.lazyfactored._
import com.cra.figaro.language._
import com.cra.figaro.library.atomic._
import com.cra.figaro.library.compound._
import com.cra.figaro.library.atomic._
import org.scalatest.Matchers

/*
 * The tests for unbounded depth values computation are in com.cra.figaro.test.algorithm.ValuesTest.
 */
class LazyValuesTest extends WordSpec with Matchers {
  "Calling values()" should {
    "given a non-enumerable element such as a continuous Uniform" should {
      "return the value set consisting only of *" in {
        val universe = Universe.createNew()
        val vs = LazyValues(universe)(continuous.Uniform(0.0, 1.0), 1)
        vs.regularValues should equal (Set())
        vs.hasStar should equal (true)
      }
    }

    "use the old result when called twice on the same complete element" in {
      val universe = Universe.createNew()
      var a = 0
      def f(x: Int) = { a += 1; x }
      val elem1 = Apply(Constant(2), f)
      val elem2 = Apply(Constant(3), f)
      a = 0
      val values = LazyValues(universe)
      values(elem1, 1)
      values(elem1, 2)
      values(elem2, 1)
      a should equal (2)
    }
    
    "use the old result when called twice on the same incomplete element with the same or lesser depth" in {
      /*
       * This test is tricky. We want to count the number of times the function inside the chain is called while computing
       * values. It must be a non-caching chain, since a caching chain would memoize the function anyway even if values
       * wasn't working correctly, so it wouldn't have a good test. The parent must have at least two values, because even
       * a non-caching chain does not recompute the function if the parent value hasn't changed. So, we make the parent
       * have two values. The final trick is to make sure that the first parent value is the current value when values is
       * called on the chain, so we know the function is not recomputed the first time, and is only recomputed for the
       * second parent value. That way, we can be sure the function should only be called once in the first call to
       * values. Since values is memoized, the function should not be called at all in subsequent times, so the total
       * number of times it is called should be 1.
       */
      val universe = Universe.createNew()
      var a = 0
      val parent = Select(0.5 -> 2, 0.5 -> 5)
      val elem1 = NonCachingChain(parent, (i: Int) => {
        a += 1
        Chain(Constant(i + 1), (j: Int) => Constant(j + 1))
      })
      parent.set(2)
      a = 0
      val values = LazyValues(universe)
      values(elem1, 1)
      values(elem1, 1)
      values(elem1, 0)
      a should equal (1)
    }
    
    "use the old result when called twice on the same universe" in {
      val universe = Universe.createNew()
      var a = 0
      def f(x: Int) = { a += 1; x }
      val elem1 = Apply(Constant(2), f)
      val elem2 = Apply(Constant(3), f)
      a = 0
      val values = LazyValues(universe)
      LazyValues(universe)(elem1, 1)
      LazyValues(universe)(elem1, 1)
      LazyValues(universe)(elem2, 1)
      a should equal (2)
    }
    
    "not compute values of unneeded elements" in {
      val universe = Universe.createNew()
      val e1 = Flip(0.1)
      var a = 0
      val e2 = Apply(e1, (b: Boolean) => { a += 1; b })
      a = 0
      val values = LazyValues(universe)
      values(e1, 1)
      a should equal (0)
    }
    
    "with a bounded depth expansion whose depth is less than the depth of the model produce correct starred results" in {
      val universe = Universe.createNew()
      val flip1 = Flip(0.1)
      val flip2 = Flip(0.2)
      val uniform1 = discrete.Uniform(1,2)
      val uniform2 = discrete.Uniform(2,3)
      val uniform3 = discrete.Uniform(3,5)
      val c1 = Chain(flip2, (b: Boolean) => if (b) uniform2; else uniform3)
      val c2 = Chain(flip1, (b: Boolean) => if (b) uniform1; else c1)
      val lv = LazyValues(universe)(c2, 1)
      lv.regularValues should equal (Set(1,2))
      lv.hasStar should equal (true)
    }

    "with a bounded depth expansion not expand unreached elements" in {
      val universe = Universe.createNew()
      val flip1 = Flip(0.1)
      val flip2 = Flip(0.2)
      val uniform1 = discrete.Uniform(1,2)
      val uniform2 = discrete.Uniform(2,3)
      var a = 0
      val apply1 = Apply(discrete.Uniform(3,5), (i: Int) => { a += 1; i })
      val c1 = Chain(flip2, (b: Boolean) => if (b) uniform2; else apply1)
      val c2 = Chain(flip1, (b: Boolean) => if (b) uniform1; else c1)
      a = 0
      LazyValues(universe)(c2, 1)
      a should equal (0)
    }
    
    "use the maximum depth values for all elements, even when query elements use them at different depths" in {
      /*
       * This test is meant to catch a subtle case that occurs in lazy values computation.
       * Suppose we have two query elements X and Y that both depend on an element Z.
       * Suppose that X depend on Z directly, while Y depends on Z indirectly.
       * If Y is expanded first, there is a danger that it will use a lesser depth of values of Z than X,
       * which can result in inconsistencies down the road.
       * It is necessary for lazy values computation to make sure that the same depth of all elements is
       * used consistently.
       */
      val universe = Universe.createNew()
      val select1 = Select(0.1 -> 1, 0.9 -> 2)
      val select2 = Select(0.2 -> 3, 0.8 -> 4)
      val apply1 = Apply(select1, (i: Int) => i + 1) // range is { 2, 3 }
      val apply2 = Apply(select2, (i: Int) => i + 1) // range is { 4, 5 }
      val x = Dist(0.3 -> apply1, 0.7 -> select2) // range should be { 2, 3, 4 }
      val y = Dist(0.4 -> select1, 0.6 -> apply2) // range should be { 1, 2, 4, 5 }
      val values = LazyValues(universe)
      values.expandAll(Set((x, 1), (y, 1)))
      values.storedValues(x).xvalues should equal (Set(Regular(2), Regular(3), Regular(4)))
      values.storedValues(y).xvalues should equal (Set(Regular(1), Regular(2), Regular(4), Regular(5)))
      
      
    }
    
    "not include elements from other universes" in {
      Universe.createNew()
      val v1 = Flip(0.5)
      Universe.createNew()
      val v2 = Constant(v1)
      
      val lv = LazyValues()
      lv.expandAll(Set((v2, Int.MaxValue)))
      lv.storedValues(v1).xvalues should be(empty)
    }
  }
}