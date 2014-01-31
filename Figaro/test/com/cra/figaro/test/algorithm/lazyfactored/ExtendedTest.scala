/*
 * ExtendedTest.scala
 * Test of extended values and value sets
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
import org.scalatest.matchers.ShouldMatchers
import com.cra.figaro.algorithm.lazyfactored._
import ValueSet._
import com.cra.figaro.language._
import com.cra.figaro.library.compound.If

class ExtendedTest extends WordSpec with ShouldMatchers {
  "Value sets" should {
    "have correct result when adding an extended value" in {
      val vs1 = withoutStar(Set(1,2,3))
      val vs2 = withStar(Set(1,2,3))
      val x1 = Regular(1)
      val x2 = Regular(4)
      val x3 = Star[Int]
      val r11 = vs1 + x1
      val r12 = vs1 + x2
      val r13 = vs1 + x3
      val r21 = vs2 + x1
      val r22 = vs2 + x2
      val r23 = vs2 + x3
      r11.regularValues should equal (Set(1,2,3))
      r11.hasStar should equal (false)
      r12.regularValues should equal (Set(1,2,3,4))
      r12.hasStar should equal (false)
      r13.regularValues should equal (Set(1,2,3))
      r13.hasStar should equal (true)
      r21.regularValues should equal (Set(1,2,3))
      r21.hasStar should equal (true)
      r22.regularValues should equal (Set(1,2,3,4))
      r22.hasStar should equal (true)
      r23.regularValues should equal (Set(1,2,3))
      r23.hasStar should equal (true)
    }
    
    "have correct result when unioning with another value set" in {
      val vs1 = withoutStar(Set(1,2,3))
      val vs2 = withoutStar(Set(1,4,5))
      val vs3 = withStar(Set(1,2,4))
      val vs4 = withStar(Set(3,4,6))
      val r12 = vs1 ++ vs2
      val r13 = vs1 ++ vs3
      val r42 = vs4 ++ vs2
      val r34 = vs3 ++ vs4
      r12.regularValues should equal (Set(1,2,3,4,5))
      r12.hasStar should equal (false)
      r13.regularValues should equal (Set(1,2,3,4))
      r13.hasStar should equal (true)
      r42.regularValues should equal (Set(1,3,4,5,6))
      r42.hasStar should equal (true)
      r34.regularValues should equal (Set(1,2,3,4,6))
      r34.hasStar should equal (true)
    }
    
    "have correct completeness" in {
      val vs1 = withoutStar(Set())
      val vs2 = withoutStar(Set(1,2,3))
      val vs3 = withStar(Set())
      val vs4 = withStar(Set(1,2,3))
      vs1.hasStar should equal (false)
      vs2.hasStar should equal (false)
      vs3.hasStar should equal (true)
      vs4.hasStar should equal (true)
    }
    
    "have map apply the function to every element while maintaining star nature" in {
      val vs1 = withoutStar(Set(1,2))
      val vs2 = withStar(Set(1,2))
      def f(i: Int) = i + 1
      val r1 = vs1.map(f)
      val r2 = vs2.map(f)
      r1.regularValues should equal (Set(2,3))
      r1.hasStar should equal (false)
      r2.regularValues should equal (Set(2,3))
      r2.hasStar should equal (true)
    }
  }
  /*
  "Extended variables" when {
    "constructed directly" should {
	    "contain the right range" in {
	      val vs1 = withoutStar(Set(1,2))
	      val vs2 = withStar(Set(1,2))
	      val var1 = new ExtendedVariable(vs1)
	      val var2 = new ExtendedVariable(vs2)
	      var1.range.toSet should equal (Set(Regular(1), Regular(2)))
	      var2.range.toSet should equal (Set(Star[Int](), Regular(1), Regular(2)))
	    }
    }
    
    "constructed from a simple element" should {
      "contain the right range" in {
        val universe = Universe.createNew()
        val elem1 = Flip(0.2)
        val var1 =  ExtendedVariable(elem1)
        var1.range.toSet should equal (Set(Regular(false), Regular(true)))
      }
    }
    
    "constructed from a chain whose range has already been computed" should {
      "contain the right range" in {
        val universe = Universe.createNew()
        val elem1 = If(Flip(0.2), Flip(0.3), Flip(0.4))
        LazyValues(universe)(elem1, 1)
        val var1 = ExtendedVariable(elem1)
        var1.range.toSet should equal (Set(Regular(false), Regular(true)))
      }
    }

    "constructed from a chain whose range has not yet been computed" should {
      "contain a range with only Star" in {
        val universe = Universe.createNew()
        val elem1 = If(Flip(0.2), Flip(0.3), Flip(0.4))
        val var1 = ExtendedVariable(elem1)
        var1.range.toSet should equal (Set(Star[Boolean]()))
      }
    }
    
 }*/
}