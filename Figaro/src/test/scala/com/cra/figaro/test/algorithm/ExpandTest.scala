/*
 * ExpandTest.scala 
 * Expansion tests.
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Jan 1, 2009
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.test.algorithm

import org.scalatest.Matchers
import org.scalatest.WordSpec
import com.cra.figaro.algorithm._
import com.cra.figaro.language._

  /* OBSOLETE: Need to replace it with tests of getMap lazily growing
class ExpandTest extends WordSpec with Matchers {
  "After calling Expand.expand" should {
    "have all the result elements of a chain's function applied to its arguments be active" in {
      Universe.createNew()
      val c = CachingChain(Flip(0.2), (b: Boolean) => Constant(b))
      Expand()
      assert(Universe.universe.activeElements exists (isConstant(_, false)))
      assert(Universe.universe.activeElements exists (isConstant(_, true)))
    }

    "have all intermediate elements created by applying a chain's function be active" in {
      Universe.createNew()
      def f(b: Boolean) = Apply(Constant(b), (b: Boolean) => b)
      val c = CachingChain(Flip(0.2), f)
      Expand(Integer.MAX_VALUE)
      assert(Universe.universe.activeElements exists (isConstant(_, false)))
      assert(Universe.universe.activeElements exists (isConstant(_, true)))
    }

    "have elements recursively created a chain created by a chain be active" in {
      Universe.createNew()
      def f(d: Double) = CachingChain(Flip(d), (b: Boolean) => if (b) Constant(1); else Constant(2))
      val c = CachingChain(Constant(0.5), f)
      Expand(Integer.MAX_VALUE)
      assert(Universe.universe.activeElements exists (isConstant(_, 1)))
      assert(Universe.universe.activeElements exists (isConstant(_, 2)))
    }

    "associate a top-level chain with a map from its argument values to their associated result elements" in {
      Universe.createNew()
      val c = CachingChain(Flip(0.2), (b: Boolean) => Constant(b))
      Expand(Integer.MAX_VALUE)
      assert(hasBooleanConstantMap(c, false, true))
    }

    "associate a recursively defined chain with a map from its argument values " +
      "to their associated result elements" in {
        Universe.createNew()
        def f(d: Double) = CachingChain(Flip(d), (b: Boolean) => if (b) Constant(1); else Constant(2))
        val c = CachingChain(Constant(0.5), f)
        Expand(Integer.MAX_VALUE)
        val c2 = Expand().getMap(c)(0.5).asInstanceOf[Chain[Boolean, Int]]
        assert(hasBooleanConstantMap(c2, 2, 1))
      }
  }
  // Tests whether the element is a constant with the given value
  def isConstant[T](elem: Element[_], value: T): Boolean =
    elem match {
      case c: Constant[_] => c.constant == value
      case _ => false
    }

  // Tests whether the chain is associated with a map that maps Booleans to constants with the given values
  def hasBooleanConstantMap[T](chain: Chain[Boolean, T], falseValue: T, trueValue: T): Boolean = {
    val map = Expand(Universe.universe).getMap(chain)
    isConstant(map(false), falseValue) && isConstant(map(true), trueValue)
  }

}
*/