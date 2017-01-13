/*
 * ExpandTest.scala
 * Test of SFI expansion methods.
 *
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   March 1, 2015
 *
 * Copyright 2015 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */
package com.cra.figaro.test.algorithm.structured

import org.scalatest.{ WordSpec, Matchers }
import com.cra.figaro.algorithm.structured.ComponentCollection
import com.cra.figaro.language._
import com.cra.figaro.library.atomic.discrete.Uniform
import com.cra.figaro.library.collection.MakeArray
import com.cra.figaro.algorithm.structured._
import com.cra.figaro.language.Name.stringToName

class ExpandTest extends WordSpec with Matchers {
  "Expanding a chain" should {
    "at one parent value, create the subproblem whose target is the result element" in {
      Universe.createNew()
      val cc = new ComponentCollection
      val pr = new Problem(cc)
      val e1 = Flip(0.2)
      val e2 = Constant(1)
      val e3 = Uniform(2, 3)
      def f(b: Boolean) = if (b) e2 else e3
      val e4 = Chain(e1, f)
      pr.add(e4)
      val c4 = cc(e4)
      c4.expand(false)

      c4.subproblems.size should equal(1)
      c4.subproblems(false).target should equal(e3)
    }

    "when repeating a parent value, not create a new expansion" in {
      Universe.createNew()
      val cc = new ComponentCollection
      val pr = new Problem(cc)
      val e1 = Flip(0.2)
      val e2 = Constant(1)
      val e3 = Uniform(2, 3)
      def f(b: Boolean) = if (b) e2 else e3
      val e4 = Chain(e1, f)
      pr.add(e4)
      val c4 = cc(e4)
      c4.expand(false)
      c4.expand(false)

      cc.expansions.size should equal(1)
    }

    "at one parent value, not add the parent" in {
      Universe.createNew()
      val cc = new ComponentCollection
      val pr = new Problem(cc)
      val e1 = Flip(0.2)
      val e2 = Constant(1)
      val e3 = Uniform(2, 3)
      def f(b: Boolean) = if (b) e2 else e3
      val e4 = Chain(e1, f)
      pr.add(e4)
      val c4 = cc(e4)
      c4.expand(false)

      cc.contains(e1) should equal(false)
    }

    "for a full expansion with an added parent, add subproblems for all the parent values" in {
      Universe.createNew()
      val cc = new ComponentCollection
      val pr = new Problem(cc)
      val e1 = Flip(0.2)
      val e2 = Constant(1)
      val e3 = Uniform(2, 3)
      def f(b: Boolean) = if (b) e2 else e3
      val e4 = Chain(e1, f)
      pr.add(e1)
      pr.add(e4)
      val c1 = cc(e1)
      val c4 = cc(e4)
      c1.generateRange()
      c4.expand()

      c4.subproblems.size should equal(2)
      c4.subproblems(true).target should equal(e2)
      c4.subproblems(false).target should equal(e3)
    }

    "for a full expansion with an unadded parent, not expand anything or add the parent" in {
      Universe.createNew()
      val cc = new ComponentCollection
      val pr = new Problem(cc)
      val e1 = Flip(0.2)
      val e2 = Constant(1)
      val e3 = Uniform(2, 3)
      def f(b: Boolean) = if (b) e2 else e3
      val e4 = Chain(e1, f)
      pr.add(e4)
      val c4 = cc(e4)
      c4.expand()

      c4.subproblems.size should equal(0)
      cc.contains(e1) should equal(false)
    }

    "if the result elements are already in the collection, not create a new component but point to the existing one in the subproblem" in {
      val e1 = Flip(0.3)
      val e2 = Select(0.1 -> 1, 0.9 -> 2)
      val e3 = Select(0.7 -> 1, 0.2 -> 2, 0.1 -> 3)
      val e4 = Chain(e1, (b: Boolean) => if (b) e2; else e3)
      val cc = new ComponentCollection
      val pr = new Problem(cc, List(e4))
      pr.add(e1)
      pr.add(e2)
      pr.add(e3)
      val c1 = cc(e1)
      val c2 = cc(e2)
      val c3 = cc(e3)
      val c4 = cc(e4)
      c1.generateRange()
      c2.generateRange()
      c3.generateRange()
      c4.expand()
      c4.generateRange()

      c4.range.hasStar should equal(false)
      c4.range.regularValues should equal(Set(1, 2, 3))
    }
  }

  def process(comp: ProblemComponent[_]) = {
    comp.generateRange(Int.MaxValue )
    comp.makeNonConstraintFactors(false)
    comp.makeConstraintFactors(Lower)
  }

  "Raising a subproblem" should {
    "add non-constraint factors to the chain" in {
      Universe.createNew()
      val cc = new ComponentCollection
      // raising should use the old chain method
      val pr = new Problem(cc)
      val e1 = Flip(0.2)
      def f(b: Boolean) = if (b) Uniform(1,2) else Uniform(3,4)
      val e4 = Chain(e1, f)
      pr.add(e4)
      pr.add(e1)
      val c4 = cc(e4)
      process(cc(e1))
      c4.expand()
      c4.subproblems.foreach(f => process(cc(f._2.target)))
      process(c4)
      c4.raise(Lower)
      c4.nonConstraintFactors.size should be (5)
      val tf = cc(c4.subproblems(true).target).nonConstraintFactors(0)
      val ff = cc(c4.subproblems(false).target).nonConstraintFactors(0)
      val tfVar = c4.nonConstraintFactors.exists(p => p.output.exists(_ == tf.variables.head) || p.parents.exists(_ == tf.variables.head))
      val ffVar = c4.nonConstraintFactors.exists(p => p.output.exists(_ == ff.variables.head) || p.parents.exists(_ == ff.variables.head))
      tfVar should be (false)
      ffVar should be (false)
    }
    
    "add constraint factors to the chain" in {
      Universe.createNew()
      val cc = new ComponentCollection
      // raising should use the old chain method
      val pr = new Problem(cc)
      val u1 = Uniform(1,2)
      val u2 = Uniform(3,4)
      val e1 = Flip(0.2)
      def f(b: Boolean) = if (b) u1 else u2
      val e4 = Chain(e1, f)
      u1.observe(1)
      pr.add(e4)
      pr.add(e1)
      val c4 = cc(e4)
      process(cc(e1))
      c4.expand()
      c4.subproblems.foreach(f => process(cc(f._2.target)))
      process(c4)
      c4.raise(Lower)
      c4.constraintFactors(Lower).size should be (1)      
    }

  }

  "Expanding a MakeArray" should {
    "at one count, add all the previously unadded items to the problem" in {
      Universe.createNew()
      val cc = new ComponentCollection
      val pr = new Problem(cc)
      val e0 = Constant(0)
      val e1 = Constant(1)
      val e2 = Constant(2)
      val ar = Array(e0, e1, e2)
      val e4 = Uniform(0, 1, 2)
      val e5 = new MakeArray("", e4, (i: Int) => ar(i), Universe.universe)
      pr.add(e0)
      pr.add(e5)
      val c5 = cc(e5)
      c5.expand(2)

      cc.contains(e1) should equal(true)
      cc(e1).problem should equal(pr)
    }

    "leave previously added items in their original problem" in {
      Universe.createNew()
      val cc = new ComponentCollection
      val pr1 = new Problem(cc)
      val pr2 = new Problem(cc)
      val e0 = Constant(0)
      val e1 = Constant(1)
      val e2 = Constant(2)
      val ar = Array(e0, e1, e2)
      val e4 = Uniform(0, 1, 2)
      val e5 = new MakeArray("", e4, (i: Int) => ar(i), Universe.universe)
      pr1.add(e0)
      pr2.add(e5)
      val c5 = cc(e5)
      c5.expand(2)

      cc(e0).problem should equal(pr1)
      cc(e1).problem should equal(pr2)
    }

    "for a full expansion with an added numItems, add the items for the maximum parent value" in {
      Universe.createNew()
      val cc = new ComponentCollection
      val pr = new Problem(cc)
      val e0 = Constant(0)
      val e1 = Constant(1)
      val e2 = Constant(2)
      val ar = Array(e0, e1, e2)
      val e4 = Uniform(0, 1, 2, 3)
      val e5 = new MakeArray("", e4, (i: Int) => ar(i), Universe.universe)
      pr.add(e4)
      pr.add(e5)
      val c4 = cc(e4)
      val c5 = cc(e5)
      c4.generateRange()
      c5.expand()

      cc.contains(e0) should equal(true)
      cc.contains(e1) should equal(true)
      cc.contains(e2) should equal(true)
    }

    "for a full expansion with an unadded numItems, not expand anything or add numItems" in {
      Universe.createNew()
      val cc = new ComponentCollection
      val pr = new Problem(cc)
      val e0 = Constant(0)
      val e1 = Constant(1)
      val e2 = Constant(2)
      val ar = Array(e0, e1, e2)
      val e4 = Uniform(0, 1, 2, 3)
      val e5 = new MakeArray("", e4, (i: Int) => ar(i), Universe.universe)
      pr.add(e5)
      val c5 = cc(e5)
      c5.expand()

      cc.contains(e0) should equal(false)
      cc.contains(e1) should equal(false)
      cc.contains(e2) should equal(false)
      cc.contains(e4) should equal(false)
    }
  }

  "Expanding a MakeArray" when {
    "expanding to a specific number of items greater than the previous maxExpanded" should {
      "set maxExpanded to the new number of items" in {
        val u = Universe.createNew()
        val cc = new ComponentCollection
        val pr = new Problem(cc)
        val e1 = new MakeArray("", Uniform(2, 4), (i: Int) => Constant(i), u)
        pr.add(e1)
        val c1 = cc(e1)
        c1.expand(2)
        c1.expand(4)

        c1.maxExpanded should equal(4)
      }

      "add all and only the previously unadded items to the problem" in {
        val u = Universe.createNew()
        val cc = new ComponentCollection
        val pr = new Problem(cc)
        val e1 = new MakeArray("", Uniform(2, 4), (i: Int) => Constant(i), u)
        pr.add(e1)
        val c1 = cc(e1)
        c1.expand(2)
        val n1 = cc.components.size
        c1.expand(4)
        val n2 = cc.components.size

        n1 should equal(3) // 1 for the MakeArray and 2 for the items
        n2 should equal(5)
      }
    }

    "expanding to a specific number of items less than the previous maxExpanded" should {
      "leave maxExpanded what it was" in {
        val u = Universe.createNew()
        val cc = new ComponentCollection
        val pr = new Problem(cc)
        val e1 = new MakeArray("", Uniform(2, 4), (i: Int) => Constant(i), u)
        pr.add(e1)
        val c1 = cc(e1)
        c1.expand(4)
        c1.expand(2)

        c1.maxExpanded should equal(4)
      }

      "add no items to the problem" in {
        val u = Universe.createNew()
        val cc = new ComponentCollection
        val pr = new Problem(cc)
        val e1 = new MakeArray("", Uniform(2, 4), (i: Int) => Constant(i), u)
        pr.add(e1)
        val c1 = cc(e1)
        c1.expand(4)
        val n1 = cc.components.size
        c1.expand(2)
        val n2 = cc.components.size

        n1 should equal(5)
        n2 should equal(5)
      }
    }

    "expanding with no specific value" should {
      "set maxExpanded to the maximum value" in {
        val u = Universe.createNew()
        val cc = new ComponentCollection
        val pr = new Problem(cc)
        val e1 = Uniform(2, 4)
        val e2 = new MakeArray("", e1, (i: Int) => Constant(i), u)
        pr.add(e1)
        pr.add(e2)
        val c1 = cc(e1)
        val c2 = cc(e2)
        c1.generateRange()
        c2.expand()

        c2.maxExpanded should equal(4)
      }

      "add all the elements to the collection" in {
        val u = Universe.createNew()
        val cc = new ComponentCollection
        val pr = new Problem(cc)
        val e1 = Uniform(2, 4)
        val e2 = new MakeArray("", e1, (i: Int) => Constant(i), u)
        pr.add(e1)
        pr.add(e2)
        val c1 = cc(e1)
        val c2 = cc(e2)
        c1.generateRange()
        c2.expand()

        cc.components.size should equal(6) // e1, e2, and the four items
      }
    }

  }
}
