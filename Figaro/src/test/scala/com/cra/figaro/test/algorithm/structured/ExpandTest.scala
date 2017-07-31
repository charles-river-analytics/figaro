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
  def recursiveElement(): Chain[Boolean, Boolean] = Chain(Flip(0.5), chainFunction)
  val chainFunction = (_: Boolean) => recursiveElement()

  "Expanding a chain" should {
    "add the expanded subproblem to the collection" in {
      Universe.createNew()
      val cc = new ComponentCollection
      val pr = new Problem(cc)
      val e1 = Flip(0.2)
      val e2 = Constant(1)
      val e3 = Uniform(2, 3)
      val f = (b: Boolean) => if (b) e2 else e3
      val e4 = Chain(e1, f)
      pr.add(e4)
      val c4 = cc(e4)
      c4.expand(false)

      val spr = c4.subproblems(false)
      cc.expansions((f, false)) should have size 1
      cc.expansions((f, false)).head should be theSameInstanceAs spr
      cc.expandableComponents(spr) should equal(Set(c4))
    }

    "correctly detect cycles and return the appropriate copy of the subproblem" when {
      "the subproblem does not use itself recursively" in {
        Universe.createNew()
        val cc = new ComponentCollection
        val pr = new Problem(cc)
        val e1 = Flip(0.2)
        val e2 = Flip(0.3)
        val e3 = Constant(1)
        val e4 = Uniform(2, 3)
        val f = (b: Boolean) => if (b) e3 else e4
        val e5 = Chain(e1, f)
        val e6 = Chain(e2, f)
        pr.add(e5)
        pr.add(e6)
        val c5 = cc(e5)
        val c6 = cc(e6)
        c5.expand(false)
        cc.createsCycle(c6, c5.subproblems(false)) should equal(false)
        c6.expand(false)

        val spr5 = c5.subproblems(false)
        val spr6 = c6.subproblems(false)
        spr5 should be theSameInstanceAs spr6
        cc.expansions((f, false)) should have size 1
        cc.expansions((f, false)).head should be theSameInstanceAs spr5
        cc.expandableComponents(spr5) should equal(Set(c5, c6))
      }

      "the subproblem uses itself recursively, either directly or indirectly" in {
        Universe.createNew()
        val cc = new ComponentCollection
        val pr = new Problem(cc)
        // A simple branching program that calls itself recursively through one of two subproblems
        val e2 = recursiveElement()
        val e1 = e2.parent
        pr.add(e1)
        pr.add(e2)
        val c1 = cc(e1)
        val c2 = cc(e2)
        c1.generateRange()
        c2.expand(true)
        c2.expand(false)

        val sprTrue = c2.subproblems(true)
        val sprFalse = c2.subproblems(false)
        val e2True = sprTrue.target.asInstanceOf[Chain[Boolean, Boolean]]
        val e1True = e2True.parent
        sprTrue.add(e1True)
        sprTrue.add(e2True)
        val c1True = cc(e1True)
        val c2True = cc(e2True)
        c1True.generateRange()

        // The collection should not allow the true subproblem to use itself; this is a direct recursion.
        cc.createsCycle(c2True, sprTrue) should equal(true)
        c2True.expand(true)
        val sprTrueTrue = c2True.subproblems(true)
        sprTrueTrue should not equal sprTrue
        // However, the collection should allow the true subproblem to use the false subproblem
        cc.createsCycle(c2True, sprFalse) should equal(false)
        c2True.expand(false)
        val sprTrueFalse = c2True.subproblems(false)
        sprTrueFalse should equal(sprFalse)

        val e2False = sprFalse.target.asInstanceOf[Chain[Boolean, Boolean]]
        val e1False = e2False.parent
        sprFalse.add(e1False)
        sprFalse.add(e2False)
        val c1False = cc(e1False)
        val c2False = cc(e2False)
        c1False.generateRange()

        // This creates a cycle because we already allowed the true subproblem to use the false subproblem; this is
        // an indirect recursion. However, we should be able to use the depth 2 true subproblem from here. This tests
        // that the collection always finds the shallowest available subproblem before expanding a new one.
        cc.createsCycle(c2False, sprTrue) should equal(true)
        cc.createsCycle(c2False, sprTrueTrue) should equal(false)
        c2False.expand(true)
        val sprFalseTrue = c2True.subproblems(true)
        sprFalseTrue should not equal sprTrue
        sprFalseTrue should equal(sprTrueTrue)
      }
    }

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

    "when repeating a parent value for the same component, not create a new expansion" in {
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
      cc.expansions.head._2.size should equal(1)
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
