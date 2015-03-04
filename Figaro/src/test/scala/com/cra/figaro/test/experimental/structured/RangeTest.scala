package com.cra.figaro.test.experimental.structured

import org.scalatest.{WordSpec, Matchers}
import com.cra.figaro.language._
import com.cra.figaro.experimental.structured._
import com.cra.figaro.algorithm.lazyfactored.ValueSet._

class RangeTest extends WordSpec with Matchers {
  "Setting the range of a component" should {
    "for a constant, set the range to only that constant" in {
      Universe.createNew()
      val cc = new ComponentCollection
      val pr = new Problem(cc)
      val e1 = Constant(5)
      pr.add(e1)
      val c1 = cc(e1)
      c1.generateRange()

      c1.range.hasStar should equal (false)
      c1.range.regularValues should equal (Set(5))
    }

    "for a flip, set the range to only true and false" in {
      Universe.createNew()
      val cc = new ComponentCollection
      val pr = new Problem(cc)
      val e1 = Flip(0.3)
      pr.add(e1)
      val c1 = cc(e1)
      c1.generateRange()

      c1.range.hasStar should equal (false)
      c1.range.regularValues should equal (Set(true, false))
    }

    "for a select, set the range to only the possible outcomes" in {
      Universe.createNew()
      val cc = new ComponentCollection
      val pr = new Problem(cc)
      val e1 = Select(0.1 -> 'a, 0.2 -> 'b, 0.3 -> 'c)
      pr.add(e1)
      val c1 = cc(e1)
      c1.generateRange()

      c1.range.hasStar should equal (false)
      c1.range.regularValues should equal (Set('a, 'b, 'c))
    }

    "for a dist, set the range to the union of the precomputed ranges of the outcomes, including * if necessary" in {
      Universe.createNew()
      val cc = new ComponentCollection
      val pr = new Problem(cc)
      val e1 = Constant(5)
      val e2 = Select(0.2 -> 2, 0.8 -> 3)
      val e3 = Select(0.1 -> 6, 0.2 -> 7, 0.3 -> 8)
      val e4 = Constant(1)
      val e5 = Dist(0.4 -> e1, 0.3 -> e2, 0.3 -> e3)
      pr.add(e1)
      pr.add(e2)
      // we do not add e3
      pr.add(e4)
      pr.add(e5)
      val c1 = cc(e1)
      val c2 = cc(e2)
      val c4 = cc(e4)
      val c5 = cc(e5)
      c1.generateRange()
      c2.generateRange()
      // we do not generate a range for e4
      c5.generateRange()

      c5.range.hasStar should equal (true)
      c5.range.regularValues should equal (Set(5, 2, 3))
      cc.contains(e3) should equal (false)
    }

    "for an Apply1 with an added argument without *, set the range to the image of the function on the argument values" in {
      Universe.createNew()
      val cc = new ComponentCollection
      val pr = new Problem(cc)
      val e1 = Select(0.1 -> 1, 0.2 -> 2, 0.3 -> 3)
      val e2 = e1.map(_ + 1)
      pr.add(e1)
      pr.add(e2)
      val c1 = cc(e1)
      val c2 = cc(e2)
      c1.generateRange()
      c2.generateRange()

      c2.range.hasStar should equal (false)
      c2.range.regularValues should equal (Set(2, 3, 4))
    }

    "for an Apply1 with an added argument that contains *, set the range to the image of the function on the regular values plus *" in {
      Universe.createNew()
      val cc = new ComponentCollection
      val pr = new Problem(cc)
      val e1 = Constant(1)
      val e2 = Select(0.2 -> 2, 0.3 -> 3)
      val e3 = Dist(0.1 -> e1, 0.2 -> e2)
      val e4 = e3.map(_ + 1)
      pr.add(e2)
      pr.add(e3)
      pr.add(e4)
      val c2 = cc(e2)
      val c3 = cc(e3)
      val c4 = cc(e4)
      c2.generateRange()
      c3.generateRange()
      c4.generateRange()

      c4.range.hasStar should equal (true)
      c4.range.regularValues should equal (Set(3, 4))
    }

    "for an Apply1 with an unadded argument, set the range to empty and not add the argument" in {
      Universe.createNew()
      val cc = new ComponentCollection
      val pr = new Problem(cc)
      val e1 = Select(0.1 -> 1, 0.2 -> 2, 0.3 -> 3)
      val e2 = e1.map(_ + 1)
      pr.add(e2)
      val c2 = cc(e2)
      c2.generateRange()

      c2.range.hasStar should equal (true)
      c2.range.regularValues should equal (Set())
      cc.contains(e1) should equal (false)
    }

    "for an Apply2 with added arguments without *, set the range to the image of the function on the argument values" in {
      Universe.createNew()
      val cc = new ComponentCollection
      val pr = new Problem(cc)
      val e1 = Select(0.1 -> 1, 0.2 -> 2, 0.3 -> 3)
      val e2 = Constant(4)
      val e3 = Apply(e1, e2, (i1: Int, i2: Int) => i1 + i2)
      pr.add(e1)
      pr.add(e2)
      pr.add(e3)
      val c1 = cc(e1)
      val c2 = cc(e2)
      val c3 = cc(e3)
      c1.generateRange()
      c2.generateRange()
      c3.generateRange()

      c3.range.hasStar should equal (false)
      c3.range.regularValues should equal (Set(5, 6, 7))
    }

    "for an Apply2 with added arguments that contain *, set the range to the image of the function on the regular values plus *" in {
      Universe.createNew()
      val cc = new ComponentCollection
      val pr = new Problem(cc)
      val e1 = Constant(1)
      val e2 = Select(0.2 -> 2, 0.3 -> 3)
      val e3 = Dist(0.1 -> e1, 0.2 -> e2)
      val e4 = Constant(4)
      val e5 = Apply(e3, e4, (i1: Int, i2: Int) => i1 + i2)
      pr.add(e2)
      pr.add(e3)
      pr.add(e4)
      pr.add(e5)
      val c2 = cc(e2)
      val c3 = cc(e3)
      val c4 = cc(e4)
      val c5 = cc(e5)
      c2.generateRange()
      c3.generateRange()
      c4.generateRange()
      c5.generateRange()

      c5.range.hasStar should equal (true)
      c5.range.regularValues should equal (Set(6, 7))
    }

    "for an Apply2 with an unadded argument, set the range to empty and not add the argument" in {
      Universe.createNew()
      val cc = new ComponentCollection
      val pr = new Problem(cc)
      val e1 = Select(0.1 -> 1, 0.2 -> 2, 0.3 -> 3)
      val e2 = Constant(4)
      val e3 = Apply(e1, e2, (i1: Int, i2: Int) => i1 + i2)
      pr.add(e1)
      pr.add(e3)
      val c1 = cc(e1)
      val c3 = cc(e3)
      c1.generateRange()
      c3.generateRange()

      c3.range.hasStar should equal (true)
      c3.range.regularValues should equal (Set())
      cc.contains(e2) should equal (false)
    }

    "for an Apply3 with added arguments without *, set the range to the image of the function on the argument values" in {
      Universe.createNew()
      val cc = new ComponentCollection
      val pr = new Problem(cc)
      val e1 = Select(0.1 -> 1, 0.2 -> 2, 0.3 -> 3)
      val e2 = Constant(4)
      val e3 = Constant(5)
      val e4 = Apply(e1, e2, e3, (i1: Int, i2: Int, i3: Int) => i1 + i2 + i3)
      pr.add(e1)
      pr.add(e2)
      pr.add(e3)
      pr.add(e4)
      val c1 = cc(e1)
      val c2 = cc(e2)
      val c3 = cc(e3)
      val c4 = cc(e4)
      c1.generateRange()
      c2.generateRange()
      c3.generateRange()
      c4.generateRange()

      c4.range.hasStar should equal (false)
      c4.range.regularValues should equal (Set(10, 11, 12))
    }

    "for an Apply3 with added arguments that contain *, set the range to the image of the function on the regular values plus *" in {
      Universe.createNew()
      val cc = new ComponentCollection
      val pr = new Problem(cc)
      val e1 = Constant(1)
      val e2 = Select(0.2 -> 2, 0.3 -> 3)
      val e3 = Dist(0.1 -> e1, 0.2 -> e2)
      val e4 = Constant(4)
      val e5 = Constant(5)
      val e6 = Apply(e3, e4, e5, (i1: Int, i2: Int, i3: Int) => i1 + i2 + i3)
      pr.add(e2)
      pr.add(e3)
      pr.add(e4)
      pr.add(e5)
      pr.add(e6)
      val c2 = cc(e2)
      val c3 = cc(e3)
      val c4 = cc(e4)
      val c5 = cc(e5)
      val c6 = cc(e6)
      c2.generateRange()
      c3.generateRange()
      c4.generateRange()
      c5.generateRange()
      c6.generateRange()

      c6.range.hasStar should equal (true)
      c6.range.regularValues should equal (Set(11, 12))
    }

    "for an Apply3 with an unadded argument, set the range to empty and not add the argument" in {
      Universe.createNew()
      val cc = new ComponentCollection
      val pr = new Problem(cc)
      val e1 = Select(0.1 -> 1, 0.2 -> 2, 0.3 -> 3)
      val e2 = Constant(4)
      val e3 = Constant(5)
      val e4 = Apply(e1, e2, e3, (i1: Int, i2: Int, i3: Int) => i1 + i2 + i3)
      pr.add(e1)
      pr.add(e3)
      pr.add(e4)
      val c1 = cc(e1)
      val c3 = cc(e3)
      val c4 = cc(e4)
      c1.generateRange()
      c3.generateRange()
      c4.generateRange()

      c4.range.hasStar should equal (true)
      c4.range.regularValues should equal (Set())
      cc.contains(e2) should equal (false)
    }

    "for an Apply4 with added arguments without *, set the range to the image of the function on the argument values" in {
      Universe.createNew()
      val cc = new ComponentCollection
      val pr = new Problem(cc)
      val e1 = Select(0.1 -> 1, 0.2 -> 2, 0.3 -> 3)
      val e2 = Constant(4)
      val e3 = Constant(5)
      val e4 = Constant(6)
      val e5 = Apply(e1, e2, e3, e4, (i1: Int, i2: Int, i3: Int, i4: Int) => i1 + i2 + i3 + i4)
      pr.add(e1)
      pr.add(e2)
      pr.add(e3)
      pr.add(e4)
      pr.add(e5)
      val c1 = cc(e1)
      val c2 = cc(e2)
      val c3 = cc(e3)
      val c4 = cc(e4)
      val c5 = cc(e5)
      c1.generateRange()
      c2.generateRange()
      c3.generateRange()
      c4.generateRange()
      c5.generateRange()

      c5.range.hasStar should equal (false)
      c5.range.regularValues should equal (Set(16, 17, 18))
    }

    "for an Apply4 with added arguments that contain *, set the range to the image of the function on the regular values plus *" in {
      Universe.createNew()
      val cc = new ComponentCollection
      val pr = new Problem(cc)
      val e1 = Constant(1)
      val e2 = Select(0.2 -> 2, 0.3 -> 3)
      val e3 = Dist(0.1 -> e1, 0.2 -> e2)
      val e4 = Constant(4)
      val e5 = Constant(5)
      val e6 = Constant(6)
      val e7 = Apply(e3, e4, e5, e6, (i1: Int, i2: Int, i3: Int, i4: Int) => i1 + i2 + i3 + i4)
      pr.add(e2)
      pr.add(e3)
      pr.add(e4)
      pr.add(e5)
      pr.add(e6)
      pr.add(e7)
      val c2 = cc(e2)
      val c3 = cc(e3)
      val c4 = cc(e4)
      val c5 = cc(e5)
      val c6 = cc(e6)
      val c7 = cc(e7)
      c2.generateRange()
      c3.generateRange()
      c4.generateRange()
      c5.generateRange()
      c6.generateRange()
      c7.generateRange()

      c7.range.hasStar should equal (true)
      c7.range.regularValues should equal (Set(17, 18))
    }

    "for an Apply4 with an unadded argument, set the range to empty and not add the argument" in {
      Universe.createNew()
      val cc = new ComponentCollection
      val pr = new Problem(cc)
      val e1 = Select(0.1 -> 1, 0.2 -> 2, 0.3 -> 3)
      val e2 = Constant(4)
      val e3 = Constant(5)
      val e4 = Constant(6)
      val e5 = Apply(e1, e2, e3, e4, (i1: Int, i2: Int, i3: Int, i4: Int) => i1 + i2 + i3 + i4)
      pr.add(e1)
      pr.add(e3)
      pr.add(e4)
      pr.add(e5)
      val c1 = cc(e1)
      val c3 = cc(e3)
      val c4 = cc(e4)
      val c5 = cc(e5)
      c1.generateRange()
      c3.generateRange()
      c4.generateRange()
      c5.generateRange()

      c5.range.hasStar should equal (true)
      c5.range.regularValues should equal (Set())
      cc.contains(e2) should equal (false)
    }

    "for an Apply5 with added arguments without *, set the range to the image of the function on the argument values" in {
      Universe.createNew()
      val cc = new ComponentCollection
      val pr = new Problem(cc)
      val e1 = Select(0.1 -> 1, 0.2 -> 2, 0.3 -> 3)
      val e2 = Constant(4)
      val e3 = Constant(5)
      val e4 = Constant(6)
      val e5 = Constant(7)
      val e6 = Apply(e1, e2, e3, e4, e5, (i1: Int, i2: Int, i3: Int, i4: Int, i5: Int) => i1 + i2 + i3 + i4 + i5)
      pr.add(e1)
      pr.add(e2)
      pr.add(e3)
      pr.add(e4)
      pr.add(e5)
      pr.add(e6)
      val c1 = cc(e1)
      val c2 = cc(e2)
      val c3 = cc(e3)
      val c4 = cc(e4)
      val c5 = cc(e5)
      val c6 = cc(e6)
      c1.generateRange()
      c2.generateRange()
      c3.generateRange()
      c4.generateRange()
      c5.generateRange()
      c6.generateRange()

      c6.range.hasStar should equal (false)
      c6.range.regularValues should equal (Set(23, 24, 25))
    }

    "for an Apply5 with added arguments that contain *, set the range to the image of the function on the regular values plus *" in {
      Universe.createNew()
      val cc = new ComponentCollection
      val pr = new Problem(cc)
      val e1 = Constant(1)
      val e2 = Select(0.2 -> 2, 0.3 -> 3)
      val e3 = Dist(0.1 -> e1, 0.2 -> e2)
      val e4 = Constant(4)
      val e5 = Constant(5)
      val e6 = Constant(6)
      val e7 = Constant(7)
      val e8 = Apply(e3, e4, e5, e6, e7, (i1: Int, i2: Int, i3: Int, i4: Int, i5: Int) => i1 + i2 + i3 + i4 + i5)
      pr.add(e2)
      pr.add(e3)
      pr.add(e4)
      pr.add(e5)
      pr.add(e6)
      pr.add(e7)
      pr.add(e8)
      val c2 = cc(e2)
      val c3 = cc(e3)
      val c4 = cc(e4)
      val c5 = cc(e5)
      val c6 = cc(e6)
      val c7 = cc(e7)
      val c8 = cc(e8)
      c2.generateRange()
      c3.generateRange()
      c4.generateRange()
      c5.generateRange()
      c6.generateRange()
      c7.generateRange()
      c8.generateRange()

      c8.range.hasStar should equal (true)
      c8.range.regularValues should equal (Set(24, 25))
    }

    "for an Apply5 with an unadded argument, set the range to empty and not add the argument" in {
      Universe.createNew()
      val cc = new ComponentCollection
      val pr = new Problem(cc)
      val e1 = Select(0.1 -> 1, 0.2 -> 2, 0.3 -> 3)
      val e2 = Constant(4)
      val e3 = Constant(5)
      val e4 = Constant(6)
      val e5 = Constant(7)
      val e6 = Apply(e1, e2, e3, e4, e5, (i1: Int, i2: Int, i3: Int, i4: Int, i5: Int) => i1 + i2 + i3 + i4 + i5)
      pr.add(e1)
      pr.add(e3)
      pr.add(e4)
      pr.add(e5)
      pr.add(e6)
      val c1 = cc(e1)
      val c3 = cc(e3)
      val c4 = cc(e4)
      val c5 = cc(e5)
      val c6 = cc(e6)
      c1.generateRange()
      c3.generateRange()
      c4.generateRange()
      c5.generateRange()
      c6.generateRange()

      c6.range.hasStar should equal (true)
      c6.range.regularValues should equal (Set())
      cc.contains(e2) should equal (false)
    }

    "for an Inject with added arguments without *, set the range to lists of the cartesian product of the ranges of the arguments" in {
      Universe.createNew()
      val cc = new ComponentCollection
      val pr = new Problem(cc)
      val e1 = Select(0.1 -> 1, 0.2 -> 2)
      val e2 = Select(0.3 -> 3, 0.4 -> 4)
      val e3 = Inject(e1, e2)
      pr.add(e1)
      pr.add(e2)
      pr.add(e3)
      val c1 = cc(e1)
      val c2 = cc(e2)
      val c3 = cc(e3)
      c1.generateRange()
      c2.generateRange()
      c3.generateRange()

      c3.range.hasStar should equal (false)
      c3.range.regularValues should equal (Set(List(1, 3), List(1, 4), List(2, 3), List(2, 4)))
    }

    "for an Inject with added arguments that contain *, set the range to lists of the cartesian product of the regular values of the arguments plus *" in {
      Universe.createNew()
      val cc = new ComponentCollection
      val pr = new Problem(cc)
      val e1 = Constant(1)
      val e2 = Constant(2)
      val e3 = Dist(0.1 -> e1, 0.2 -> e2)
      val e4 = Select(0.3 -> 3, 0.4 -> 4)
      val e5 = Inject(e3, e4)
      pr.add(e2)
      pr.add(e3)
      pr.add(e4)
      pr.add(e5)
      val c2 = cc(e2)
      val c3 = cc(e3)
      val c4 = cc(e4)
      val c5 = cc(e5)
      c2.generateRange()
      c3.generateRange()
      c4.generateRange()
      c5.generateRange()

      c5.range.hasStar should equal (true)
      c5.range.regularValues should equal (Set(List(2, 3), List(2, 4)))
    }

    "for an Inject with an unadded argument, set the range to empty and not add the argument" in {
      Universe.createNew()
      val cc = new ComponentCollection
      val pr = new Problem(cc)
      val e1 = Select(0.1 -> 1, 0.2 -> 2)
      val e2 = Select(0.3 -> 3, 0.4 -> 4)
      val e3 = Inject(e1, e2)
      pr.add(e2)
      pr.add(e3)
      val c2 = cc(e2)
      val c3 = cc(e3)
      c2.generateRange()
      c3.generateRange()

      c3.range.hasStar should equal (true)
      c3.range.regularValues should equal (Set())
      cc.contains(e1) should equal (false)
    }

    "for a chain with an unadded parent, set the range to empty and don't add the parent" in {
      Universe.createNew()
      val cc = new ComponentCollection
      val pr = new Problem(cc)
      val e1 = Flip(0.1)
      val e2 = Chain(e1, (b: Boolean) => if (b) Constant(1) else Constant(2))
      pr.add(e2)
      val c2 = cc(e2)
      c2.generateRange()

      c2.range.hasStar should equal (true)
      c2.range.regularValues should equal (Set())
      cc.contains(e1) should equal (false)
    }

    "for an unexpanded chain with an added parent, set the range to empty and don't expand" in {
      Universe.createNew()
      val cc = new ComponentCollection
      val pr = new Problem(cc)
      val e1 = Flip(0.1)
      val e2 = Chain(e1, (b: Boolean) => if (b) Constant(1) else Constant(2))
      pr.add(e1)
      pr.add(e2)
      val c1 = cc(e1)
      val c2 = cc(e2)
      c1.generateRange()
      c2.generateRange()

      c2.range.hasStar should equal (true)
      c2.range.regularValues should equal (Set())
      c2.asInstanceOf[ChainComponent[_,_]].subproblems should be (empty)
    }

    "for an expanded chain with an added parent, set the range to the union of the ranges of the outcome elements" in {
      Universe.createNew()
      val cc = new ComponentCollection
      val pr = new Problem(cc)
      val e1 = Flip(0.1)
      val e2 = Constant(1)
      val e3 = Constant(2)
      val e4 = Chain(e1, (b: Boolean) => if (b) e2 else e3)
      pr.add(e1)
      pr.add(e4)
      val c1 = cc(e1)
      val c4 = cc(e4)
      c1.generateRange()
      c4.expand()
      val c2 = cc(c4.subproblems(true).target)
      val c3 = cc(c4.subproblems(false).target)
      c2.generateRange()
      c3.generateRange()
      c4.generateRange()

      c4.range.hasStar should equal (false)
      c4.range.regularValues should equal (Set(1, 2))
    }

  }
}
