package com.cra.figaro.test.experimental.structured

import org.scalatest.{WordSpec, Matchers}
import com.cra.figaro.language._
import com.cra.figaro.library.atomic.discrete.Uniform
import com.cra.figaro.library.compound.If
import com.cra.figaro.experimental.structured._
import com.cra.figaro.algorithm.lazyfactored.ValueSet._
import com.cra.figaro.util.MultiSet

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

    "for an if with values as the consequents, set the range to the two possible consequents, even if the test has not been added" in {
      Universe.createNew()
      val cc = new ComponentCollection
      val pr = new Problem(cc)
      val e1 = Flip(0.1)
      val e2 = If(e1, 1, 2)
      pr.add(e2)
      val c2 = cc(e2)
      c2.generateRange()

      c2.range.hasStar should equal (false)
      c2.range.regularValues should equal (Set(1,2))

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

    "for an Apply1 with an unadded argument, set the range to {*} and not add the argument" in {
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

    "for an Apply2 with an unadded argument, set the range to {*} and not add the argument" in {
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

    "for an Apply3 with an unadded argument, set the range to {*} and not add the argument" in {
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

    "for an Apply4 with an unadded argument, set the range to {*} and not add the argument" in {
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

    "for an Apply5 with an unadded argument, set the range to {*} and not add the argument" in {
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

    "for an Inject with an unadded argument, set the range to {*} and not add the argument" in {
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

    "for a chain with an unadded parent, set the range to {*} and don't add the parent" in {
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

    "for an unexpanded chain with an added parent, set the range to {*} and don't expand" in {
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

    "for a non-enumerable element, set the range to {*}" in {
      class TestElem(name: Name[Int], collection: ElementCollection) extends Element[Int](name, collection) {
        type Randomness = Int
        def args = List()
        def generateValue(rand: Randomness) = 1
        def generateRandomness() = 1
      }

      val universe = Universe.createNew()
      val cc = new ComponentCollection
      val pr = new Problem(cc)
      val e1 = new TestElem("", universe)
      pr.add(e1)
      val c1 = cc(e1)
      c1.generateRange()

      c1.range.hasStar should equal (true)
      c1.range.regularValues should equal (Set())
    }

    "for a simple single-valued reference with an added target, set the range to the range of the target" in {
      val universe = Universe.createNew()
      val cc = new ComponentCollection
      val pr = new Problem(cc)
      val e1 = Flip(0.5)("e1", universe)
      val e2 = universe.get[Boolean]("e1")
      pr.add(e1)
      pr.add(e2)
      val c1 = cc(e1)
      val c2 = cc(e2)
      c1.generateRange()
      c2.generateRange()

      c2.range.hasStar should equal (false)
      c2.range.regularValues should equal (Set(true, false))
    }

    "for a simple single-valued reference with an unadded target, set the range to {*} and not add the target" in {
      val universe = Universe.createNew()
      val cc = new ComponentCollection
      val pr = new Problem(cc)
      val e1 = Flip(0.5)("e1", universe)
      val e2 = universe.get[Boolean]("e1")
      pr.add(e2)
      val c2 = cc(e2)
      c2.generateRange()

      c2.range.hasStar should equal (true)
      c2.range.regularValues should equal (Set())
      cc.contains(e1) should equal (false)
    }

    "for an indirect single-valued reference with all required elements added, set the range to the range of the target" in {
      val universe = Universe.createNew()
      val cc = new ComponentCollection
      val pr = new Problem(cc)
      val ec1 = new EC1(universe)
      val e1 = Flip(0.5)("e1", ec1)
      val e2 = Constant(ec1)("e2", universe)
      val e3 = universe.get[Boolean]("e2.e1")
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
      c3.range.regularValues should equal (Set(true, false))
    }

    "for an indirect single-valued reference with a first required element unadded, set the range to {*} and not add the element" in {
      val universe = Universe.createNew()
      val cc = new ComponentCollection
      val pr = new Problem(cc)
      val ec1 = new EC1(universe)
      val e1 = Flip(0.5)("e1", ec1)
      val e2 = Constant(ec1)("e2", universe)
      val e3 = universe.get[Boolean]("e2.e1")
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

    "for an indirect single-valued reference with an internal required element unadded, set the range to {*} and not add the element" in {
      val universe = Universe.createNew()
      val cc = new ComponentCollection
      val pr = new Problem(cc)
      val ec1 = new EC1(universe)
      val ec2 = new EC1(universe)
      val e1 = Flip(0.5)("e1", ec1)
      val e2 = Constant(ec1)("e2", ec2)
      val e3 = Constant(ec2)("e3", universe)
      val e4 = universe.get[Boolean]("e3.e2.e1")
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

    "for a simple multi-valued reference with an added target, set the range to the range of the target" in {
      val universe = Universe.createNew()
      def aggregate(ms: MultiSet[Boolean]) = ms(true)
      val cc = new ComponentCollection
      val pr = new Problem(cc)
      val e1 = Flip(0.5)("e1", universe)
      val e2 = universe.getAggregate[Boolean, Int](aggregate)("e1")
      val e3 = e2.mvre
      pr.add(e1)
      pr.add(e3)
      val c1 = cc(e1)
      val c3 = cc(e3)
      c1.generateRange()
      c3.generateRange()

      c3.range.hasStar should equal (false)
      val vs = c3.range.regularValues.toList
      vs.size should equal (2)
      val ms0 = vs(0).elements
      val ms1 = vs(1).elements
      ms0.size should equal (1)
      ms1.size should equal (1)
      val hasTrue = ms0(0) == true || ms1(0) == true
      val hasFalse = ms0(0) == false || ms1(0) == false
      hasTrue should equal (true)
      hasFalse should equal (true)
    }

    "for a simple multi-valued reference with an unadded target, set the range to {*} and not add the target" in {
      val universe = Universe.createNew()
      def aggregate(ms: MultiSet[Boolean]) = ms(true)
      val cc = new ComponentCollection
      val pr = new Problem(cc)
      val e1 = Flip(0.5)("e1", universe)
      val e2 = universe.getAggregate[Boolean, Int](aggregate)("e1")
      val e3 = e2.mvre
      pr.add(e2) // don't need to add this
      pr.add(e3)
      val c2 = cc(e2)
      val c3 = cc(e3)
      c2.generateRange()
      c3.generateRange()

      c2.range.hasStar should equal (true)
      c2.range.regularValues should equal (Set())
      cc.contains(e1) should equal (false)
    }

    "for an indirect multi-valued reference with all required elements added, set the range to the range of the target" in {
      val universe = Universe.createNew()
      def aggregate(ms: MultiSet[Boolean]) = ms(true)
      val cc = new ComponentCollection
      val pr = new Problem(cc)
      val ec1 = new EC1(universe)
      val e1 = Flip(0.5)("e1", ec1)
      val e2 = Constant(ec1)("e2", universe)
      val e3 = universe.getAggregate[Boolean, Int](aggregate)("e2.e1")
      val e4 = e3.mvre
      pr.add(e1)
      pr.add(e2)
      pr.add(e4)
      val c1 = cc(e1)
      val c2 = cc(e2)
      val c4 = cc(e4)
      c1.generateRange()
      c2.generateRange()
      c4.generateRange()

      c4.range.hasStar should equal (false)
      val vs = c4.range.regularValues.toList
      vs.size should equal (2)
      val ms0 = vs(0).elements
      val ms1 = vs(1).elements
      ms0.size should equal (1)
      ms1.size should equal (1)
      val hasTrue = ms0(0) == true || ms1(0) == true
      val hasFalse = ms0(0) == false || ms1(0) == false
      hasTrue should equal (true)
      hasFalse should equal (true)
    }

    "for an indirect multi-valued reference with a first required element unadded, set the range to {*} and not add the element" in {
      val universe = Universe.createNew()
      def aggregate(ms: MultiSet[Boolean]) = ms(true)
      val cc = new ComponentCollection
      val pr = new Problem(cc)
      val ec1 = new EC1(universe)
      val e1 = Flip(0.5)("e1", ec1)
      val e2 = Constant(ec1)("e2", universe)
      val e3 = universe.getAggregate[Boolean, Int](aggregate)("e2.e1")
      val e4 = e3.mvre
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

    "for an indirect multi-valued reference with an internal required element unadded, set the range to {*} and not add the element" in {
      val universe = Universe.createNew()
      def aggregate(ms: MultiSet[Boolean]) = ms(true)
      val cc = new ComponentCollection
      val pr = new Problem(cc)
      val ec1 = new EC1(universe)
      val ec2 = new EC1(universe)
      val e1 = Flip(0.5)("e1", ec1)
      val e2 = Constant(ec1)("e2", ec2)
      val e3 = Constant(ec2)("e3", universe)
      val e4 = universe.getAggregate[Boolean, Int](aggregate)("e3.e2.e1")
      val e5 = e4.mvre
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

    "for a multiset with branching possibilities, set the range to the union of all the possible multisets" in {
      val universe = Universe.createNew()
      def aggregate(ms: MultiSet[Int]) = ms.elements.sum
      val cc = new ComponentCollection
      val pr = new Problem(cc)
      val ec1 = new EC1(universe)
      val ec2 = new EC1(universe)
      val ec3 = new EC1(universe)
      val ec4 = new EC1(universe)
      val ec5 = new EC1(universe)
      val e1a = Uniform(1, 2)("e1", ec1)
      val e1b = Uniform(3, 4)("e1", ec2)
      val e1c = Constant(5)("e1", ec3)
      val e2a = Uniform(ec1, ec2)("e2", ec4)
      val e2b = Constant(ec3)("e2", ec5)
      val e3 = Uniform(List(ec4, ec5), List(ec5))("e3", universe)
      val e4 = universe.getAggregate(aggregate)("e3.e2.e1")
      val e5 = e4.mvre
      pr.add(e1a)
      pr.add(e1b)
      pr.add(e1c)
      pr.add(e2a)
      pr.add(e2b)
      pr.add(e3)
      pr.add(e4)
      pr.add(e5)
      val c1a = cc(e1a)
      val c1b = cc(e1b)
      val c1c = cc(e1c)
      val c2a = cc(e2a)
      val c2b = cc(e2b)
      val c3 = cc(e3)
      val c4 = cc(e4)
      val c5 = cc(e5)
      c1a.generateRange()
      c1b.generateRange()
      c1c.generateRange()
      c2a.generateRange()
      c2b.generateRange()
      c3.generateRange()
      c4.generateRange()
      c5.generateRange()

      // possible resolutions of e3: ec4 + ec5; ec5
      // possible resolutions of e3.e2: ec1 + ec3; ec2 + ec3; ec4
      // possible resolutions of e3.e2.e1: 1 + 5; 2 + 5; 3 + 5; 4 + 5; 5
      c5.range.hasStar should equal (false)
      val vs = c5.range.regularValues.toList
      vs.size should equal (5)
      val sums = vs.map(_.sum)
      val has5 = sums.exists(_ == 5)
      val has6 = sums.exists(_ == 6)
      val has7 = sums.exists(_ == 7)
      val has8 = sums.exists(_ == 8)
      val has9 = sums.exists(_ == 9)
      has5 should equal (true)
      has6 should equal (true)
      has7 should equal (true)
      has8 should equal (true)
      has9 should equal (true)
    }

    "for an aggregate with an added multi-valued reference element, set the range to all the possible aggregates of the values of the MVRE" in {
      val universe = Universe.createNew()
      def aggregate(ms: MultiSet[Int]) = ms.elements.sum
      val cc = new ComponentCollection
      val pr = new Problem(cc)
      val ec1 = new EC1(universe)
      val ec2 = new EC1(universe)
      val ec3 = new EC1(universe)
      val ec4 = new EC1(universe)
      val ec5 = new EC1(universe)
      val e1a = Uniform(1, 2)("e1", ec1)
      val e1b = Uniform(3, 4)("e1", ec2)
      val e1c = Constant(5)("e1", ec3)
      val e2a = Uniform(ec1, ec2)("e2", ec4)
      val e2b = Constant(ec3)("e2", ec5)
      val e3 = Uniform(List(ec4, ec5), List(ec5))("e3", universe)
      val e4 = universe.getAggregate(aggregate)("e3.e2.e1")
      val e5 = e4.mvre
      pr.add(e1a)
      pr.add(e1b)
      pr.add(e1c)
      pr.add(e2a)
      pr.add(e2b)
      pr.add(e3)
      pr.add(e4)
      pr.add(e5)
      val c1a = cc(e1a)
      val c1b = cc(e1b)
      val c1c = cc(e1c)
      val c2a = cc(e2a)
      val c2b = cc(e2b)
      val c3 = cc(e3)
      val c4 = cc(e4)
      val c5 = cc(e5)
      c1a.generateRange()
      c1b.generateRange()
      c1c.generateRange()
      c2a.generateRange()
      c2b.generateRange()
      c3.generateRange()
      // mvre range has to be generated before aggregate
      c5.generateRange()
      c4.generateRange()

      // possible resolutions of e3: ec4 + ec5; ec5
      // possible resolutions of e3.e2: ec1 + ec3; ec2 + ec3; ec4
      // possible resolutions of e3.e2.e1: 1 + 5; 2 + 5; 3 + 5; 4 + 5; 5
      // possible sums are 5, 6, 7, 8, 9
      c4.range.hasStar should equal (false)
      val vs = c4.range.regularValues.toList
      vs.size should equal (5)
      val has5 = vs.exists(_ == 5)
      val has6 = vs.exists(_ == 6)
      val has7 = vs.exists(_ == 7)
      val has8 = vs.exists(_ == 8)
      val has9 = vs.exists(_ == 9)
      has5 should equal (true)
      has6 should equal (true)
      has7 should equal (true)
      has8 should equal (true)
      has9 should equal (true)

    }

    "for an aggregate with an unadded multi-valued reference element, set the range to * and don't add the MVRE" in {
      val universe = Universe.createNew()
      def aggregate(ms: MultiSet[Int]) = ms.elements.sum
      val cc = new ComponentCollection
      val pr = new Problem(cc)
      val ec1 = new EC1(universe)
      val ec2 = new EC1(universe)
      val ec3 = new EC1(universe)
      val ec4 = new EC1(universe)
      val ec5 = new EC1(universe)
      val e1a = Uniform(1, 2)("e1", ec1)
      val e1b = Uniform(3, 4)("e1", ec2)
      val e1c = Constant(5)("e1", ec3)
      val e2a = Uniform(ec1, ec2)("e2", ec4)
      val e2b = Constant(ec3)("e2", ec5)
      val e3 = Uniform(List(ec4, ec5), List(ec5))("e3", universe)
      val e4 = universe.getAggregate(aggregate)("e3.e2.e1")
      val e5 = e4.mvre
      pr.add(e1a)
      pr.add(e1b)
      pr.add(e1c)
      pr.add(e2a)
      pr.add(e2b)
      pr.add(e3)
      pr.add(e4)
      val c1a = cc(e1a)
      val c1b = cc(e1b)
      val c1c = cc(e1c)
      val c2a = cc(e2a)
      val c2b = cc(e2b)
      val c3 = cc(e3)
      val c4 = cc(e4)
      c1a.generateRange()
      c1b.generateRange()
      c1c.generateRange()
      c2a.generateRange()
      c2b.generateRange()
      c3.generateRange()
      c4.generateRange()

      c4.range.hasStar should equal (true)
      c4.range.regularValues should equal (Set())
    }
  }

  class EC1(universe: Universe) extends ElementCollection
}
