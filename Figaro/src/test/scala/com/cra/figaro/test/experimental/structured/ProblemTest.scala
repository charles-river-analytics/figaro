package com.cra.figaro.test.experimental.structured

import org.scalatest.{WordSpec, Matchers}
import com.cra.figaro.experimental.structured._
import com.cra.figaro.language._
import com.cra.figaro.library.compound.If
import com.cra.figaro.library.collection.MakeArray

class ProblemTest extends WordSpec with Matchers {
  "Adding an element to a problem" should {
    "for an ordinary element, create an initialized ordinary problem component in the collection whose problem is the given problem" in {
      Universe.createNew()
      val cc = new ComponentCollection
      val pr = new Problem(cc)
      val e1 = Flip(0.1)
      pr.add(e1)
      val c1 = cc(e1)

      c1.problem should equal (pr)
      c1.element should equal (e1)
      c1.range.hasStar should equal (true)
      c1.range.regularValues should be (empty)
      c1.constraintLower should be (empty)
      c1.constraintUpper should be (empty)
      c1.nonConstraintFactors should be (empty)
//      c1.belief should equal (null)
//      c1.neighbors should be (empty)
//      c1.incomingMessages should be (empty)
//      c1.queuedMessages should be (empty)
    }

    "for a chain element, create an initialized chain problem component in the collection" in {
      Universe.createNew()
      val cc = new ComponentCollection
      val pr = new Problem(cc)
      val e1 = If(Flip(0.1), Flip(0.2), Flip(0.3))
      pr.add(e1)
      val c1 = cc(e1)

      c1.isInstanceOf[ChainComponent[Boolean, Boolean]] should equal (true)
      c1.subproblems should be (empty)
    }

    "for a MakeArray element, create an initialized MakeArray problem component in the collection" in {
      val universe = Universe.createNew()
      val cc = new ComponentCollection
      val pr = new Problem(cc)
      val e1 = new MakeArray("", Constant(2), (i: Int) => Constant(i), universe)
      pr.add(e1)
      val c1 = cc(e1)

      c1.isInstanceOf[MakeArrayComponent[Int]] should equal (true)
      c1.maxExpanded should equal (0)
    }

    "when adding an element twice, throw an exception" in {
      Universe.createNew()
      val cc = new ComponentCollection
      val pr = new Problem(cc)
      val e1 = Flip(0.1)
      pr.add(e1)

      an [Exception] should be thrownBy pr.add(e1)
    }
  }

  "Checking for the presence of a component" should {
    "for an added component, return true" in {
      Universe.createNew()
      val cc = new ComponentCollection
      val pr = new Problem(cc)
      val e1 = Flip(0.1)
      pr.add(e1)

      cc.contains(e1) should equal (true)
    }

    "for an unadded component, return false" in {
      Universe.createNew()
      val cc = new ComponentCollection
      val pr = new Problem(cc)
      val e1 = Flip(0.1)

      cc.contains(e1) should equal (false)
    }
  }

  "Attempting to access a component" should {
    "for an unadded component, throw NoSuchElementException" in {
      Universe.createNew()
      val cc = new ComponentCollection
      val pr = new Problem(cc)
      val e1 = Flip(0.1)

      an [NoSuchElementException] should be thrownBy cc(e1)
    }
  }
}
