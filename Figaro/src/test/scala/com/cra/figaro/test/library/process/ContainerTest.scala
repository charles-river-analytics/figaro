/*
 * ContainerTest.scala
 * Container tests.
 *
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Nov 27, 2014
 *
 * Copyright 2014 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.test.library.process

import org.scalatest.WordSpec
import org.scalatest.Matchers
import com.cra.figaro.library.process._
import com.cra.figaro.language._
import com.cra.figaro.util._
import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.algorithm.sampling.Importance
import com.cra.figaro.library.compound._
import com.cra.figaro.algorithm.sampling.MetropolisHastings

class ContainerTest extends WordSpec with Matchers {
  "A Container" should {
    "create elements in the correct universe" in {
      val u1 = Universe.createNew()
      val proc = createContainer(List(2,3))
      val fsa1 = new FixedSizeArray(1, i => Constant(true)("", u1))
      val u2 = Universe.createNew()
      val fsa2 = new FixedSizeArray(1, i => Constant(false)("", u2))
      val e1 = proc(2)
      e1.universe should equal (u1)
      val e2 = proc.get(2)
      e2.universe should equal (u1)
      val e3 = proc(List(2,3))(2)
      e3.universe should equal (u1)
      val e4 = proc.get(List(2,3))(2)
      e4.universe should equal (u1)
      val e5 = proc.map(!_)(2)
      e5.universe should equal (u1)
      val e6 = proc.chain(if (_) Flip(0.3) else Flip(0.6))(2)
      e6.universe should equal (u1)
      val proc2 = createContainer(List(4))
      val proc3 = proc ++ proc2
      // It is possible to have elements from different universes in the same process. Getting an element should produce an element from
      // the correct universe
      val e7 = proc3.get(2)
      val e8 = proc3.get(4)
      e7.universe should equal (u1)
      e8.universe should equal (u2)
      val e9 = proc3.map(!_)(2)
      val e10 = proc3.map(!_)(4)
      e9.universe should equal (u1)
      e10.universe should equal (u2)
      val e11 = proc3.chain(if (_) Flip(0.3) else Flip(0.6))(2)
      val e12 = proc3.chain(if (_) Flip(0.3) else Flip(0.6))(4)
      e11.universe should equal (u1)
      e12.universe should equal (u2)
      val fsa3 = fsa1.concat(fsa2)
      // It is possible to have elements from different universes in the same process. Getting an element should produce an element from
      // the correct universe
      val e13 = fsa3.get(0)
      val e14 = fsa3.get(1)
      e13.universe should equal (u1)
      e14.universe should equal (u2)
      val e15 = fsa3.map(!_)(0)
      val e16 = fsa3.map(!_)(1)
      e15.universe should equal (u1)
      e16.universe should equal (u2)
      val e17 = fsa3.chain(if (_) Flip(0.3) else Flip(0.6))(0)
      val e18 = fsa3.chain(if (_) Flip(0.3) else Flip(0.6))(1)
      e17.universe should equal (u1)
      e18.universe should equal (u2)
    }

    "check range correctly" in {
      Universe.createNew()
      val proc = createContainer(List(2,3))
      proc.rangeCheck(2) should equal (true)
      proc.rangeCheck(1) should equal (false)
    }

    "generate the correct elements" in {
      Universe.createNew()
      val proc = createContainer(List(2,3))
      val elems = proc.elements
      assert(elems(0).isInstanceOf[AtomicFlip])
      elems(0).asInstanceOf[AtomicFlip].prob should equal (0.5)
      assert(elems(1).isInstanceOf[AtomicFlip])
      elems(1).asInstanceOf[AtomicFlip].prob should be ((1.0/3) +- 0.0000000001)
      elems.length should equal (2)
    }

    "generate the same elements each time" in {
      Universe.createNew()
      val proc = createContainer(List(2,3))
      val elems1 = proc.elements
      val elems2 = proc.elements
      elems1(0) should equal (elems2(0))
      elems1(1) should equal (elems2(1))
    }

    "generate the correct map" in {
      Universe.createNew()
      val proc = createContainer(List(2,3))
    }

    "when mapping, have each point mapped according to the function" in {
      Universe.createNew()
      val proc = createContainer(List(2,3)).map(!_)
      val elem = proc(3)
      VariableElimination.probability(elem, true) should be ((2.0 / 3) +- 0.000000001)
    }

    "when chaining, have each point flatMapped according to the function" in {
      Universe.createNew()
      val proc = createContainer(List(2,3)).chain(if (_) Flip(0.3) else Flip(0.6))
      val elem = proc(3)
      VariableElimination.probability(elem, true) should be ((1.0 / 3 * 0.3 + 2.0 / 3 * 0.6) +- 0.000000001)
    }

    "when appending, have all the elements of both processes, with the second process replacing the first when necessary" in {
      Universe.createNew()
      val proc1 = createContainer(List(2,3))
      val proc2 = createContainer(List(3,4), true)
      val proc = proc1 ++ proc2
      val elem2 = proc(2)
      val elem3 = proc(3)
      val elem4 = proc(4)
      an [proc.IndexOutOfRangeException] should be thrownBy { proc(1) }
      val alg = VariableElimination(elem2, elem3, elem4)
      alg.start()
      alg.probability(elem2, true) should be (0.5 +- 0.000001)
      alg.probability(elem3, true) should be (2.0 / 3.0 +- 0.00000001) // inverted
      alg.probability(elem4, true) should be (3.0 / 4.0 +- 0.00000001) // inverted
      alg.kill()
    }

    "when folding or reducing, have the points folded according to the function" in {
      Universe.createNew()
      val proc = createContainer(List(2,3))
      val elem1 = proc.foldLeft(true)((b1: Boolean, b2: Boolean) => b1 && b2)
      val elem2 = proc.foldRight(true)((b1: Boolean, b2: Boolean) => b1 && b2)
      val elem3 = proc.reduce((b1: Boolean, b2: Boolean) => b1 && b2)
      val elem4 = proc.aggregate(true)((b1: Boolean, b2: Boolean) => !b1 || b2, (b1: Boolean, b2: Boolean) => b1 && b2)
      val alg = Importance(10000, elem1, elem2, elem3, elem4)
      alg.start()
      alg.probability(elem1, true) should be (((1.0 / 2.0) * (1.0 / 3.0)) +- 0.02)
      alg.probability(elem2, true) should be (((1.0 / 2.0) * (1.0 / 3.0)) +- 0.02)
      alg.probability(elem3, true) should be (((1.0 / 2.0) * (1.0 / 3.0)) +- 0.02)
      alg.probability(elem4, true) should be (((1.0 / 2.0) * (1.0 / 3.0)) +- 0.02)
      alg.kill()
      VariableElimination.probability(elem1, true) should be (((1.0 / 2.0) * (1.0 / 3.0)) +- 0.02)
      MetropolisHastings.probability(elem1, true) should  be (((1.0 / 2.0) * (1.0 / 3.0)) +- 0.02)
    }

    "when quantifying over elements satisfying a predicate, produce the right answer" in {
      Universe.createNew()
      val proc = createContainer(List(2,3))
      val elem1 = proc.count((b: Boolean) => b)
      val elem2 = proc.exists((b: Boolean) => b)
      val elem3 = proc.forall((b: Boolean) => b)
      val alg = Importance(10000, elem1, elem2, elem3)
      alg.start()
      val p0 = 1.0 / 2 * 2.0 / 3
      val p1 = 1.0 / 2 * 1.0 / 3 + 1.0 / 2 * 2.0 / 3
      val p2 = 1.0 / 2 * 1.0 / 3
      alg.probability(elem1, 0) should be (p0 +- 0.02)
      alg.probability(elem1, 1) should be (p1 +- 0.02)
      alg.probability(elem1, 2) should be (p2 +- 0.02)
      alg.probability(elem2, true) should be ((p1 + p2) +- 0.02)
      alg.probability(elem3, true) should be (p2 +- 0.02)
      alg.kill()
    }

    "when finding the index of the first element, produce the right answer" in {
      Universe.createNew()
      val proc = createContainer(List(2,3))
      val elem = proc.findIndex((b: Boolean) => b)
      val alg = Importance(10000, elem)
      alg.start()
      val p2 = 1.0 / 2.0
      val p3 = (1 - p2) * 1.0 / 3.0
      val pNone = 1 - p2 - p3
      alg.probability(elem, Some(2)) should be (p2 +- 0.02)
      alg.probability(elem, Some(3)) should be (p3 +- 0.02)
      alg.probability(elem, None) should be (pNone +- 0.02)
      alg.kill()
    }

    "when concatenating, have all the elements of both processes, with the second process following the first" in {
      Universe.createNew()
      val fsa1 = new FixedSizeArray(2, i => Constant(i))
      val fsa2 = new FixedSizeArray(2, i => Constant(i + 2))
      val fsa = fsa1.concat(fsa2)
      fsa.indices.toList should equal (List(0, 1, 2, 3))
      val e0 = fsa(0)
      val e1 = fsa(1)
      val e2 = fsa(2)
      val e3 = fsa(3)
      assert(e0.isInstanceOf[Constant[Int]])
      e0.asInstanceOf[Constant[Int]].constant should equal (0)
      assert(e2.isInstanceOf[Constant[Int]])
      e2.asInstanceOf[Constant[Int]].constant should equal (2)
    }

    "when choosing a random element, choose one of the elements uniformly at random" in {
      Universe.createNew()
      val fsa = new FixedSizeArray(2, i => Constant(i))
      val elem = fsa.randomElement
      VariableElimination.probability(elem, 1) should be (0.5 +- 0.00000001)
    }

    "when choosing two random elements, have them be independent" in {
      Universe.createNew()
      val fsa = new FixedSizeArray(2, i => Constant(i))
      val elem1 = fsa.randomElement()
      val elem2 = fsa.randomElement()
      val eq = elem1 === elem2
      VariableElimination.probability(eq, true) should be (0.5 +- 0.000000001)
    }
  }

  def createContainer(is: List[Int], invert: Boolean = false): Container[Int, Boolean] = new Container[Int, Boolean] {
    val universe = Universe.universe
    val indices = is
    def generate(index: Int) = if (invert) Flip(1.0 - 1.0 / index)("", universe) else Flip(1.0 / index)("", universe)
    def generate(indices: List[Int]) = {
      val unary = for {
        index <- indices
      } yield (index, generate(index))
      val map = Map(unary:_*)
      val binary =
        for {
          index1 <- indices
          index2 <- indices
          if index1 < index2
        } yield {
          val elem1 = map(index1)
          val elem2 = map(index2)
          val pair = ^^(elem1, elem2)("", universe)
          pair.addConstraint((pair: (Boolean, Boolean)) => if (pair._1 != pair._2) 1.0 / (index1 + index2) else 1.0)
          pair
        }
      Map(unary:_*)
    }
  }
}
