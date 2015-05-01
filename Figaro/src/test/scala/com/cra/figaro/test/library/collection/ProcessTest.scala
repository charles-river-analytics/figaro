/*
 * ProcessTest.scala
 * Process tests.
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Nov 27, 2014
 * 
 * Copyright 2014 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.test.library.collection

import org.scalatest.WordSpec
import org.scalatest.Matchers
import com.cra.figaro.library.collection._
import com.cra.figaro.language._
import com.cra.figaro.util._
import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.algorithm.sampling.Importance
import com.cra.figaro.library.compound._

class ProcessTest extends WordSpec with Matchers {
  "A process" should {
    "create elements in the correct universe" in {
      val u1 = Universe.createNew()
      val proc = createProcess(List(2,3))
      val u2 = Universe.createNew()
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
      val proc2 = createProcess(List(4))
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
    }    
    
    "create elements in the correct universe using flatMap instead of chain" in {
      val u1 = Universe.createNew()
      val proc = createProcess(List(2,3))
      val u2 = Universe.createNew()
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
      val e6 = proc.flatMap(if (_) Flip(0.3) else Flip(0.6))(2)
      e6.universe should equal (u1)
      val proc2 = createProcess(List(4))
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
      val e11 = proc3.flatMap(if (_) Flip(0.3) else Flip(0.6))(2)
      val e12 = proc3.flatMap(if (_) Flip(0.3) else Flip(0.6))(4)
      e11.universe should equal (u1)
      e12.universe should equal (u2)
    }

    "get the right element for an index in range" in {
      Universe.createNew()
      val proc = createProcess(List(2,3))
      val elem = proc(2)
      val alg = VariableElimination(elem)
      alg.start()
      alg.probability(elem, true) should be (0.5 +- 0.0000000001)
      alg.kill()
    }

    "correctly cache a single element when using apply" in {
      Universe.createNew()
      val proc = createProcess(List(2,3))
      val elem1 = proc(2)
      val elem2 = proc(2)
      elem1 should equal (elem2)
    }

    "get the right elements for multiple indices in range" in {
      Universe.createNew()
      val proc = createProcess(List(2,3))
      val elems = proc(List(2,3))
      val alg = VariableElimination(elems(2))
      alg.start()
      val q23 = (1.0 / 2) * (1.0 / 3)
      val q2not3 = (1.0 / 2) * (2.0 / 3) * (1.0 / 5)
      val qnot23 = (1.0 / 2) * (1.0 / 3) * (1.0 / 5)
      val qnot2not3 = (1.0 / 2) * (2.0 / 3)
      val q2 = q23 + q2not3
      val qnot2 = qnot23 + qnot2not3
      val p2 = q2 / (q2 + qnot2)
      alg.probability(elems(2), true) should be (p2 +- 0.0000000001)
      alg.kill()
    }

    "throw IndexOutOfRange exception for an index out of range" in {
      Universe.createNew()
      val proc = createProcess(List(2,3))
      an [proc.IndexOutOfRangeException] should be thrownBy { proc(1) }
      an [proc.IndexOutOfRangeException] should be thrownBy { proc(List(2,1)) }
    }

    "get the right optional element for an index in or out of range" in {
      Universe.createNew()
      val proc = createProcess(List(2,3))
      val elem1 = proc.get(2)
      val elem2 = proc.get(1)
      val alg = VariableElimination(elem1, elem2)
      alg.start()
      alg.probability(elem1, Some(true)) should be (0.5 +- 0.00000001)
      alg.probability(elem2, None) should be (1.0 +- 0.000000001)
      alg.kill()
    }

    "get the right optional elements for multiple indices in or out of range" in {
      Universe.createNew()
      val proc = createProcess(List(2,3))
      val elems = proc.get(List(2, 1))
      val alg = VariableElimination(elems(1), elems(2))
      alg.start()
      alg.probability(elems(2), Some(true)) should be (0.5 +- 0.00000001)
      alg.probability(elems(1), None) should be (1.0 +- 0.000000001)
      alg.kill()
    }

    "when mapping, have each point mapped according to the function" in {
      Universe.createNew()
      val proc = createProcess(List(2,3)).map(!_)
      val elem = proc(3)
      val alg = VariableElimination(elem)
      alg.start()
      alg.probability(elem, true) should be ((2.0 / 3) +- 0.000000001)
      alg.kill()
    }

    "when chaining, have each point flatMapped according to the function" in {
      Universe.createNew()
      val proc = createProcess(List(2,3)).chain(if (_) Flip(0.3) else Flip(0.6))
      val elem = proc(3)
      val alg = VariableElimination(elem)
      alg.start()
      alg.probability(elem, true) should be ((1.0 / 3 * 0.3 + 2.0 / 3 * 0.6) +- 0.000000001)
      alg.kill()
    }
    
     "(use flatMap instead of chain) when chaining, have each point flatMapped according to the function" in {
      Universe.createNew()
      val proc = createProcess(List(2,3)).chain(if (_) Flip(0.3) else Flip(0.6))
      val elem = proc(3)
      val alg = VariableElimination(elem)
      alg.start()
      alg.probability(elem, true) should be ((1.0 / 3 * 0.3 + 2.0 / 3 * 0.6) +- 0.000000001)
      alg.kill()
    }

    "when appending, have all the elements of both processes, with the second process replacing the first when necessary" in {
      Universe.createNew()
      val proc1 = createProcess(List(2,3))
      val proc2 = createProcess(List(3,4), true)
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
  }

  def createProcess(indices: List[Int], invert: Boolean = false): Process[Int, Boolean] = new Process[Int, Boolean] {
    val universe = Universe.universe
    def rangeCheck(index: Int) = indices.contains(index)
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
