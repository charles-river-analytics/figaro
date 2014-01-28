/*
 * ValuesTest.scala
 * Range computation tests.
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
import com.cra.figaro.library.atomic.continuous._
import com.cra.figaro.library.compound._

class ValuesTest extends WordSpec with Matchers {
  "Values.Values" when {
    "given a Constant element" should {
      "return a set containing only the constant value" in {
        Universe.createNew()
        val s: Set[Int] = Values()(Constant(8))
        s should contain(8)
        s.size should equal(1)
      }
    }

    "given a Flip with constant probabilities" should {
      "return a set containing true and false" in {
        Universe.createNew()
        val s: Set[Boolean] = Values()(Flip(0.7))
        s should contain(true)
        s should contain(false)
        s.size should equal(2)
      }
    }

    "given a Flip with elements for probabilities" should {
      "return a set containing true and false" in {
        Universe.createNew()
        val s: Set[Boolean] = Values()(Flip(Uniform(0.7, 0.8)))
        s should contain(true)
        s should contain(false)
        s.size should equal(2)
      }
    }

    "given a Select with constant probabilities" should {
      "return a set containing the outcomes" in {
        Universe.createNew()
        val s: Set[Int] = Values()(Select(0.3 -> 1, 0.2 -> 2, 0.5 -> 3))
        s should contain(1)
        s should contain(2)
        s should contain(3)
        s.size should equal(3)
      }
    }

    "given a Select with elements for probabilities" should {
      "return a set containing the outcomes" in {
        Universe.createNew()
        val s: Set[Int] = Values()(Select(Uniform(0.3, 0.4) -> 1, Constant(0.2) -> 2, Constant(0.5) -> 3))
        s should contain(1)
        s should contain(2)
        s should contain(3)
        s.size should equal(3)
      }
    }

    "given a Dist with constant probabilities and outcomes of enumerable range" should {
      "return the union of the outcomes' value sets" in {
        Universe.createNew()
        val s: Set[Int] = Values()(Dist(0.3 -> Select(0.1 -> 1, 0.9 -> 2), 0.7 -> Select(0.5 -> 3, 0.5 -> 2)))
        s should contain(1)
        s should contain(2)
        s should contain(3)
        s.size should equal(3)
      }
    }

    "given a Dist with elements for probabilities and outcomes of enumerable range" should {
      "return the union of the outcomes' value sets" in {
        Universe.createNew()
        val d = Dist(Uniform(0.3, 0.4) -> Select(0.1 -> 1, 0.9 -> 2),
          Constant(0.7) -> Select(0.5 -> 3, 0.5 -> 2))
        val s: Set[Int] = Values()(d)
        s should contain(1)
        s should contain(2)
        s should contain(3)
        s.size should equal(3)
      }
    }

    "given a Chain with enumerable argument and results" should {
      "return the union of the results' value sets" in {
        Universe.createNew()
        val f1 = Flip(0.7)
        val d1 = Select(0.1 -> 1, 0.2 -> 2)
        val d2 = Select(0.2 -> 2, 0.3 -> 3)
        val s: Set[Int] = Values()(Chain(f1, (b: Boolean) => if (b) d1; else d2))
        s should contain(1)
        s should contain(2)
        s should contain(3)
        s.size should equal(3)
      }
    }

    "given an Apply with one enumerable argument" should {
      "return the image of its argument's value set" in {
        Universe.createNew()
        val s: Set[Int] = Values()(Apply(Constant(8), (x: Int) => x + 1))
        s should contain(9)
        s.size should equal(1)
      }
    }

    "given an Apply with two enumerable arguments" should {
      "return the image of the product of its arguments' values sets" in {
        Universe.createNew()
        def fn(b: Boolean, i: Int) = if (b) i; else i + 1
        val s: Set[Int] = Values()(Apply(Flip(0.4), Select(0.1 -> 1, 0.2 -> 2, 0.3 -> 3), fn _))
        s should contain(1)
        s should contain(2)
        s should contain(3)
        s should contain(4)
        s.size should equal(4)
      }
    }

    "given an Apply with three enumerable arguments" should {
      "return the image of the product of its arguments' values sets" in {
        Universe.createNew()
        def fn(b: Boolean, i: Int, j: Int) = if (b) i; else i + j
        val s: Set[Int] = Values()(Apply(Flip(0.4), Select(0.1 -> 1, 0.2 -> 2, 0.3 -> 3), Constant(1), fn _))
        s should contain(1)
        s should contain(2)
        s should contain(3)
        s should contain(4)
        s.size should equal(4)
      }
    }

    "given an Apply with four enumerable arguments" should {
      "return the image of the product of its arguments' values sets" in {
        Universe.createNew()
        def fn(b: Boolean, i: Int, j: Int, k: Int) = if (b) i + j; else i + k
        val s: Set[Int] = Values()(Apply(Flip(0.4),
          Select(0.1 -> 1, 0.2 -> 2, 0.3 -> 3),
          Constant(1),
          Select(0.5 -> 4, 0.5 -> 5),
          fn _))
        s should contain(2)
        s should contain(3)
        s should contain(4)
        s should contain(5)
        s should contain(6)
        s should contain(7)
        s should contain(8)
        s.size should equal(7)
      }
    }

    "given an Apply with five enumerable arguments" should {
      "return the image of the product of its arguments' values sets" in {
        Universe.createNew()
        def fn(b: Boolean, i: Int, j: Int, k: Int, l: Boolean) = if (b || l) i + j; else i + k
        val s: Set[Int] = Values()(Apply(Flip(0.4),
          Select(0.1 -> 1, 0.2 -> 2, 0.3 -> 3),
          Constant(1),
          Select(0.5 -> 4, 0.5 -> 5),
          Flip(0.7),
          fn _))
        s should contain(2)
        s should contain(3)
        s should contain(4)
        s should contain(5)
        s should contain(6)
        s should contain(7)
        s should contain(8)
        s.size should equal(7)
      }
    }

    "given an If with constant consequents" should {
      "return a set containing the two consequents" in {
        Universe.createNew()
        val s: Set[Int] = Values()(If(Flip(0.3), 1, 2))
        s should contain(1)
        s should contain(2)
        s.size should equal(2)
      }
    }

    "given an If with variable consequents" should {
      "return the union of the two consequents' value sets" in {
        Universe.createNew()
        val s: Set[Int] = Values()(If(Flip(0.3), Select(0.1 -> 1, 0.2 -> 2), Select(0.3 -> 3, 0.2 -> 2)))
        s should contain(1)
        s should contain(2)
        s should contain(3)
        s.size should equal(3)
      }
    }

    "given an Inject" should {
      "return the cartesian product of its input" in {
        Universe.createNew()
        val s: Set[List[Int]] = Values()(Inject(Select(0.5 -> 1, 0.5 -> 2), Select(0.2 -> 3, 0.3 -> 4, 0.5 -> 5)))
        assert(s exists ((q: Seq[Int]) => q.toList == List(1, 3)))
        assert(s exists ((q: Seq[Int]) => q.toList == List(1, 4)))
        assert(s exists ((q: Seq[Int]) => q.toList == List(1, 5)))
        assert(s exists ((q: Seq[Int]) => q.toList == List(2, 3)))
        assert(s exists ((q: Seq[Int]) => q.toList == List(2, 4)))
        assert(s exists ((q: Seq[Int]) => q.toList == List(2, 5)))
        s.size should equal(6)
      }
    }

    "given a non-enumerable element such as Uniform" should {
      "throw UnsupportedAlgorithmException" in {
        Universe.createNew()
        evaluating { Values()(Uniform(0.0, 1.0)) } should produce[UnsupportedAlgorithmException]
      }
    }

    "called on the same element twice" should {
      "reuse its work from the first call for the second call" in {
        Universe.createNew()
        var sum = 0
        def fn(b: Boolean) = {
          sum += 1
          if (b) Constant(1); else Constant(2)
        }
        val c = Chain(Flip(0.5), fn _)
        Values()(c)
        var s1 = sum
        Values()(c)
        sum should equal(s1)
      }
    }
  }
}
