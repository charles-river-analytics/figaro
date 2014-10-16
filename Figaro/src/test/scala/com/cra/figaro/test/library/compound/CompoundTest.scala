/*
 * CompoundTest.scala  
 * Compound element tests.
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Jan 1, 2009
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.test.library.compound

import org.scalatest.WordSpec
import org.scalatest.Matchers
import com.cra.figaro.algorithm._
import com.cra.figaro.algorithm.factored._
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.language._
import com.cra.figaro.library.compound._
import com.cra.figaro.library.atomic.continuous.Uniform
import com.cra.figaro.library.atomic.discrete.Geometric

class CompoundTest extends WordSpec with Matchers {
  "A FastIf" should {
    "have value equal to the then clause if the test is true and the else clause if the test is false" in {
      Universe.createNew()
      val f = Flip(0.3)
      val i = If(f, 1, 2)
      f.value = true
      i.generate()
      i.value should equal(1)
      f.value = false
      i.generate()
      i.value should equal(2)
    }

    "convert to the correct string" in {
      Universe.createNew()
      If(Flip(0.3), 1, 2).toString should equal("FastIf(Flip(0.3), 1, 2)")
    }
  }

  "An If" should {
    "have value equal to the value of the then clause if the test is true " +
      "and the value of the else clause if the test is false" in {
        Universe.createNew()
        val f = Flip(0.3)
        val u = Uniform(0.0, 1.0)
        val c = Constant(0.8)
        val i = If(f, u, c)
        u.value = 0.7
        f.value = true
        i.generate()
        i.value should equal(0.7)
        f.value = false
        i.generate()
        i.value should equal(0.8)
      }

    "when non-finite convert to the correct string" in {
      Universe.createNew()
      val s = If(Flip(0.3), Uniform(0.0, 1.0), Constant(0.8)).toString
      s should equal("If(Flip(0.3), Uniform(0.0, 1.0), Constant(0.8))")
    }
  }

  "A pair" should {
    "have value the pair of its arguments when given two arguments" in {
      Universe.createNew()
      val c1 = Constant("a")
      val c2 = Constant(1)
      val t = ^^(c1, c2)
      Forward(Universe.universe)
      t.value should equal("a", 1)
    }

    "support extracting the first and second component" in {
      Universe.createNew()
      val c1 = Constant(1)
      val c2 = Constant('a')
      val d = ^^(c1, c2)
      val d1 = d._1
      val d2 = d._2
      Forward(Universe.universe)
      d1.value should equal(1)
      d2.value should equal('a')
    }
  }

  "A triple" should {
    "have value the triple of its arguments given three arguments" in {
      Universe.createNew()
      val c1 = Constant("a")
      val c2 = Constant(1)
      val c3 = Constant(true)
      val t = ^^(c1, c2, c3)
      Forward(Universe.universe)
      t.value should equal("a", 1, true)
    }

    "support extracting the three components" in {
      Universe.createNew()
      val t = ^^(Constant("a"), Constant(1), Constant(true))
      val t1 = t._1
      val t2 = t._2
      val t3 = t._3
      Forward(Universe.universe)
      t1.value should equal("a")
      t2.value should equal(1)
      t3.value should equal(true)
    }
  }

  "A quadruple" should {
    "have value the quadruple of its arguments given four arguments" in {
      Universe.createNew()
      val c1 = Constant("a")
      val c2 = Constant(1)
      val c3 = Constant(true)
      val c4 = Constant(8.0)
      Forward(Universe.universe)
      val t = ^^(c1, c2, c3, c4)
      t.generate()
      t.value should equal("a", 1, true, 8.0)
    }

    "support extracting the four components" in {
      Universe.createNew()
      val t = ^^(Constant("a"), Constant(1), Constant(true), Constant(8.0))
      val t1 = t._1
      val t2 = t._2
      val t3 = t._3
      val t4 = t._4
      Forward(Universe.universe)
      t1.value should equal("a")
      t2.value should equal(1)
      t3.value should equal(true)
      t4.value should equal(8.0)
    }
  }

  "A quintuple" should {
    "have value the quintuple of its arguments given five arguments" in {
      Universe.createNew()
      val c1 = Constant("a")
      val c2 = Constant(1)
      val c3 = Constant(true)
      val c4 = Constant(8.0)
      val c5 = Constant(2)
      val t = ^^(c1, c2, c3, c4, c5)
      Forward(Universe.universe)
      t.value should equal("a", 1, true, 8.0, 2)
    }

    "support extracting the five components" in {
      Universe.createNew()
      val t = ^^(Constant("a"), Constant(1), Constant(true), Constant(8.0), Constant(2))
      val t1 = t._1
      val t2 = t._2
      val t3 = t._3
      val t4 = t._4
      val t5 = t._5
      Forward(Universe.universe)
      t1.value should equal("a")
      t2.value should equal(1)
      t3.value should equal(true)
      t4.value should equal(8.0)
      t5.value should equal(2)
    }
  }

  "A BooleanElement" should {
    "support conjunction, disjunction, and negation" in {
      Universe.createNew()
      val b1 = Constant(true)
      val b2 = Constant(false)
      val b1And1 = b1 && b1
      val b1And2 = b1 && b2
      val b1Or2 = b1 || b2
      val b2Or2 = b2 || b2
      val negB1 = !b1
      val negB2 = !b2
      Forward(Universe.universe)
      b1And1.value should equal(true)
      b1And2.value should equal(false)
      b1Or2.value should equal(true)
      b2Or2.value should equal(false)
      negB1.value should equal(false)
      negB2.value should equal(true)
    }
  }

  "A DoubleElement" should {
    "support addition, subtraction, multiplication, division, and negation" in {
      Universe.createNew()
      val d1 = Constant(1.0)
      val d2 = Constant(2.0)
      val d1Plus2 = d1 ++ d2
      val d1Minus2 = d1 - d2
      val d1Times2 = d1 * d2
      val d1Div2 = d1 / d2
      val negD2 = -d2
      Forward(Universe.universe)
      d1Plus2.value should equal(3.0)
      d1Minus2.value should equal(-1.0)
      d1Times2.value should equal(2.0)
      d1Div2.value should equal(0.5)
      negD2.value should equal(-2.0)
    }
  }

  "An IntElement" should {
    "support addition, subtraction, multiplication, division, remainder, and negation" in {
      Universe.createNew()
      val i1 = Constant(2)
      val i2 = Constant(5)
      val i1Plus2 = i1 ++ i2
      val i1Minus2 = i1 - i2
      val i1Times2 = i1 * i2
      val i2Div1 = i2 / i1
      val i2Mod1 = i2 % i1
      val negI2 = -i2
      Forward(Universe.universe)
      i1Plus2.value should equal(7)
      i1Minus2.value should equal(-3)
      i1Times2.value should equal(10)
      i2Div1.value should equal(2)
      i2Mod1.value should equal(1)
      negI2.value should equal(-5)
    }
  }

  "A CPD with one argument" should {
    "produce a result with the expectation over the parent of the probability of the child" in {
      Universe.createNew()
      val x = Flip(0.2)
      val y = CPD(x, false -> Flip(0.1), true -> Flip(0.7))
      val alg = VariableElimination(y)
      alg.start()
      alg.probability(y, true) should be((0.2 * 0.7 + 0.8 * 0.1) +- 0.00000000001)
    }

    "throw MatchError if no clause exists for a given parent value" in {
      Universe.createNew()
      val x = Flip(0.2)
      an [MatchError] should be thrownBy {
        val y = CPD(x, true -> Flip(0.7))
        val alg = VariableElimination(y)
        alg.start()
      } 
    }
  }

  "A CPD with two arguments" should {
    "produce a result with the expectation over the parents of the probability of the child" in {
      Universe.createNew()
      val x1 = Flip(0.2)
      val x2 = Select(0.2 -> 1, 0.3 -> 2, 0.5 -> 3)
      val y = CPD(x1, x2, (false, 1) -> Flip(0.1),
        (false, 2) -> Flip(0.2),
        (false, 3) -> Flip(0.3),
        (true, 1) -> Flip(0.4),
        (true, 2) -> Flip(0.5),
        (true, 3) -> Flip(0.6))
      val alg = VariableElimination(y)
      alg.start()
      alg.probability(y, true) should be((0.2 * (0.2 * 0.4 + 0.3 * 0.5 + 0.5 * 0.6) +
        0.8 * (0.2 * 0.1 + 0.3 * 0.2 + 0.5 * 0.3)) +- 0.00000000001)
    }

    "throw MatchError if no clause exists for a given parent value" in {
      Universe.createNew()
      val x1 = Flip(0.2)
      val x2 = Select(0.2 -> 1, 0.3 -> 2, 0.5 -> 3)
      an [MatchError] should be thrownBy {
        val y = CPD(x1, x2, (false, 1) -> Flip(0.1),
          (false, 3) -> Flip(0.3),
          (true, 1) -> Flip(0.4),
          (true, 2) -> Flip(0.5),
          (true, 3) -> Flip(0.6))
        val alg = VariableElimination(y)
        alg.start()
      } 
    }
  }

  "A CPD with three arguments" should {
    "produce a result with the expectation over the parents of the probability of the child" in {
      Universe.createNew()
      val x1 = Flip(0.2)
      val x2 = Select(0.2 -> 1, 0.3 -> 2, 0.5 -> 3)
      val x3 = Constant(7)
      val y = CPD(x1, x2, x3, (false, 1, 7) -> Flip(0.1),
        (false, 2, 7) -> Flip(0.2),
        (false, 3, 7) -> Flip(0.3),
        (true, 1, 7) -> Flip(0.4),
        (true, 2, 7) -> Flip(0.5),
        (true, 3, 7) -> Flip(0.6))
      val alg = VariableElimination(y)
      alg.start()
      alg.probability(y, true) should be((0.2 * (0.2 * 0.4 + 0.3 * 0.5 + 0.5 * 0.6) +
        0.8 * (0.2 * 0.1 + 0.3 * 0.2 + 0.5 * 0.3)) +- 0.00000000001)
    }

    "throw MatchError if no clause exists for a given parent value" in {
      Universe.createNew()
      val x1 = Flip(0.2)
      val x2 = Select(0.2 -> 1, 0.3 -> 2, 0.5 -> 3)
      val x3 = Constant(7)
      an [MatchError] should be thrownBy {
        val y = CPD(x1, x2, x3, (false, 1, 7) -> Flip(0.1),
          (false, 3, 7) -> Flip(0.3),
          (true, 1, 7) -> Flip(0.4),
          (true, 2, 7) -> Flip(0.5),
          (true, 3, 7) -> Flip(0.6))
        val alg = VariableElimination(y)
        alg.start()
      } 
    }
  }

  "A CPD with four arguments" should {
    "produce a result with the expectation over the parents of the probability of the child" in {
      Universe.createNew()
      val x1 = Flip(0.2)
      val x2 = Select(0.2 -> 1, 0.3 -> 2, 0.5 -> 3)
      val x3 = Constant(7)
      val x4 = Flip(0.9)
      val y = CPD(x1, x2, x3, x4, (false, 1, 7, false) -> Flip(0.1),
        (false, 2, 7, false) -> Flip(0.2),
        (false, 3, 7, false) -> Flip(0.3),
        (true, 1, 7, false) -> Flip(0.4),
        (true, 2, 7, false) -> Flip(0.5),
        (true, 3, 7, false) -> Flip(0.6),
        (false, 1, 7, true) -> Flip(0.15),
        (false, 2, 7, true) -> Flip(0.25),
        (false, 3, 7, true) -> Flip(0.35),
        (true, 1, 7, true) -> Flip(0.45),
        (true, 2, 7, true) -> Flip(0.55),
        (true, 3, 7, true) -> Flip(0.65))
      val alg = VariableElimination(y)
      alg.start()
      alg.probability(y, true) should be((0.2 * 0.9 * (0.2 * 0.45 + 0.3 * 0.55 + 0.5 * 0.65) +
        0.2 * 0.1 * (0.2 * 0.4 + 0.3 * 0.5 + 0.5 * 0.6) +
        0.8 * 0.9 * (0.2 * 0.15 + 0.3 * 0.25 + 0.5 * 0.35) +
        0.8 * 0.1 * (0.2 * 0.1 + 0.3 * 0.2 + 0.5 * 0.3))
        +- 0.00000000001)
    }

    "throw MatchError if no clause exists for a given parent value" in {
      Universe.createNew()
      val x1 = Flip(0.2)
      val x2 = Select(0.2 -> 1, 0.3 -> 2, 0.5 -> 3)
      val x3 = Constant(7)
      val x4 = Flip(0.9)
      an [MatchError] should be thrownBy {
        val y = CPD(x1, x2, x3, x4, (false, 1, 7, false) -> Flip(0.1),
          (false, 3, 7, false) -> Flip(0.3),
          (true, 1, 7, false) -> Flip(0.4),
          (true, 2, 7, false) -> Flip(0.5),
          (true, 3, 7, false) -> Flip(0.6),
          (false, 1, 7, true) -> Flip(0.15),
          (false, 2, 7, true) -> Flip(0.25),
          (false, 3, 7, true) -> Flip(0.35),
          (true, 1, 7, true) -> Flip(0.45),
          (true, 2, 7, true) -> Flip(0.55),
          (true, 3, 7, true) -> Flip(0.65))
        val alg = VariableElimination(y)
        alg.start()
      } 
    }
  }

  "A CPD with five arguments" should {
    "produce a result with the expectation over the parents of the probability of the child" in {
      Universe.createNew()
      val x1 = Flip(0.2)
      val x2 = Select(0.2 -> 1, 0.3 -> 2, 0.5 -> 3)
      val x3 = Constant(7)
      val x4 = Flip(0.9)
      val x5 = Constant('a)
      val y = CPD(x1, x2, x3, x4, x5, (false, 1, 7, false, 'a) -> Flip(0.1),
        (false, 2, 7, false, 'a) -> Flip(0.2),
        (false, 3, 7, false, 'a) -> Flip(0.3),
        (true, 1, 7, false, 'a) -> Flip(0.4),
        (true, 2, 7, false, 'a) -> Flip(0.5),
        (true, 3, 7, false, 'a) -> Flip(0.6),
        (false, 1, 7, true, 'a) -> Flip(0.15),
        (false, 2, 7, true, 'a) -> Flip(0.25),
        (false, 3, 7, true, 'a) -> Flip(0.35),
        (true, 1, 7, true, 'a) -> Flip(0.45),
        (true, 2, 7, true, 'a) -> Flip(0.55),
        (true, 3, 7, true, 'a) -> Flip(0.65))
      val alg = VariableElimination(y)
      alg.start()
      alg.probability(y, true) should be((0.2 * 0.9 * (0.2 * 0.45 + 0.3 * 0.55 + 0.5 * 0.65) +
        0.2 * 0.1 * (0.2 * 0.4 + 0.3 * 0.5 + 0.5 * 0.6) +
        0.8 * 0.9 * (0.2 * 0.15 + 0.3 * 0.25 + 0.5 * 0.35) +
        0.8 * 0.1 * (0.2 * 0.1 + 0.3 * 0.2 + 0.5 * 0.3))
        +- 0.00000000001)
    }

    "throw MatchError if no clause exists for a given parent value" in {
      Universe.createNew()
      val x1 = Flip(0.2)
      val x2 = Select(0.2 -> 1, 0.3 -> 2, 0.5 -> 3)
      val x3 = Constant(7)
      val x4 = Flip(0.9)
      val x5 = Constant('a)
      an [MatchError] should be thrownBy {
        val y = CPD(x1, x2, x3, x4, x5, (false, 1, 7, false, 'a) -> Flip(0.1),
          (false, 3, 7, false, 'a) -> Flip(0.3),
          (true, 1, 7, false, 'a) -> Flip(0.4),
          (true, 2, 7, false, 'a) -> Flip(0.5),
          (true, 3, 7, false, 'a) -> Flip(0.6),
          (false, 1, 7, true, 'a) -> Flip(0.15),
          (false, 2, 7, true, 'a) -> Flip(0.25),
          (false, 3, 7, true, 'a) -> Flip(0.35),
          (true, 1, 7, true, 'a) -> Flip(0.45),
          (true, 2, 7, true, 'a) -> Flip(0.55),
          (true, 3, 7, true, 'a) -> Flip(0.65))
        val alg = VariableElimination(y)
        alg.start()
      } 
    }
  }

  "A Rich CPD with one argument" should {
    "produce a result with the expectation over the parent of the probability of the child" in {
      Universe.createNew()
      val x = Select(0.1 -> 1, 0.2 -> 2, 0.3 -> 3, 0.4 -> 4)
      val y = RichCPD(x, OneOf(1, 2) -> Flip(0.1), NoneOf(4) -> Flip(0.7), * -> Flip(0.9))
      val alg = VariableElimination(y)
      alg.start()
      alg.probability(y, true) should be(((0.1 + 0.2) * 0.1 + 0.3 * 0.7 + 0.4 * 0.9) +- 0.00000000001)
    }
  }

  "A Rich CPD with two arguments" should {
    "produce a result with the expectation over the parent of the probability of the child" in {
      Universe.createNew()
      val x1 = Select(0.1 -> 1, 0.2 -> 2, 0.3 -> 3, 0.4 -> 4)
      val x2 = Flip(0.6)
      val y = RichCPD(x1, x2,
        (OneOf(1, 2), *) -> Flip(0.1),
        (NoneOf(4), OneOf(false)) -> Flip(0.7),
        (*, *) -> Flip(0.9))
      val alg = VariableElimination(y)
      alg.start()
      alg.probability(y, true) should be(((0.1 + 0.2) * 0.1 + 0.3 * 0.4 * 0.7 + (0.3 * 0.6 + 0.4) * 0.9)
        +- 0.00000000001)
    }
  }

  "A Rich CPD with three arguments" should {
    "produce a result with the expectation over the parent of the probability of the child" in {
      Universe.createNew()
      val x1 = Select(0.1 -> 1, 0.2 -> 2, 0.3 -> 3, 0.4 -> 4)
      val x2 = Flip(0.6)
      val x3 = Constant(5)
      val y: Element[Boolean] = RichCPD(x1, x2, x3,
        (OneOf(1, 2), *, OneOf(5)) -> Flip(0.1),
        (NoneOf(4), OneOf(false), *) -> Flip(0.7),
        (*, *, NoneOf(6, 7)) -> Flip(0.9))
      val alg = VariableElimination(y)
      alg.start()
      alg.probability(y, true) should be(((0.1 + 0.2) * 0.1 + 0.3 * 0.4 * 0.7 + (0.3 * 0.6 + 0.4) * 0.9)
        +- 0.00000000001)
    }
  }

  "A Rich CPD with four arguments" should {
    "produce a result with the expectation over the parent of the probability of the child" in {
      Universe.createNew()
      val x1 = Select(0.1 -> 1, 0.2 -> 2, 0.3 -> 3, 0.4 -> 4)
      val x2 = Flip(0.6)
      val x3 = Constant(5)
      val x4 = Flip(0.8)
      val y = RichCPD(x1, x2, x3, x4,
        (OneOf(1, 2), *, OneOf(5), *) -> Flip(0.1),
        (NoneOf(4), OneOf(false), *, *) -> Flip(0.7),
        (*, *, NoneOf(6, 7), OneOf(true)) -> Flip(0.9),
        (*, *, *, OneOf(false)) -> Constant(true))
      val alg = VariableElimination(y)
      alg.start()
      alg.probability(y, true) should be(((0.1 + 0.2) * 0.1 + 0.3 * 0.4 * 0.7 +
        (0.3 * 0.6 + 0.4) * (0.8 * 0.9 + 0.2))
        +- 0.00000000001)
    }
  }

  "A Rich CPD with five arguments" should {
    "produce a result with the expectation over the parent of the probability of the child" in {
      Universe.createNew()
      val x1 = Select(0.1 -> 1, 0.2 -> 2, 0.3 -> 3, 0.4 -> 4)
      val x2 = Flip(0.6)
      val x3 = Constant(5)
      val x4 = Flip(0.8)
      val x5 = Flip(0.5)
      val y: Element[Boolean] = RichCPD(x1, x2, x3, x4, x5,
        (OneOf(1, 2), *, OneOf(5), *, *) -> Flip(0.1),
        (NoneOf(4), OneOf(false), *, *, *) -> Flip(0.7),
        (*, *, NoneOf(6, 7), OneOf(true), *) -> Flip(0.9),
        (*, *, *, OneOf(false), *) -> Constant(true))
      val alg = VariableElimination(y)
      alg.start()
      alg.probability(y, true) should be(((0.1 + 0.2) * 0.1 + 0.3 * 0.4 * 0.7 +
        (0.3 * 0.6 + 0.4) * (0.8 * 0.9 + 0.2))
        +- 0.00000000001)
    }
  }

  "A MakeList" should {
    "have the number of items be distributed according to the first argument" in {
      Universe.createNew()
      val x1 = Geometric(0.9)
      val x2 = MakeList(x1, () => Flip(0.2))
      val x3 = Apply(x2, (vs: List[Boolean]) => vs.length)
      val alg = Importance(10000, x3)
      alg.start()
      val answer = 0.9 * 0.9 * 0.1
      alg.probability(x3, 3) should be(answer +- 0.01)
    }

    "have each item be distributed according to the element generator" in {
      Universe.createNew()
      val x1 = Select(0.5 -> 2, 0.5 -> 3)
      val x2 = MakeList(x1, () => Flip(0.2))
      val x3 = Apply(x2, (vs: List[Boolean]) => vs(0))
      val x4 = Apply(x2, (vs: List[Boolean]) => vs(1))
      val alg = Importance(20000, x3, x4)
      alg.start()
      val answer = 0.2
      alg.probability(x3, true) should be(answer +- 0.01)
      alg.probability(x4, true) should be(answer +- 0.01)
    }

    "have the items be generated independently" in {
      Universe.createNew()
      val x1 = Select(0.5 -> 2, 0.5 -> 3)
      val x2 = MakeList(x1, () => Flip(0.2))
      val x3 = Apply(x2, (vs: List[Boolean]) => vs(0))
      val x4 = Apply(x2, (vs: List[Boolean]) => vs(1))
      val x5 = x3 === x4
      val alg = Importance(20000, x5)
      alg.start()
      val answer = 0.2 * 0.2 + 0.8 * 0.8
      alg.probability(x5, true) should be(answer +- 0.01)
    }

    "have the correct set of possible values" in {
      Universe.createNew()
      val x1 = Select(0.4 -> 2, 0.6 -> 3)
      val x2 = MakeList(x1, () => Flip(0.2))
      Values()(x2) should equal(Set(
        List(false, false), List(false, true), List(true, false), List(true, true),
        List(false, false, false), List(false, false, true), List(false, true, false), List(false, true, true),
        List(true, false, false), List(true, false, true), List(true, true, false), List(true, true, true)))
    }

    "return the correct probability under importance sampling" in {
      Universe.createNew()
      val x1 = Select(0.4 -> 2, 0.6 -> 3)
      val x2 = MakeList(x1, () => Flip(0.2))
      val x3 = Apply(x2, (vs: List[Boolean]) => (false /: vs)((x: Boolean, y: Boolean) => x || y))
      x3.observe(true)
      val alg = Importance(50000, x1)
      alg.start()
      val p2 = 0.4 * (1 - 0.8 * 0.8)
      val p3 = 0.6 * (1 - 0.8 * 0.8 * 0.8)
      val answer = p3 / (p2 + p3)
      alg.probability(x1, 3) should be(answer +- 0.01)
    }

    "return the correct probability under variable elimination" in {
      Universe.createNew()
      val x1 = Select(0.4 -> 2, 0.6 -> 3)
      val x2 = MakeList(x1, () => Flip(0.2))
      //val i2 = Inject(Flip(0.2), Flip(0.2))
      //val i3 = Inject(Flip(0.2), Flip(0.2), Flip(0.2))
      //val x2 = Chain(x1, (i: Int) => if (i == 2) i2; else i3)//CPD(x1, (2) -> i2, (3) -> i3)
      val x3 = Apply(x2, (vs: List[Boolean]) => (false /: vs)((x: Boolean, y: Boolean) => x || y))
      x3.observe(true)
      val alg = VariableElimination(x1)
      alg.start()
      val p2 = 0.4 * (1 - 0.8 * 0.8)
      val p3 = 0.6 * (1 - 0.8 * 0.8 * 0.8)
      val answer = p3 / (p2 + p3)
      alg.probability(x1, 3) should be(answer +- 0.01)
    }

    "return the correct probability under Metropolis-Hastings" in {
      Universe.createNew()
      val x1 = Select(0.4 -> 2, 0.6 -> 3)
      val x2 = MakeList(x1, () => Flip(0.2))
      val x3 = Apply(x2, (vs: List[Boolean]) => (false /: vs)((x: Boolean, y: Boolean) => x || y))
      x3.observe(true)
      val alg = MetropolisHastings(200000, ProposalScheme.default, x1)
      alg.start()
      val p2 = 0.4 * (1 - 0.8 * 0.8)
      val p3 = 0.6 * (1 - 0.8 * 0.8 * 0.8)
      val answer = p3 / (p2 + p3)
      alg.probability(x1, 3) should be(answer +- 0.01)
    }
  }

  "An IntSelector" should {
    "have a value be generated uniformly between 0 and the value of the bound" in {
      Universe.createNew()
      val x1 = Select(0.4 -> 2, 0.6 -> 3)
      val x2 = IntSelector(x1)
      val alg = Importance(20000, x2)
      alg.start()
      alg.probability(x2, 0) should be(0.4 / 2 + 0.6 / 3 +- 0.01)
      alg.probability(x2, 1) should be(0.4 / 2 + 0.6 / 3 +- 0.01)
      alg.probability(x2, 2) should be(0.6 / 3 +- 0.01)
    }

    "avoid changing value when the bound changes" in {
      Universe.createNew()
      val x1 = Geometric(0.9)
      val x2 = IntSelector(x1)
      x2.randomness = Stream(0.6, 0.2, 0.9)
      x1.value = 1
      x2.value = x2.generateValue(x2.randomness)
      x2.value should equal(0)
      x1.value = 2
      x2.value = x2.generateValue(x2.randomness)
      x2.value should equal(0)
      x1.value = 3
      x2.value = x2.generateValue(x2.randomness)
      x2.value should equal(2)
      x1.value = 2
      x2.value = x2.generateValue(x2.randomness)
      x2.value should equal(0)
      x1.value = 1
      x2.value = x2.generateValue(x2.randomness)
      x2.value should equal(0)
    }
    
    "produce the correct values when Values() called" in {
      Universe.createNew()
      val x1 = Select(0.75 -> 3, 0.25 -> 5)
      val x2 = IntSelector(x1)
      Values()(x2) should equal(Set(0, 1, 2, 3, 4))
    }
    
    "produce the correct factors" in {
      def prob(counter: Int, value: Int) = if (value < counter) 1.0/counter else 0.0
      Universe.createNew()
      val clauses = List((0.75, 3), (0.25, 5))
      val x1 = Select(clauses:_*)
      val x2 = IntSelector(x1)
      val alg = VariableElimination(x2)
      alg.start
      val dist = alg.distribution(x2)
      dist.foreach{v =>
        v._1 should be(clauses.filter(v._2 < _._2).map(c => c._1/c._2).sum +- 0.0001)
      }
      
    }
    
  }
}
