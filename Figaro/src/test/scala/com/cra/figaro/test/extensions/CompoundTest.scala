package com.cra.figaro.test.extensions

import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.extensions.CPD
import com.cra.figaro.language._
import com.cra.figaro.library.compound.{CPD => _, _}
import org.scalatest.Matchers
import org.scalatest.WordSpec

class CompoundTest extends WordSpec with Matchers {
  "A CPD with one argument" should {
    "produce a result with the expectation over the parent of the probability of the child" in {
      Universe.createNew()
      val x = Flip(0.2)
      val y = CPD(x) {
        case false => Flip(0.1)
        case true => Flip(0.7)
      }
      val alg = VariableElimination(y)
      alg.start()
      alg.probability(y, true) should be((0.2 * 0.7 + 0.8 * 0.1) +- 0.00000000001)
    }

    "throw MatchError if no clause exists for a given parent value" in {
      Universe.createNew()
      val x = Flip(0.2)
      an [MatchError] should be thrownBy {
        val y = CPD(x){
          case true => Flip(0.7)
        }
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
      val y = CPD(x1, x2) {
        case (false, 1) => Flip(0.1)
        case (false, 2) => Flip(0.2)
        case (false, 3) => Flip(0.3)
        case (true, 1) => Flip(0.4)
        case (true, 2) => Flip(0.5)
        case (true, 3) => Flip(0.6)
      }
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
        val y = CPD(x1, x2) {
          case (false, 1) => Flip(0.1)
          case (false, 3) => Flip(0.3)
          case (true, 1) => Flip(0.4)
          case (true, 2) => Flip(0.5)
          case (true, 3) => Flip(0.6)
        }
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
      val y = CPD(x1, x2, x3) {
        case (false, 1, 7) => Flip(0.1)
        case (false, 2, 7) => Flip(0.2)
        case (false, 3, 7) => Flip(0.3)
        case (true, 1, 7) => Flip(0.4)
        case (true, 2, 7) => Flip(0.5)
        case (true, 3, 7) => Flip(0.6)
      }
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
        val y = CPD(x1, x2, x3) {
          case (false, 1, 7) => Flip(0.1)
          case (false, 3, 7) => Flip(0.3)
          case (true, 1, 7) => Flip(0.4)
          case (true, 2, 7) => Flip(0.5)
          case (true, 3, 7) => Flip(0.6)
        }
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
      val y = CPD(x1, x2, x3, x4) {
        case (false, 1, 7, false) => Flip(0.1)
        case (false, 2, 7, false) => Flip(0.2)
        case (false, 3, 7, false) => Flip(0.3)
        case (true, 1, 7, false) => Flip(0.4)
        case (true, 2, 7, false) => Flip(0.5)
        case (true, 3, 7, false) => Flip(0.6)
        case (false, 1, 7, true) => Flip(0.15)
        case (false, 2, 7, true) => Flip(0.25)
        case (false, 3, 7, true) => Flip(0.35)
        case (true, 1, 7, true) => Flip(0.45)
        case (true, 2, 7, true) => Flip(0.55)
        case (true, 3, 7, true) => Flip(0.65)
      }
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
        val y = CPD(x1, x2, x3, x4) {
          case (false, 1, 7, false) => Flip(0.1)
          case (false, 3, 7, false) => Flip(0.3)
          case (true, 1, 7, false) => Flip(0.4)
          case (true, 2, 7, false) => Flip(0.5)
          case (true, 3, 7, false) => Flip(0.6)
          case (false, 1, 7, true) => Flip(0.15)
          case (false, 2, 7, true) => Flip(0.25)
          case (false, 3, 7, true) => Flip(0.35)
          case (true, 1, 7, true) => Flip(0.45)
          case (true, 2, 7, true) => Flip(0.55)
          case (true, 3, 7, true) => Flip(0.65)
        }
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
      val y = CPD(x1, x2, x3, x4, x5) {
        case (false, 1, 7, false, 'a) => Flip(0.1)
        case (false, 2, 7, false, 'a) => Flip(0.2)
        case (false, 3, 7, false, 'a) => Flip(0.3)
        case (true, 1, 7, false, 'a) => Flip(0.4)
        case (true, 2, 7, false, 'a) => Flip(0.5)
        case (true, 3, 7, false, 'a) => Flip(0.6)
        case (false, 1, 7, true, 'a) => Flip(0.15)
        case (false, 2, 7, true, 'a) => Flip(0.25)
        case (false, 3, 7, true, 'a) => Flip(0.35)
        case (true, 1, 7, true, 'a) => Flip(0.45)
        case (true, 2, 7, true, 'a) => Flip(0.55)
        case (true, 3, 7, true, 'a) => Flip(0.65)
      }
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
        val y = CPD(x1, x2, x3, x4, x5) {
          case (false, 1, 7, false, 'a) => Flip(0.1)
          case (false, 3, 7, false, 'a) => Flip(0.3)
          case (true, 1, 7, false, 'a) => Flip(0.4)
          case (true, 2, 7, false, 'a) => Flip(0.5)
          case (true, 3, 7, false, 'a) => Flip(0.6)
          case (false, 1, 7, true, 'a) => Flip(0.15)
          case (false, 2, 7, true, 'a) => Flip(0.25)
          case (false, 3, 7, true, 'a) => Flip(0.35)
          case (true, 1, 7, true, 'a) => Flip(0.45)
          case (true, 2, 7, true, 'a) => Flip(0.55)
          case (true, 3, 7, true, 'a) => Flip(0.65)
        }
        val alg = VariableElimination(y)
        alg.start()
      }
    }
  }

  "A Rich CPD with one argument" should {
    "produce a result with the expectation over the parent of the probability of the child" in {
      Universe.createNew()
      val x = Select(0.1 -> 1, 0.2 -> 2, 0.3 -> 3, 0.4 -> 4)
      val y = CPD(x) {
        case k if Set(1, 2)(k) => Flip(0.1)
        case k if k != 4 => Flip(0.7)
        case _ => Flip(0.9)
      }
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
      val y = CPD(x1, x2) {
        case (k1, k2) if Set(1, 2)(k1) => Flip(0.1)
        case (k1, k2) if k1 != 4 && !k2 => Flip(0.7)
        case (_, _) => Flip(0.9)
      }
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
      val y: Element[Boolean] = CPD(x1, x2, x3) {
        case (k1, _, k3) if Set(1,2)(k1) && k3 == 5 => Flip(0.1)
        case (k1, k2, _) if k1 != 4 && !k2 => Flip(0.7)
        case (_, _, k3) if !Set(6, 7)(k3) => Flip(0.9)
      }
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
      val y = CPD(x1, x2, x3, x4) {
        case (k1, _, k3, _) if Set(1,2)(k1) && k3 == 5 => Flip(0.1)
        case (k1, k2, _, _) if k1 != 4 && !k2 => Flip(0.7)
        case (_, _, k3, k4) if !Set(6,7)(k3) && k4 => Flip(0.9)
        case (_, _, _, k4) if !k4 => Constant(true)
      }
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
      val y: Element[Boolean] = CPD(x1, x2, x3, x4, x5) {
        case (k1, _, k3, _, _) if Set(1,2)(k1) && k3 ==5 => Flip(0.1)
        case (k1, k2, _, _, _) if k1 != 4 && !k2=> Flip(0.7)
        case (_, _, k3, k4, _) if !Set(6,7)(k3) && k4 => Flip(0.9)
        case (_, _, _, k4, _) if !k4 => Constant(true)
      }
      val alg = VariableElimination(y)
      alg.start()
      alg.probability(y, true) should be(((0.1 + 0.2) * 0.1 + 0.3 * 0.4 * 0.7 +
        (0.3 * 0.6 + 0.4) * (0.8 * 0.9 + 0.2))
        +- 0.00000000001)
    }
  }

  "A Rich CPD using an implicit universe" should {
    "should add elements automatically to implicit universe instead of default" in {
      Universe.createNew()
      val otherUniverse = new Universe
      implicit val implicitUniverse: Universe = otherUniverse
      val x = Constant(true)
      val y = Constant(true)
      val z: Element[Boolean] = CPD(x, y) {
        case (k1, _) if k1 => Constant(true)
        case (k1, _) if !k1=> Constant(false)
      }
      Universe.universe.activeElements.size should be(0)
      otherUniverse.activeElements.size should be(4)
    }
  }
}