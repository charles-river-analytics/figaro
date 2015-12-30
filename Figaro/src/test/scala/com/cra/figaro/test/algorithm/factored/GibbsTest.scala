/*
 * GibbsTest.scala
 * Gibbs sampler tests.
 *
 * Created By:      William Kretschmer (kretsch@mit.edu)
 * Creation Date:   Aug 4, 2015
 *
 * Copyright 2015 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.test.algorithm.factored

import com.cra.figaro.algorithm.factored.factors._
import com.cra.figaro.algorithm.lazyfactored.{ LazyValues, ValueSet }
import com.cra.figaro.language._
import com.cra.figaro.library.atomic.discrete.Uniform
import com.cra.figaro.library.compound.If
import org.scalatest.Matchers
import org.scalatest.WordSpec
import com.cra.figaro.algorithm.factored.factors.factory.Factory
import com.cra.figaro.algorithm.factored.gibbs.Gibbs
import com.cra.figaro.algorithm.factored.gibbs.WalkSAT
import com.cra.figaro.algorithm.factored.gibbs.StateNotFoundException
import com.cra.figaro.algorithm.factored.gibbs.BlockSampler
import com.cra.figaro.language.Element.toBooleanElement

class GibbsTest extends WordSpec with Matchers {
  "A Gibbs sampler" should {
    "block Apply elements" in {
      Universe.createNew
      val u1 = Uniform(1, 2, 3)
      val u2 = Uniform(4, 5, 6)
      val a = Apply[Int, Int, Int](u1, u2, _ + _)
      val alg = Gibbs(1, a)
      alg.initialize()
      val blocks = alg.createBlocks()
      blocks.map(_.toSet) should contain theSameElementsAs (List(
        Set(Variable(u1), Variable(a)),
        Set(Variable(u2), Variable(a))))
    }

    "block Chain elements" in {
      Universe.createNew
      val f = Flip(0.3)
      val u1 = Uniform(1, 2, 3)
      val u2 = Uniform(2, 3, 4)
      val c = If(f, u1, u2)
      val alg = Gibbs(1, c)
      alg.initialize()
      val icv = alg.variables.find(_.isInstanceOf[InternalChainVariable[_]]).get
      val blocks = alg.createBlocks()
      blocks.map(_.toSet) should contain theSameElementsAs (List(
        Set(Variable(f), icv),
        Set(Variable(u1), Variable(c), icv),
        Set(Variable(u2), Variable(c), icv)))
    }

    "with an unconstrained model produce the correct result" in {
      Universe.createNew
      val f = Flip(0.3)
      val s1 = Select(0.1 -> 1, 0.4 -> 2, 0.5 -> 3)
      val s2 = Select(0.7 -> 2, 0.1 -> 3, 0.2 -> 4)
      val c = If(f, s1, s2)
      // 0.3 * 0.4 + 0.7 * 0.7 = 0.61
      test[Int](c, _ == 2, 0.61)
    }

    "with a constrained model produce the correct result" in {
      Universe.createNew
      val f = Flip(0.3)
      val s1 = Select(0.1 -> 1, 0.4 -> 2, 0.5 -> 3)
      val s2 = Select(0.7 -> 2, 0.1 -> 3, 0.2 -> 4)
      val c = If(f, s1, s2)
      c.addConstraint(identity)
      // 0.61 * 2 / (0.3*0.1*1 + 0.3*0.4*2 + 0.3*0.5*3 + 0.7*0.7*2 + 0.7*0.1*3 + 0.7*0.2*4)
      test[Int](c, _ == 2, 0.4939)
    }

    "with a constraint on a Chain produce the correct result for the parent" in {
      val f = Flip(0.3)
      val c = If(f, Flip(0.8), Constant(false))
      c.addConstraint(b => if (b) 2.0 else 1.0)
      // (0.3 * 0.8 * 2) / (0.3 * 0.8 * 2 + 0.3 * 0.2 + 0.7)
      test[Boolean](c, identity, 0.3871)
    }

    "with a constraint on a Chain result correctly constrain the Chain but not the parent" in {
      val f = Flip(0.3)
      val r1 = Flip(0.8)
      r1.addConstraint(b => if (b) 2.0 else 1.0)
      val c = If(f, r1, Constant(false))
      test[Boolean](f, identity, 0.3)
      // r1 true with probability (0.8 * 2) / (0.8 * 2 + 0.2) = 0.8889
      // 0.3 * 0.8889
      test[Boolean](c, identity, 0.2667)
    }

    "with an element used multiple times use the same value each time" in {
      val f = Flip(0.3)
      val e = f === f
      test[Boolean](e, identity, 1.0)
    }

    "not underflow" in {
      Universe.createNew()
      val x = Flip(0.99)
      for (i <- 0 until 10) {
        x.addConstraint((b: Boolean) => if (b) 1e-100; else 1e-120)
      }
      test[Boolean](x, identity, 1.0)
    }
  }

  "WalkSAT" should {
    "return the correct state on a conditioned model" in {
      def chainMapper(chain: Chain[_, _]): Set[Variable[_]] = LazyValues(chain.universe).getMap(chain).values.map(Variable(_)).toSet

      Universe.createNew()
      val x = Flip(0.5)
      val y = Flip(0.5)
      val z = Flip(0.5)
      val result = (x || !y) && z && !((x !== y) !== z)
      // Satisfied by x = true, y = false, z = true
      result.observe(true)
      val factors = makeFactors()
      val variables = factors.flatMap(_.variables).toSet
      val state = WalkSAT(factors, variables, SumProductSemiring(), chainMapper)
      List((x, true), (y, false), (z, true)).foreach(pair => {
        val (elem, value) = pair
        val variable = Variable(elem)
        variable.range(state(variable)).value should be(value)
      })
    }

    "fail on a contradictory model" in {
      def chainMapper(chain: Chain[_, _]): Set[Variable[_]] = LazyValues(chain.universe).getMap(chain).values.map(Variable(_)).toSet

      Universe.createNew()
      val x = Flip(0.5)
      val y = Flip(0.5)
      val result = (x !== y) && (x === y)
      result.observe(true)
      val factors = makeFactors()
      val variables = factors.flatMap(_.variables).toSet
      a[StateNotFoundException] should be thrownBy (WalkSAT(factors, variables, SumProductSemiring(), chainMapper))
    }
  }

  "A default block sampler" should {
    "produce sub-factors on initialization" in {
      val v1 = new Variable(ValueSet.withoutStar(Set(0, 1, 2, 3)))
      val v2 = new Variable(ValueSet.withoutStar(Set(0, 1)))
      val semiring = LogSumProductSemiring()
      val factor = new SparseFactor[Double](List(v1), List(v2), semiring)
      factor.set(List(0, 0), 1.0)
      factor.set(List(1, 0), 2.0)
      factor.set(List(1, 1), 3.0)
      factor.set(List(2, 0), Double.NegativeInfinity)
      factor.set(List(2, 1), 4.0)
      val sampler = BlockSampler.default(List(v2), List(factor))
      val List(mbLookupFactor) = sampler.mbLookupFactors
      mbLookupFactor.get(List(0)).get(List(0)) should be(1.0)
      mbLookupFactor.get(List(1)).get(List(0)) should be(2.0)
      mbLookupFactor.get(List(1)).get(List(1)) should be(3.0)
      mbLookupFactor.get(List(2)).contents.get(List(0)) should be(empty)
      mbLookupFactor.get(List(2)).get(List(1)) should be(4.0)
      mbLookupFactor.contents.get(List(3)) should be(empty)
    }

    "normalize and make un-logarithmic a factor" in {
      val v1 = new Variable(ValueSet.withoutStar(Set(0, 1)))
      val v2 = new Variable(ValueSet.withoutStar(Set(0, 1)))
      val semiring = LogSumProductSemiring()
      val factor = new SparseFactor[Double](List(v1), List(v2), semiring)
      factor.set(List(0, 0), -5000)
      factor.set(List(1, 0), -5001)
      factor.set(List(1, 1), -5002)
      val sampler = BlockSampler.default(List(), List())
      val normalized = sampler.normalizeFactor(factor)
      val sum = 1 + math.exp(1) + math.exp(2)
      val tol = 1e-9
      normalized.get(List(0, 0)) should be((math.exp(2) / sum) +- tol)
      normalized.get(List(1, 0)) should be((math.exp(1) / sum) +- tol)
      normalized.get(List(1, 1)) should be((1 / sum) +- tol)
    }

    "compute a sampling factor" in {
      val v1 = new Variable(ValueSet.withoutStar(Set(0, 1)))
      val v2 = new Variable(ValueSet.withoutStar(Set(0, 1)))
      val v3 = new Variable(ValueSet.withoutStar(Set(0, 1)))
      val semiring = LogSumProductSemiring()
      val f12 = new SparseFactor[Double](List(v1), List(v2), semiring)
      val f32 = new SparseFactor[Double](List(v3), List(v2), semiring)
      f12.set(List(0, 0), math.log(0.3))
      f12.set(List(0, 1), math.log(0.7))
      f12.set(List(1, 0), math.log(0.8))
      f12.set(List(1, 1), math.log(0.2))
      f32.set(List(0, 0), math.log(0.25))
      f32.set(List(0, 1), math.log(0.75))
      f32.set(List(1, 0), math.log(0.1))
      f32.set(List(1, 1), math.log(0.9))
      val sampler = BlockSampler.default(List(v2), List(f12, f32))
      val currentSamples = collection.mutable.Map[Variable[_], Int](v1 -> 0, v2 -> 1, v3 -> 1)
      val samplingFactor = sampler.computeSamplingFactor(currentSamples)
      val tol = 1e-9
      samplingFactor.get(List(0)) should be((0.3 * 0.1) / (0.3 * 0.1 + 0.7 * 0.9) +- tol)
      samplingFactor.get(List(1)) should be((0.7 * 0.9) / (0.3 * 0.1 + 0.7 * 0.9) +- tol)
    }

    "cache recent sampling factors" in {
      val v1 = new Variable(ValueSet.withoutStar(Set(0, 1)))
      val v2 = new Variable(ValueSet.withoutStar(Set(0, 1)))
      val semiring = LogSumProductSemiring()
      val factor = new SparseFactor[Double](List(v1), List(v2), semiring)
      factor.set(List(0, 0), math.log(0.3))
      factor.set(List(0, 1), math.log(0.7))
      factor.set(List(1, 0), math.log(0.8))
      factor.set(List(1, 1), math.log(0.2))
      val sampler = BlockSampler.default(List(v2), List(factor))
      val currentSamples = collection.mutable.Map[Variable[_], Int](v1 -> 0, v2 -> 1)
      val samplingFactor1 = sampler.getSamplingFactor(currentSamples)
      val samplingFactor2 = sampler.getSamplingFactor(currentSamples)
      samplingFactor1 should be theSameInstanceAs samplingFactor2
    }
  }

  def makeFactors(): List[Factor[Double]] = {
    LazyValues(Universe.universe).expandAll(Universe.universe.activeElements.toSet.map((elem: Element[_]) => ((elem, Integer.MAX_VALUE))))
    Universe.universe.activeElements.foreach(Variable(_))
    Universe.universe.activeElements flatMap (Factory.makeFactorsForElement(_))
  }

  def test[T](target: Element[T], predicate: T => Boolean, prob: Double, tol: Double = 0.025) {
    val algorithm = Gibbs(100000, target)
    algorithm.start()
    algorithm.probability(target, predicate) should be(prob +- tol)
  }
}