/*
 * CollapsedGibbsTest.scala
 * Collapsed Gibbs sampler tests.
 *
 * Created By:      Cory Scott (scottcb@uci.edu)
 * Creation Date:   July 21, 2016
 *
 * Copyright 2015 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.test.experimental

import com.cra.figaro.algorithm.factored.factors._
import com.cra.figaro.algorithm.lazyfactored.LazyValues
import com.cra.figaro.language._
import com.cra.figaro.library.compound.If
import com.cra.figaro.library.compound.^^
import org.scalatest.Matchers
import org.scalatest.WordSpec
import com.cra.figaro.algorithm.factored.factors.factory.Factory
import com.cra.figaro.algorithm.factored.gibbs.Gibbs
import com.cra.figaro.experimental.collapsedgibbs._

class CollapsedGibbsTest extends WordSpec with Matchers {
  "A Collapsed Gibbs sampler" should {
    
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
      Universe.createNew
      val f = Flip(0.3)
      val c = If(f, Flip(0.8), Constant(false))
      c.addConstraint(b => if (b) 2.0 else 1.0)
      // (0.3 * 0.8 * 2) / (0.3 * 0.8 * 2 + 0.3 * 0.2 + 0.7)
      test[Boolean](c, identity, 0.3871)
    }

    "with a constraint on a Chain result correctly constrain the Chain but not the parent" in {
      Universe.createNew
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
      Universe.createNew
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
      test[Boolean](x, identity, 1.0, 2)
    }
    
    "with the default collapser (default parameters) produce the correct result in a deterministic model" in {
      testStrategyDeterministicModel[Boolean]("")
    }
    "with the DETERM collapser (default parameters) produce the correct result in a deterministic model" in {
      testStrategyDeterministicModel[Boolean]("DETERM")
    }
    "with the RECURR collapser (default parameters) produce the correct result in a deterministic model" in {
      testStrategyDeterministicModel[Boolean]("RECURR")
    }
    "with the FACTOR collapser (default parameters) produce the correct result in a deterministic model" in {
      testStrategyDeterministicModel[Boolean]("FACTOR")
    }
    "with the SIMPLE collapser (default parameters) produce the correct result in a deterministic model" in {
      testStrategyDeterministicModel[Boolean]("SIMPLE")
    }
    
    "with the default collapser (high alpha and gamma) produce the correct result in a deterministic model" in {
      testStrategyWithParamsDeterministicModel[Boolean]("", Seq(10, 2000))
    }
    "with the DETERM collapser (high alpha and gamma) produce the correct result in a deterministic model" in {
      testStrategyWithParamsDeterministicModel[Boolean]("DETERM", Seq(10, 2000))
    }
    "with the RECURR collapser (high alpha, gamma, frequent sample saving) produce the correct result in a deterministic model" in {
      testStrategyWithParamsDeterministicModel[Boolean]("RECURR", Seq(10, 2000, 2000, 1))
    }
    "with the FACTOR collapser (high alpha, gamma, and factorThreshhold) produce the correct result in a deterministic model" in {
      testStrategyWithParamsDeterministicModel[Boolean]("FACTOR", Seq(10, 2000, 10000))
    }
    "with the SIMPLE collapser (high alpha and gamma) produce the correct result in a deterministic model" in {
      testStrategyWithParamsDeterministicModel[Boolean]("SIMPLE", Seq(10, 2000))
    }

    "with the default collapser (default parameters) produce the correct result in an Ising model" in {
      testStrategyIsingModel[Boolean]("")
    }
    "with the DETERM collapser (default parameters) produce the correct result in an Ising model" in {
      testStrategyIsingModel[Boolean]("DETERM")
    }
    "with the RECURR collapser (default parameters) produce the correct result in an Ising model" in {
      testStrategyIsingModel[Boolean]("RECURR")
    }
    "with the FACTOR collapser (default parameters) produce the correct result in an Ising model" in {
      testStrategyIsingModel[Boolean]("FACTOR")
    }
    "with the SIMPLE collapser (default parameters) produce the correct result in an Ising model" in {
      testStrategyIsingModel[Boolean]("SIMPLE")
    }
    
    "with the default collapser (high alpha and gamma) produce the correct result in a Ising model" in {
      testStrategyWithParamsIsingModel[Boolean]("", Seq(10, 2000))
    }
    "with the DETERM collapser (high alpha and gamma) produce the correct result in a Ising model" in {
      testStrategyWithParamsIsingModel[Boolean]("DETERM", Seq(10, 2000))
    }
    "with the RECURR collapser (high alpha, gamma, frequent sample saving)  produce the correct result in a Ising model" in {
      testStrategyWithParamsIsingModel[Boolean]("RECURR", Seq(10, 2000, 2000, 1))
    }
    "with the FACTOR collapser (high alpha, gamma, and factorThreshhold) produce the correct result in a Ising model" in {
      testStrategyWithParamsIsingModel[Boolean]("FACTOR", Seq(10, 2000, 10000))
    }
    "with the SIMPLE collapser (high alpha and gamma) produce the correct result in a Ising model" in {
      testStrategyWithParamsIsingModel[Boolean]("SIMPLE", Seq(10, 2000))
    }
    
    "with the default collapser and default parameters collapse down to query variables" in {
      Universe.createNew
      val a1 = Flip(0.5)
      val a2 = Flip(0.5)
      val a3 = Flip(0.5)
      val b1 = Apply(a1, a2, (b1: Boolean, b2: Boolean) => b1 || b2)
      val b2 = Apply(a3, a2, (b1: Boolean, b2: Boolean) => b1 || b2)
      val b3 = Apply(a1, a3, (b1: Boolean, b2: Boolean) => b1 || b2)
      val c1 = Apply(b1, b2, (b1: Boolean, b2: Boolean) => b1 || b2)
      val c2 = Apply(b3, b2, (b1: Boolean, b2: Boolean) => b1 || b2)
      val c3 = Apply(b1, b3, (b1: Boolean, b2: Boolean) => b1 || b2)
      val d3 = Apply(c1, c2, c3, (b1: Boolean, b2: Boolean, b3: Boolean) => b1 || b2 || b3)
      val alg2 = CollapsedGibbs(10000, d3)
      alg2.initialize()
      alg2.variables.toList.length should be (1)
    }
    
  }

  def makeFactors(): List[Factor[Double]] = {
    LazyValues(Universe.universe).expandAll(Universe.universe.activeElements.toSet.map((elem: Element[_]) => ((elem, Integer.MAX_VALUE))))
    Universe.universe.activeElements.foreach(Variable(_))
    Universe.universe.activeElements flatMap (Factory.makeFactorsForElement(_))
  }

  def test[T](target: Element[T], predicate: T => Boolean, prob: Double, tol: Double = 0.025) {
    val algorithm = CollapsedGibbs(100000, target)
    algorithm.start()
    algorithm.stop()
    algorithm.probability(target, predicate) should be(prob +- tol)
    algorithm.kill()
  }
  def testStrategyDeterministicModel[T](strategy:String, tol: Double = 0.025) {
    Universe.universe.finalize()
    Universe.createNew
    val a1 = Flip(0.5)
    val a2 = Flip(0.5)
    val a3 = Flip(0.5)
    val b1 = Apply(a1, a2, (b1: Boolean, b2: Boolean) => b1 || b2)
    val b2 = Apply(a3, a2, (b1: Boolean, b2: Boolean) => b1 || b2)
    val b3 = Apply(a1, a3, (b1: Boolean, b2: Boolean) => b1 || b2)
    val c1 = Apply(b1, b2, (b1: Boolean, b2: Boolean) => b1 || b2)
    val c2 = Apply(b3, b2, (b1: Boolean, b2: Boolean) => b1 || b2)
    val c3 = Apply(b1, b3, (b1: Boolean, b2: Boolean) => b1 || b2)
    val d3 = Apply(c1, c2, c3, (b1: Boolean, b2: Boolean, b3: Boolean) => b1 || b2 || b3)
    val algorithm = CollapsedGibbs(strategy:String, 100000, d3)
    algorithm.start()
    algorithm.stop()
    algorithm.probability(d3, true) should be(.875 +- tol)
    algorithm.kill()
  }
  def testStrategyWithParamsDeterministicModel[T](strategy:String, params:Seq[Int], tol: Double = 0.025) {
    Universe.universe.finalize()
    Universe.createNew
    val a1 = Flip(0.5)
    val a2 = Flip(0.5)
    val a3 = Flip(0.5)
    val b1 = Apply(a1, a2, (b1: Boolean, b2: Boolean) => b1 || b2)
    val b2 = Apply(a3, a2, (b1: Boolean, b2: Boolean) => b1 || b2)
    val b3 = Apply(a1, a3, (b1: Boolean, b2: Boolean) => b1 || b2)
    val c1 = Apply(b1, b2, (b1: Boolean, b2: Boolean) => b1 || b2)
    val c2 = Apply(b3, b2, (b1: Boolean, b2: Boolean) => b1 || b2)
    val c3 = Apply(b1, b3, (b1: Boolean, b2: Boolean) => b1 || b2)
    val d3 = Apply(c1, c2, c3, (b1: Boolean, b2: Boolean, b3: Boolean) => b1 || b2 || b3)
    val algorithm = CollapsedGibbs(strategy:String, params, 100000, d3)
    algorithm.start()
    algorithm.stop()
    algorithm.probability(d3, true) should be(.875 +- tol)
    algorithm.kill()
  }
  def testStrategyIsingModel[T](strategy:String, tol: Double = 0.025) {
    Universe.universe.finalize()
    Universe.createNew()
    def IsingConstraint(pair: (Boolean, Boolean)) = if (pair._1 == pair._2) 1.1; else 1.0
    val IsingSize = 4
    var allFlips = for {i <- 0 until IsingSize} yield (for {j <- 0 until IsingSize} yield Flip(0.5)).toList
    for {i <- 0 until IsingSize} {
      for {j <- 0 until IsingSize} {
        ^^(allFlips(i)(j), allFlips((i+1) % IsingSize)(j)).setConstraint(IsingConstraint)
        ^^(allFlips(i)(j), allFlips(i)((j + 1) % IsingSize)).setConstraint(IsingConstraint)
      }
    }
    allFlips(IsingSize/2)(IsingSize/2).observe(false)
    val toTest = allFlips(0)(0)
    val algorithm = CollapsedGibbs(strategy:String, 100000, toTest)
    algorithm.start()
    algorithm.stop()
    algorithm.probability(toTest, true) should be(Gibbs.probability(toTest, true) +- tol)
    algorithm.kill()
  }
  def testStrategyWithParamsIsingModel[T](strategy:String, params:Seq[Int], tol: Double = 0.025) {
    Universe.universe.finalize()
    Universe.createNew()
    def IsingConstraint(pair: (Boolean, Boolean)) = if (pair._1 == pair._2) 1.1; else 1.0
    val IsingSize = 4
    var allFlips = for {i <- 0 until IsingSize} yield (for {j <- 0 until IsingSize} yield Flip(0.5)).toList
    for {i <- 0 until IsingSize} {
      for {j <- 0 until IsingSize} {
        ^^(allFlips(i)(j), allFlips((i+1) % IsingSize)(j)).setConstraint(IsingConstraint)
        ^^(allFlips(i)(j), allFlips(i)((j + 1) % IsingSize)).setConstraint(IsingConstraint)
      }
    }
    allFlips(IsingSize/2)(IsingSize/2).observe(false)
    val toTest = allFlips(0)(0)
    val algorithm = CollapsedGibbs(strategy:String, params, 100000, toTest)
    algorithm.start()
    algorithm.stop()
    algorithm.probability(toTest, true) should be(Gibbs.probability(toTest, true) +- tol)
    algorithm.kill()
  }
}