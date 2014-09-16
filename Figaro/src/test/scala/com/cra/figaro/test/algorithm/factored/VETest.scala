/*
 * VETest.scala  
 * Variable elimination tests.
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Jan 1, 2009
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.test.algorithm.factored

import org.scalatest.Matchers
import org.scalatest.{ WordSpec, PrivateMethodTester }
import math.log
import com.cra.figaro.algorithm._
import com.cra.figaro.algorithm.factored._
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.language._
import com.cra.figaro.library.compound._
import com.cra.figaro.library.atomic.continuous._
import com.cra.figaro.util._
import com.cra.figaro.test._
import scala.collection.mutable.Map
import com.cra.figaro.test.tags.Performance
import com.cra.figaro.test.tags.NonDeterministic

class VETest extends WordSpec with Matchers {
  "A VEGraph" when {
    "initially" should {
      "associate each element with all its factors and neighbors" in {
        Universe.createNew()
        val e1 = Flip(0.1)
        val e2 = Constant(8)
        val e3 = Select(0.2 -> "a", 0.3 -> "b", 0.5 -> "c")
        val e4 = Flip(0.7)
        val e5 = Constant('a)
        val e6 = Select(0.1 -> 1.5, 0.9 -> 2.5)
        val v1 = Variable(e1)
        val v2 = Variable(e2)
        val v3 = Variable(e3)
        val v4 = Variable(e4)
        val v5 = Variable(e5)
        val v6 = Variable(e6)
        val f = Factory.make[Double](List(v1, v2, v3, v4))
        val g = Factory.make[Double](List(v5, v3, v2, v6))
        val af = AbstractFactor(f.variables)
        val ag = AbstractFactor(g.variables)
        val graph = new VEGraph(List(f, g))
        val info = graph.info
        info(v1) should equal(VariableInfo(Set(af), Set(v1, v2, v3, v4)))
        info(v2) should equal(VariableInfo(Set(af, ag), Set(v1, v2, v3, v4, v5, v6)))
        info(v3) should equal(VariableInfo(Set(af, ag), Set(v1, v2, v3, v4, v5, v6)))
        info(v4) should equal(VariableInfo(Set(af), Set(v1, v2, v3, v4)))
        info(v5) should equal(VariableInfo(Set(ag), Set(v2, v3, v5, v6)))
        info(v6) should equal(VariableInfo(Set(ag), Set(v2, v3, v5, v6)))
      }
    }

    "computing the cost of a set of factors" should {
      "return the sum over the factors of the product of the ranges of the variables in each factor" in {
        Universe.createNew()
        val e1 = Flip(0.1)
        val e2 = Constant(8)
        val e3 = Select(0.2 -> "a", 0.3 -> "b", 0.5 -> "c")
        val e4 = Flip(0.7)
        val e5 = Constant('a)
        val e6 = Select(0.1 -> 1.5, 0.9 -> 2.5)
        Values()(e1)
        Values()(e2)
        Values()(e3)
        Values()(e4)
        Values()(e5)
        Values()(e6)
        val v1 = Variable(e1)
        val v2 = Variable(e2)
        val v3 = Variable(e3)
        val v4 = Variable(e4)
        val v5 = Variable(e5)
        val v6 = Variable(e6)
        val f = Factory.make[Double](List(v1, v2, v3, v4))
        val g = Factory.make[Double](List(v5, v3, v2, v6))
        val af = AbstractFactor(f.variables)
        val ag = AbstractFactor(g.variables)
        VEGraph.cost(List(af, ag)) should equal(18) // 2*1*3*2 + 1*3*1*2
      }
    }

    "computing the score of eliminating a variable" should {
      "return the cost of the new factor minus the costs of the old factors when eliminating a variable" in {
        Universe.createNew()
        val e1 = Flip(0.1)
        val e2 = Constant(8)
        val e3 = Select(0.2 -> "a", 0.3 -> "b", 0.5 -> "c")
        val e4 = Flip(0.7)
        val e5 = Constant('a)
        val e6 = Select(0.1 -> 1.5, 0.9 -> 2.5)
        val e7 = Flip(0.9)
        Values()(e1)
        Values()(e2)
        Values()(e3)
        Values()(e4)
        Values()(e5)
        Values()(e6)
        Values()(e7)
        val v1 = Variable(e1)
        val v2 = Variable(e2)
        val v3 = Variable(e3)
        val v4 = Variable(e4)
        val v5 = Variable(e5)
        val v6 = Variable(e6)
        val v7 = Variable(e7)
        val f = Factory.make[Double](List(v1, v2, v3, v4))
        val g = Factory.make[Double](List(v5, v3, v2, v6))
        val h = Factory.make[Double](List(v1, v7))
        val graph1 = new VEGraph(List(f, g, h))
        val score = graph1.score(v3)
        score should equal(-10) // 2*1*2*1*2 - (2*1*3*2 + 1*3*1*2)
      }
    }

    "after eliminating a variable" should {
      "contain a factor involving all its neighbors and not contain " +
        "any of the original factors involving the variable" in {
          Universe.createNew()
          val e1 = Flip(0.1)
          val e2 = Constant(8)
          val e3 = Select(0.2 -> "a", 0.3 -> "b", 0.5 -> "c")
          val e4 = Flip(0.7)
          val e5 = Constant('a)
          val e6 = Select(0.1 -> 1.5, 0.9 -> 2.5)
          val e7 = Flip(0.9)
          val v1 = Variable(e1)
          val v2 = Variable(e2)
          val v3 = Variable(e3)
          val v4 = Variable(e4)
          val v5 = Variable(e5)
          val v6 = Variable(e6)
          val v7 = Variable(e7)
          val f = Factory.make[Double](List(v1, v2, v3, v4))
          val g = Factory.make[Double](List(v5, v3, v2, v6))
          val h = Factory.make[Double](List(v1, v7))
          val graph1 = new VEGraph(List(f, g, h))
          val graph2 = graph1.eliminate(v3)
          val VariableInfo(v1Factors, v1Neighbors) = graph2.info(v1)
          v1Factors.size should equal(2) // h and the new factor
          assert(v1Factors exists ((factor: AbstractFactor) => factor.variables.size == 5)) // all except v3 and v7
        }

      "not have any other variables have the variable as a neighbor" in {
        Universe.createNew()
        val e1 = Flip(0.1)
        val e2 = Constant(8)
        val e3 = Select(0.2 -> "a", 0.3 -> "b", 0.5 -> "c")
        val e4 = Flip(0.7)
        val e5 = Constant('a)
        val e6 = Select(0.1 -> 1.5, 0.9 -> 2.5)
        val e7 = Flip(0.9)
        val v1 = Variable(e1)
        val v2 = Variable(e2)
        val v3 = Variable(e3)
        val v4 = Variable(e4)
        val v5 = Variable(e5)
        val v6 = Variable(e6)
        val v7 = Variable(e7)
        val f = Factory.make[Double](List(v1, v2, v3, v4))
        val g = Factory.make[Double](List(v5, v3, v2, v6))
        val h = Factory.make[Double](List(v1, v7))
        val graph1 = new VEGraph(List(f, g, h))
        val graph2 = graph1.eliminate(v3)
        val VariableInfo(v1Factors, v1Neighbors) = graph2.info(v1)
        v1Neighbors should not contain (v3)
      }
    }
  }

  "Computing an elimination order" should {
    "select an elimination order that eliminates all except the variables to preserve while greedily minimizing " +
      "cost" in {
        Universe.createNew()
        val e1 = Flip(0.1)
        val e2 = Constant(8)
        val e3 = Select(0.2 -> "a", 0.3 -> "b", 0.5 -> "c")
        val e4 = Flip(0.7)
        val e5 = Constant('a)
        val e6 = Select(0.1 -> 1.5, 0.9 -> 2.5)
        val e7 = Flip(0.9)
        val e8 = Flip(0.3)
        Values()(e1)
        Values()(e2)
        Values()(e3)
        Values()(e4)
        Values()(e5)
        Values()(e6)
        Values()(e7)
        Values()(e8)
        val v1 = Variable(e1)
        val v2 = Variable(e2)
        val v3 = Variable(e3)
        val v4 = Variable(e4)
        val v5 = Variable(e5)
        val v6 = Variable(e6)
        val v7 = Variable(e7)
        val v8 = Variable(e8)
        val f = Factory.make[Double](List(v1, v2, v3, v4))
        val g = Factory.make[Double](List(v5, v3, v2, v6))
        val h = Factory.make[Double](List(v1, v7))
        val i = Factory.make[Double](List(v8, v1, v3))
	/* Old method that considered old cost of factor containing variable */
        // Initially:
        // Assume we will not eliminate v5 or v8
        // cost(f) = 2*1*3*2 = 12
        // cost(g) = 1*3*1*2 = 6
        // cost(h) = 2*2 = 4
        // cost(i) = 2*2*3 = 12
        // Eliminating v1 will produce a factor X1(v2,v3,v4,v7,v8) with cost 1*3*2*2*2 = 24
        // Eliminating v2 will produce a factor X2(v1,v3,v4,v5,v6) with cost 2*3*2*1*2 = 24
        // Eliminating v3 will produce a factor X3(v1,v2,v4,v5,v6,v8) with cost 2*1*2*1*2*2 = 16
        // Eliminating v4 will produce a factor X4(v1,v2,v3) with cost 2*1*3 = 6
        // Eliminating v6 will produce a factor X5(v2,v3,v5) with cost 1*3*1 = 3
        // Eliminating v7 will produce a factor X6(v1) with cost 2
        // cost(v1) = 24 - (12+4+12) = -4
        // cost(v2) = 24 - (12+6) = 6
        // cost(v3) = 16 - (12+6+12) = -14
        // cost(v4) = 6 - 12 = -6
        // cost(v6) = 3 - 6 = -3
        // cost(v7) = 2 - 4 = -2
        // Choose v3 first.
        // We now have:
        // h(v1,v7) with cost 4
        // X3(v1,v2,v4,v5,v6,v8) with cost 16
        // Eliminating v1 will produce a factor X7(v2,v4,v5,v6,v7,v8) with cost 1*2*1*2*2*2 = 16
        // Eliminating v2 will produce a factor X8(v1,v4,v5,v6,v8) with cost 2*2*1*2*2 = 16
        // Eliminating v4 will produce a factor X9(v1,v2,v5,v6,v8) with cost 2*1*1*2*2 = 8
        // Eliminating v6 will produce a factor X10(v1,v2,v4,v5,v8) with cost 2*1*2*1*2 = 8
        // Eliminating v7 will produce a factor X11(v1) with cost 2
        // cost(v1) = 16 - (4+16) = -4
        // cost(v2) = 16 - 16 = 0
        // cost(v4) = 8 - 16 = -8
        // cost(v6) = 8 - 16 = -8
        // cost(v7) = 2 - 4 = -2
        // Choose v4 or v6. WLOG assume v4.
        // We now have:
        // h(v1,v7) with cost 4
        // X9(v1,v2,v5,v6,v8) with cost 8
        // Eliminating v1 will produce a factor X12(v2,v5,v6,v7,v8) with cost 1*1*2*2*2 = 8
        // Eliminating v2 will produce a factor X13(v1,v5,v6,v8) with cost 2*1*2*2 = 8
        // Eliminating v6 will produce a factor X14(v1,v2,v5,v8) with cost 2*1*1*2 = 4
        // Eliminating v7 will produce a factor X15(v1) with cost 2
        // cost(v1) = 8 - (4+8) = -4
        // cost(v2) = 8-8 = 0
        // cost(v6) = 4 - 8 = -4
        // cost(v7) = 2 - 4 = -1
        // Suppose we choose v1.
        // We now have:
        // X12(v2,v5,v6,v7,v8).
        // We will first eliminate either v6 or v7, then the other one, then v2.
        // Suppose instead we choose v6.
        // We now have:
        // h(v1,v7) with cost 4
        // X14(v1,v2,v5,v8) with cost 4
        // Eliminating v1 will produce a factor X16(v2,v5,v7,v8) with cost 4.
        // Eliminating v2 will produce a factor X17(v1,v5,v8) with cost 4.
        // Eliminating v7 will produce a factor X18(v1,v7) with cost 4.
        // cost(v1) = -4 wins. Eliminate v1.
        // We now have:
        // X16(v2,v5,v7,v8). Eliminate v7, then v2.
        //
        // Bottom line: possible elimination orders are:
        // v3, v4, v1, v6, v7, v2
        // v3, v4, v1, v7, v6, v2
        // v3, v4, v6, v1, v7, v2
        // v3, v6, v1, v4, v7, v2
        // v3, v6, v1, v7, v4, v2
        // v3, v6, v4, v1, v7, v2
        val ve = VariableElimination(e5, e8)
        val order = ve.eliminationOrder(List(f, g, h, i), Set(v5, v8))
        assert(order == List(v3, v4, v1, v6, v7, v2) ||
          order == List(v3, v4, v1, v7, v6, v2) ||
          order == List(v3, v4, v6, v1, v7, v2) ||
          order == List(v3, v6, v1, v4, v7, v2) ||
          order == List(v3, v6, v1, v7, v4, v2) ||
          order == List(v3, v6, v4, v1, v7, v2))
      }

    "take O(|factors| log |variables|)" taggedAs (Performance, NonDeterministic) in {
      Universe.createNew()
      val small = 100
      val large = 200
      def make(numVars: Int): Traversable[Factor[Double]] = {
        val universe = new Universe
        val a: List[Variable[_]] = List.tabulate(numVars)(i => Variable(Flip(0.3)("", universe)))
        for { i <- 0 to numVars - 2 } yield Factory.make[Double](List(a(i), a(i + 1)))
      }
      val factors1 = make(small)
      val factors2 = make(large)
      def order(factors: Traversable[Factor[Double]])() =
        VariableElimination().eliminationOrder(factors, List())
      val time1 = measureTime(order(factors1), 20, 100)
      val time2 = measureTime(order(factors2), 20, 100)
      val slack = 1.1
      time2 / time1 should be < (large / small * log(large) / log(small) * slack)
    }
  }

  "Running VariableElimination" should {
    "with no conditions or constraints produce the correct result" in {
      Universe.createNew()
      val u = Select(0.25 -> 0.3, 0.25 -> 0.5, 0.25 -> 0.7, 0.25 -> 0.9)
      val f = Flip(u)
      val a = If(f, Select(0.3 -> 1, 0.7 -> 2), Constant(2))
      test(f, (b: Boolean) => b, 0.6)
    }

    "with a condition on a dependent element produce the result with the correct probability" in {
      Universe.createNew()
      val u = Select(0.25 -> 0.3, 0.25 -> 0.5, 0.25 -> 0.7, 0.25 -> 0.9)
      val f = Flip(u)
      val a = If(f, Select(0.3 -> 1, 0.7 -> 2), Constant(2))
      a.setCondition((i: Int) => i == 2)
      // U(true) = \int_{0.2}^{1.0) 0.7 p = 0.35 * 0.96
      // U(false) = \int_{0.2}^{1.0) (1-p)
      val u1 = 0.35 * 0.96
      val u2 = 0.32
      test(f, (b: Boolean) => b, u1 / (u1 + u2))
    }

    "with a constraint on a dependent element produce the result with the correct probability" in {
      Universe.createNew()
      val u = Select(0.25 -> 0.3, 0.25 -> 0.5, 0.25 -> 0.7, 0.25 -> 0.9)
      val f = Flip(u)
      val a = If(f, Select(0.3 -> 1, 0.7 -> 2), Constant(2))
      a.setConstraint((i: Int) => i.toDouble)
      // U(true) = \int_{0.2}^{1.0} (0.3 + 2 * 0.7) p = 0.85 * 0.96
      // U(false) = \int_{0.2}^(1.0) (2 * (1-p)) = 0.64
      val u1 = 0.85 * 0.96
      val u2 = 0.64
      test(f, (b: Boolean) => b, u1 / (u1 + u2))
    }

    "with an element that uses another element multiple times, " +
      "always produce the same value for the different uses" in {
        Universe.createNew()
        val f = Flip(0.5)
        val e = f === f
        test(e, (b: Boolean) => b, 1.0)
      }

    "with a constraint on an element that is used multiple times, only factor in the constraint once" in {
      Universe.createNew()
      val f1 = Flip(0.5)
      val f2 = Flip(0.3)
      val e1 = f1 === f1
      val e2 = f1 === f2
      val d = Dist(0.5 -> e1, 0.5 -> e2)
      f1.setConstraint((b: Boolean) => if (b) 3.0; else 2.0)
      // Probability that f1 is true = 0.6
      // Probability that e1 is true = 1.0
      // Probability that e2 is true = 0.6 * 0.3 + 0.4 * 0.7 = 0.46
      // Probability that d is true = 0.5 * 1 + 0.5 * 0.46 = 0.73
      test(d, (b: Boolean) => b, 0.73)
    }

    "with elements that are not used by the query or evidence, produce the correct result" in {
      val u1 = Universe.createNew()
      val u = Select(0.25 -> 0.3, 0.25 -> 0.5, 0.25 -> 0.7, 0.25 -> 0.9)
      val f = Flip(u)
      val a = If(f, Select(0.3 -> 1, 0.7 -> 2), Constant(2))
      test(f, (b: Boolean) => b, 0.6)
    }
    
    "on a different universe from the current universe, produce the correct result" in {
      val u1 = Universe.createNew()
      val u = Select(0.25 -> 0.3, 0.25 -> 0.5, 0.25 -> 0.7, 0.25 -> 0.9)
      val f = Flip(u)
      Universe.createNew()
      val tolerance = 0.0000001
      val algorithm = VariableElimination(f)(u1)
      algorithm.start()
      algorithm.probability(f, (b: Boolean) => b) should be(0.6 +- tolerance)
      algorithm.kill()
    }

    "with a model using chain and no conditions or constraints, produce the correct answer" in {
      Universe.createNew()
      val f = Flip(0.3)
      val s1 = Select(0.1 -> 1, 0.9 -> 2)
      val s2 = Select(0.7 -> 1, 0.2 -> 2, 0.1 -> 3)
      val c = Chain(f, (b: Boolean) => if (b) s1; else s2)
      test(c, (i: Int) => i == 1, 0.3 * 0.1 + 0.7 * 0.7)
    }

    "with a model using chain and a condition on the result, correctly condition the parent" in {
      Universe.createNew()
      val f = Flip(0.3)
      val s1 = Select(0.1 -> 1, 0.9 -> 2)
      val s2 = Select(0.7 -> 1, 0.2 -> 2, 0.1 -> 3)
      val c = Chain(f, (b: Boolean) => if (b) s1; else s2)
      c.observe(1)
      test(f, (b: Boolean) => b, 0.3 * 0.1 / (0.3 * 0.1 + 0.7 * 0.7))
    }

    "with a model using chain and a condition on one of the outcome elements, correctly condition the result " +
      "but not change the belief about the parent" in {
        Universe.createNew()
        val f = Flip(0.3)
        val s1 = Select(0.1 -> 1, 0.9 -> 2)
        val s2 = Select(0.7 -> 1, 0.2 -> 2, 0.1 -> 3)
        val c = Chain(f, (b: Boolean) => if (b) s1; else s2)
        s1.observe(1)
        test(c, (i: Int) => i == 1, 0.3 * 1 + 0.7 * 0.7)
        test(f, (b: Boolean) => b, 0.3)
      }

    "with a dependent universe, correctly take into account probability of evidence in the dependent universe" in {
      Universe.createNew()
      val x = Flip(0.1)
      val y = Flip(0.2)
      val dependentUniverse = new Universe(List(x, y))
      val u1 = Uniform(0.0, 1.0)("", dependentUniverse)
      val u2 = Uniform(0.0, 2.0)("", dependentUniverse)
      val a = CachingChain(x, y, (x: Boolean, y: Boolean) => if (x || y) u1; else u2)("a", dependentUniverse)
      val condition = (d: Double) => d < 0.5
      val ve = VariableElimination(List((dependentUniverse, List(NamedEvidence("a", Condition(condition))))), x)
      ve.start()
      val peGivenXTrue = 0.5
      val peGivenXFalse = 0.2 * 0.5 + 0.8 * 0.25
      val unnormalizedPXTrue = 0.1 * peGivenXTrue
      val unnormalizedPXFalse = 0.9 * peGivenXFalse
      val pXTrue = unnormalizedPXTrue / (unnormalizedPXTrue + unnormalizedPXFalse)
      ve.probability(x, true) should be(pXTrue +- 0.01)
      ve.kill()
    }

    "with a contingent condition, correctly take into account the contingency" in {
      Universe.createNew()
      val x = Flip(0.1)
      val y = Flip(0.2)
      y.setCondition((b: Boolean) => b, List(Element.ElemVal(x, true)))
      // Probability of y should be (0.1 * 0.2 + 0.9 * 0.2) / (0.1 * 0.2 + 0.9 * 0.2 + 0.9 * 0.8) (because the case where x is true and y is false has been ruled out)
      val ve = VariableElimination(y)
      ve.start()
      ve.probability(y, true) should be(((0.1 * 0.2 + 0.9 * 0.2) / (0.1 * 0.2 + 0.9 * 0.2 + 0.9 * 0.8)) +- 0.0000000001)
    }
    
    "should not support non-caching chains" in {
      Universe.createNew()
      val f = Flip(0.5)
      val x = NonCachingChain(f, (b: Boolean) => if (b) Constant(0) else Constant(1))
      val ve = VariableElimination(x)
      an [UnsupportedAlgorithmException] should be thrownBy { ve.getNeededElements(List(x), Int.MaxValue) } 
    }
  }

  "MPEVariableElimination" should {
    "compute the most likely values of all the variables given the conditions and constraints" in {
      Universe.createNew()
      val e1 = Flip(0.5)
      e1.setConstraint((b: Boolean) => if (b) 3.0; else 1.0)
      val e2 = If(e1, Flip(0.4), Flip(0.9))
      val e3 = If(e1, Flip(0.52), Flip(0.4))
      val e4 = e2 === e3
      e4.observe(true)
      // p(e1=T,e2=T,e3=T) = 0.75 * 0.4 * 0.52
      // p(e1=T,e2=F,e3=F) = 0.75 * 0.6 * 0.48
      // p(e1=F,e2=T,e3=T) = 0.25 * 0.9 * 0.4
      // p(e1=F,e2=F,e3=F) = 0.25 * 0.1 * 0.6
      // MPE: e1=T,e2=F,e3=F,e4=T
      val alg = MPEVariableElimination()
      alg.start()
      alg.mostLikelyValue(e1) should equal(true)
      alg.mostLikelyValue(e2) should equal(false)
      alg.mostLikelyValue(e3) should equal(false)
      alg.mostLikelyValue(e4) should equal(true)
    }
  }
  
  def test[T](target: Element[T], predicate: T => Boolean, prob: Double) {
    val tolerance = 0.0000001
    val algorithm = VariableElimination(target)
    algorithm.start()
    algorithm.probability(target, predicate) should be(prob +- tolerance)
    algorithm.kill()
  }
}
