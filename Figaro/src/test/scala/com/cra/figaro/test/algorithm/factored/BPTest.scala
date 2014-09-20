/*
 * BPTest.scala  
 * Belief Propagation tests.
 * 
 * Created By:      Brian Ruttenberg (bruttenberg@cra.com)
 * Creation Date:   Jan 15, 2014
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.test.algorithm.factored

import scala.collection.mutable.Map
import org.scalatest.WordSpec
import org.scalatest.Matchers
import com.cra.figaro.algorithm.factored._
import com.cra.figaro.algorithm.factored.beliefpropagation._
import com.cra.figaro.language._
import com.cra.figaro.library.compound.If
import com.cra.figaro.library.atomic.discrete.{Uniform => DUniform}
import com.cra.figaro.library.atomic.continuous.{Uniform => CUniform}
import com.cra.figaro.library.compound.IntSelector
import com.cra.figaro.algorithm.lazyfactored.LazyValues
import com.cra.figaro.algorithm.UnsupportedAlgorithmException

class BPTest extends WordSpec with Matchers {

  val globalTol = 0.025

  "A basic factor graph" should {
    "Create nodes for all factors and variables" in {
      Universe.createNew
      val u = Select(0.25 -> 0.3, 0.25 -> 0.5, 0.25 -> 0.7, 0.25 -> 0.9)
      val f = Flip(u)
      val a = If(f, Select(0.3 -> 1, 0.7 -> 2), Constant(2))
      val semiring = SumProductSemiring
      LazyValues(Universe.universe).expandAll(Universe.universe.activeElements.toSet.map((elem: Element[_]) => ((elem, Integer.MAX_VALUE))))
      val factors = Universe.universe.activeElements flatMap (Factory.make(_))
      val graph = new BasicFactorGraph(factors, semiring)
      val fn = graph.adjacencyList.filter(p => { p._1 match { case fn: FactorNode => true; case _ => false; } })
      val vn = graph.adjacencyList.filter(p => { p._1 match { case vn: VariableNode => true; case _ => false; } })

      fn.size should equal(6)
      vn.size should equal(5)
    }

    "Create an edge to between each factor and the variables it has" in {
      Universe.createNew
      val u = Select(0.25 -> 0.3, 0.25 -> 0.5, 0.25 -> 0.7, 0.25 -> 0.9)
      val f = Flip(u)
      val a = If(f, Select(0.3 -> 1, 0.7 -> 2), Constant(2))
      val semiring = SumProductSemiring
      LazyValues(Universe.universe).expandAll(Universe.universe.activeElements.toSet.map((elem: Element[_]) => ((elem, Integer.MAX_VALUE))))
      val factors = Universe.universe.activeElements flatMap (Factory.make(_))
      val graph = new BasicFactorGraph(factors, semiring)
      val fn = graph.adjacencyList.filter(p => { p._1 match { case fn: FactorNode => true; case _ => false; } })
      val vn = graph.adjacencyList.filter(p => { p._1 match { case vn: VariableNode => true; case _ => false; } })

      fn.foreach(n => {
        val vars = n._1.asInstanceOf[FactorNode].variables.toSet
        val edges = n._2.keys.map(v => v.asInstanceOf[VariableNode].variable).toSet
        vars should equal(edges)
        n._2.keys.foreach(v => graph.adjacencyList(v).contains(n._1) should equal(true))
      })

    }

  }

  "Running BeliefPropagation" should {
    "Send the correct type of message" in {
      Universe.createNew
      val u = Select(0.25 -> 0.3, 0.25 -> 0.5, 0.25 -> 0.7, 0.25 -> 0.9)
      val f = Flip(u)
      val a = If(f, Select(0.3 -> 1, 0.7 -> 2), Constant(2))
      val bp = BeliefPropagation(3)
      val fn = bp.factorGraph.adjacencyList.filter(p => { p._1 match { case fn: FactorNode => true; case _ => false; } })
      val vn = bp.factorGraph.adjacencyList.filter(p => { p._1 match { case vn: VariableNode => true; case _ => false; } })

      fn.foreach { s =>
        s._2.foreach { d =>
          val msg = bp.newMessage(s._1, d._1)
          msg.variables should equal(List(d._1.asInstanceOf[VariableNode].variable))
        }
      }

      vn.foreach { s =>
        s._2.foreach { d =>
          val msg = bp.newMessage(s._1, d._1)
          msg.variables should equal(List(s._1.asInstanceOf[VariableNode].variable))
        }
      }
    }

    /* Due to the way that factors are implemented for Chain, all 
     * models that use chain will result in loops. To test a non-loopy
     * graph we have to not use chain, which IntSelector does not.
     */
    "with no loops in the factor graph give exact results" in {
      Universe.createNew()
      val e1 = DUniform(2, 3, 4)
      val e2 = IntSelector(e1)

      val bp = BeliefPropagation(3, e2)
      bp.start()

      val e2_0 = 0.33333333 * (0.5 + 0.3333333 + 0.25)
      val e2_1 = 0.33333333 * (0.5 + 0.3333333 + 0.25)
      val e2_2 = 0.33333333 * (0 + 0.3333333 + 0.25)
      val e2_3 = 0.33333333 * (0 + 0 + 0.25)

      val tol = 0.000001
      bp.probability(e2, (i: Int) => i == 0) should be(e2_0 +- tol)
      bp.probability(e2, (i: Int) => i == 1) should be(e2_1 +- tol)
      bp.probability(e2, (i: Int) => i == 2) should be(e2_2 +- tol)
      bp.probability(e2, (i: Int) => i == 3) should be(e2_3 +- tol)
    }

    "with no conditions or constraints produce the correct result" in {
      Universe.createNew()
      val u = Select(0.25 -> 0.3, 0.25 -> 0.5, 0.25 -> 0.7, 0.25 -> 0.9)
      val f = Flip(u)
      val a = If(f, Select(0.3 -> 1, 0.7 -> 2), Constant(2))
      test(f, (b: Boolean) => b, 0.6, globalTol)
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
      test(f, (b: Boolean) => b, u1 / (u1 + u2), globalTol)
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
      test(f, (b: Boolean) => b, u1 / (u1 + u2), globalTol)
    }

    "with an element that uses another element multiple times, " +
      "always produce the same value for the different uses" in {
        Universe.createNew()
        val f = Flip(0.5)
        val e = f === f
        test(e, (b: Boolean) => b, 1.0, globalTol)
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
      test(d, (b: Boolean) => b, 0.73, globalTol)
    }

    "on a different universe from the current universe, produce the correct result" in {
      val u1 = Universe.createNew()
      val u = Select(0.25 -> 0.3, 0.25 -> 0.5, 0.25 -> 0.7, 0.25 -> 0.9)
      val f = Flip(u)
      val a = If(f, Select(0.3 -> 1, 0.7 -> 2), Constant(2))
      Universe.createNew()
      val tolerance = 0.0000001
      val algorithm = BeliefPropagation(10, f)(u1)
      algorithm.start()
      algorithm.probability(f, (b: Boolean) => b) should be(0.6 +- globalTol)
      algorithm.kill()
    }

    "with a model using chain and no conditions or constraints, produce the correct answer" in {
      Universe.createNew()
      val f = Flip(0.3)
      val s1 = Select(0.1 -> 1, 0.9 -> 2)
      val s2 = Select(0.7 -> 1, 0.2 -> 2, 0.1 -> 3)
      val c = Chain(f, (b: Boolean) => if (b) s1; else s2)
      test(c, (i: Int) => i == 1, 0.3 * 0.1 + 0.7 * 0.7, globalTol)
    }

    "with a model using chain and a condition on the result, correctly condition the parent" in {
      Universe.createNew()
      val f = Flip(0.3)
      val s1 = Select(0.1 -> 1, 0.9 -> 2)
      val s2 = Select(0.7 -> 1, 0.2 -> 2, 0.1 -> 3)
      val c = Chain(f, (b: Boolean) => if (b) s1; else s2)
      c.observe(1)
      test(f, (b: Boolean) => b, 0.3 * 0.1 / (0.3 * 0.1 + 0.7 * 0.7), globalTol)
    }

    "with a model using chain and a condition on one of the outcome elements, correctly condition the result but not change the belief about the parent" in {
        Universe.createNew()
        val f = Flip(0.3)
        val s1 = Select(0.1 -> 1, 0.9 -> 2)
        val s2 = Select(0.7 -> 1, 0.2 -> 2, 0.1 -> 3)
        val c = Chain(f, (b: Boolean) => if (b) s1; else s2)

        s1.observe(1)
        //val c_actual = .79
        val c_actual = .70907

        /*
         * The "c_actual" value has been determine using Dimple to back up the results of Figaro. This exact
         * same factor graph was run on dimple and returns .70907 for i == 1
         * The same is done with 'f' (set as an increased tolerance below),
         *  although this test is not doing much since the while point it to show
         * that f does not change, at least it does not change significantly
         * 
         * The factor model is no longer loopy so the exact result .79 applies (Glenn)
         * 
         * UPDATE: We're going back to loopy since factor combining in ProbFactor is not default in BP.
         */
        test(c, (i: Int) => i == 1, c_actual, globalTol)
        test(f, (b: Boolean) => b, 0.3, 0.06)
      }

    "with a dependent universe, correctly take into account probability of evidence in the dependent universe" in {
      Universe.createNew()
      //val x = Flip(0.1)
      //val y = Flip(0.2)
      val x = IntSelector(Constant(10))
      val y = IntSelector(Constant(10))
      val x1 = Apply(x, (i: Int) => i < 1)
      val y1 = Apply(y, (i: Int) => i < 2)
      val dependentUniverse = new Universe(List(x1, y1))
      val u1 = CUniform(0.0, 1.0)("", dependentUniverse)
      val u2 = CUniform(0.0, 2.0)("", dependentUniverse)

      val a = CachingChain(x1, y1, (x: Boolean, y: Boolean) => if (x || y) u1; else u2)("a", dependentUniverse)
      val condition = (d: Double) => d < 0.5
      val ve = BeliefPropagation(List((dependentUniverse, List(NamedEvidence("a", Condition(condition))))), 200, x1)
      ve.start()
      val peGivenXTrue = 0.5
      val peGivenXFalse = 0.2 * 0.5 + 0.8 * 0.25
      val unnormalizedPXTrue = 0.1 * peGivenXTrue
      val unnormalizedPXFalse = 0.9 * peGivenXFalse
      val pXTrue = unnormalizedPXTrue / (unnormalizedPXTrue + unnormalizedPXFalse)
      ve.probability(x1, true) should be(pXTrue +- globalTol)
      ve.kill()
    }

    "with a contingent condition, correctly take into account the contingency" in {
      Universe.createNew()
      val x = Flip(0.1)
      val y = Flip(0.2)
      y.setCondition((b: Boolean) => b, List(Element.ElemVal(x, true)))
      // Probability of y should be (0.1 * 0.2 + 0.9 * 0.2) / (0.1 * 0.2 + 0.9 * 0.2 + 0.9 * 0.8) (because the case where x is true and y is false has been ruled out)
      val ve = BeliefPropagation(50, y)
      ve.start()
      ve.probability(y, true) should be(((0.1 * 0.2 + 0.9 * 0.2) / (0.1 * 0.2 + 0.9 * 0.2 + 0.9 * 0.8)) +- globalTol)
    }
    
    "should not underflow" in {
      Universe.createNew()
      val x = Flip(0.99)
      for (i <- 0 until 10) {
        x.addConstraint((b: Boolean) => if (b) 1e-100; else 1e-120)
      }
      val bp = BeliefPropagation(5, x)
      bp.start()
      bp.probability(x, true) should be (1.0)
    }
        
    "should not support non-caching chains" in {
      Universe.createNew()
      val f = Flip(0.5)
      val x = NonCachingChain(f, (b: Boolean) => if (b) Constant(0) else Constant(1))
      val ve = BeliefPropagation(50)
      an [UnsupportedAlgorithmException] should be thrownBy { ve.getNeededElements(List(x), Int.MaxValue) } 
    }
  }

  "MaxProductBeliefPropagation" should {
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
      val alg = MPEBeliefPropagation(20)
      alg.start()
      alg.mostLikelyValue(e1) should equal(true)
      alg.mostLikelyValue(e2) should equal(false)
      alg.mostLikelyValue(e3) should equal(false)
      alg.mostLikelyValue(e4) should equal(true)
    }

  }

  def test[T](target: Element[T], predicate: T => Boolean, prob: Double, tol: Double) {
    val algorithm = BeliefPropagation(100, target)
    algorithm.start()
    algorithm.probability(target, predicate) should be(prob +- tol)
  }
}