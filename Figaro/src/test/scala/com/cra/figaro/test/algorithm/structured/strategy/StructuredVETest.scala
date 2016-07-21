/*
 * StructuredVETest.scala
 * Test of structured variable elimination algorithm.
 *
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   March 1, 2015
 *
 * Copyright 2015 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */
package com.cra.figaro.test.algorithm.structured.strategy

import org.scalatest.{ WordSpec, Matchers }
import com.cra.figaro.language._
import com.cra.figaro.library.compound.If
import com.cra.figaro.algorithm.structured.algorithm.structured.StructuredVE
import com.cra.figaro.algorithm.lazyfactored.ValueSet._
import com.cra.figaro.language.Element.toBooleanElement
import com.cra.figaro.algorithm.structured.algorithm.structured.StructuredMPEVE
import com.cra.figaro.algorithm.factored.MPEVariableElimination
import com.cra.figaro.algorithm.structured.algorithm.structured.StructuredMarginalMAPVE
import com.cra.figaro.library.atomic.discrete.Uniform
import com.cra.figaro.library.collection.Container

class StructuredVETest extends WordSpec with Matchers {
  "Executing a recursive structured VE solver strategy" when {
    "given a flat model with an atomic flip without evidence" should {
      "produce the correct answer" in {
        Universe.createNew()
        val e2 = Flip(0.6)
        val e3 = Apply(e2, (b: Boolean) => b)
        StructuredVE.probability(e3, true) should equal(0.6)
      }
    }

    "given a flat model with a compound Flip without evidence" should {
      "produce the correct answer" in {
        Universe.createNew()
        val e1 = Select(0.25 -> 0.3, 0.25 -> 0.5, 0.25 -> 0.7, 0.25 -> 0.9)
        val e2 = Flip(e1)
        val e3 = Apply(e2, (b: Boolean) => b)
        StructuredVE.probability(e3, true) should equal(0.6)
      }
    }

    "given a flat model with evidence" should {
      "produce the correct answer" in {
        Universe.createNew()
        val e1 = Select(0.25 -> 0.3, 0.25 -> 0.5, 0.25 -> 0.7, 0.25 -> 0.9)
        val e2 = Flip(e1)
        val e3 = Apply(e2, (b: Boolean) => b)
        e3.observe(true)
        StructuredVE.probability(e1, 0.3) should be(0.125 +- 0.000000001)
      }
    }

    "given a model with multiple targets and no evidence" should {
      "produce the correct probability over both targets" in {
        Universe.createNew()
        val e1 = Select(0.25 -> 0.3, 0.25 -> 0.5, 0.25 -> 0.7, 0.25 -> 0.9)
        val e2 = Flip(e1)
        val e3 = Apply(e2, (b: Boolean) => b)
        val alg = StructuredVE(e2, e3)
        alg.start()
        alg.probability(e2, true) should equal(0.6)
        alg.probability(e3, true) should equal(0.6)
      }
    }

    "given a model with multiple targets with evidence" should {
      "produce the correct probability over both targets" in {
        Universe.createNew()
        val e1 = Select(0.25 -> 0.3, 0.25 -> 0.5, 0.25 -> 0.7, 0.25 -> 0.9)
        val e2 = Flip(e1)
        val e3 = Apply(e2, (b: Boolean) => b)
        e3.observe(true)
        val alg = StructuredVE(e2, e1)
        alg.start()
        alg.probability(e2, true) should equal(1.0)
        alg.probability(e1, 0.3) should be(0.125 +- 0.000000001)
      }
    }

    "given a one-level nested model without evidence" should {
      "produce the correct answer" in {
        Universe.createNew()
        val e1 = Select(0.25 -> 0.3, 0.25 -> 0.5, 0.25 -> 0.7, 0.25 -> 0.9)
        val e2 = Flip(e1)
        val e3 = If(e2, Constant(true), Constant(false))
        val alg = StructuredVE(e3)
        alg.start()
        alg.probability(e3, true) should equal(0.6)
      }
    }

    "given a one-level nested model with nested evidence" should {
      "produce the correct answer" in {
        Universe.createNew()
        val e1 = Select(0.25 -> 0.3, 0.25 -> 0.5, 0.25 -> 0.7, 0.25 -> 0.9)
        val e2 = Flip(e1)
        val e3 = If(e2, { val e = Flip(0.5); e.observe(true); e }, Constant(false))
        val alg = StructuredVE(e3)
        alg.start()
        alg.probability(e3, true) should equal(0.6)
      }
    }

    "given a two-level nested model" should {
      "produce the correct answer" in {
        Universe.createNew()
        val e1 = Select(0.25 -> 0.3, 0.25 -> 0.5, 0.25 -> 0.7, 0.25 -> 0.9)
        val e2 = Flip(e1)
        val e3 = If(e2, If(Flip(0.9), Constant(true), Constant(false)), Constant(false))
        val alg = StructuredVE(e3)
        alg.start()
        alg.probability(e3, true) should be((0.6 * 0.9) +- 0.000000001)
      }
    }

    "expanding an element with two different arguments" should {
      "expand both the arguments" in {
        Universe.createNew()
        val e1 = Flip(0.4)
        val e2 = Flip(0.3)
        val e3 = e1 && e2
        StructuredVE.probability(e3, true) should be(0.12 +- 0.000000001)
      }
    }

    "expanding an argument that is used more than once" should {
      "only expand the argument once" in {
        var count = 0
        Universe.createNew()
        val e1 = Apply(Constant(true), (b: Boolean) => { count += 1; 5 })
        val e2 = e1 === e1
        StructuredVE.probability(e2, true) should equal(1.0)
        count should equal(1)
        // Note that this should now only expand once since Apply Maps have been added to Components
      }
    }

    "expanding an argument that needs another argument later expanded" should {
      "create values for the ancestor argument first" in {
        Universe.createNew()
        val e1 = Flip(0.4)
        val e2 = If(e1, Constant(1), Constant(2))
        val e3 = Apply(e2, e1, (i: Int, b: Boolean) => if (b) i + 1 else i + 2)
        // e3 is 2 iff e1 is true, because then e2 is 1
        StructuredVE.probability(e3, 2) should be(0.4 +- 0.000000001)
      }
    }

    "solving a problem with a reused nested subproblem" should {
      "only process the nested subproblem once" in {
        var count = 0
        val f = (p: Boolean) => {
          count += 1
          Constant(p)
        }
        val e1 = Chain(Flip(0.5), f)
        val e2 = Chain(Flip(0.4), f)
        val e3 = e1 && e2
        StructuredVE.probability(e3, true) should be((0.5 * 0.4) +- 0.000000001)
        count should equal(2) // One each for p = true and p = false, but only expanded once
      }
    }

    "given a problem with unneeded elements in the universe" should {
      "not process the unneeded elements" in {
        var count = 0
        val e1 = Apply(Constant(1), (i: Int) => { count += 1; 5 })
        val e2 = Flip(0.5)
        StructuredVE.probability(e2, true) should equal(0.5)
        count should equal(0)
      }
    }
  }

  "MPE VE" when {
    "given a disconnected model should produce the right answer" in {
      Universe.createNew()
      val e1 = Flip(0.4)
      val e2 = Flip(0.6)
      val alg = StructuredMPEVE()
      alg.start
      alg.mostLikelyValue(e1) should equal(false)
      alg.mostLikelyValue(e2) should equal(true)
      alg.kill
    }
    
    "given a flat model without evidence should produce the right answer" in {
      Universe.createNew()
      val e1 = Select(0.75 -> 0.2, 0.25 -> 0.3)
      val e2 = Flip(e1)
      val e3 = Flip(e1)
      val e4 = e2 === e3
      val alg = StructuredMPEVE()
      alg.start
      // p(e1=.2,e2=T,e3=T,e4=T) = 0.75 * 0.2 * 0.2 = .03
      // p(e1=.2,e2=F,e3=F,e4=T) = 0.75 * 0.8 * 0.8 = .48
      // p(e1=.3,e2=T,e3=T,e4=T) = 0.25 * 0.3 * 0.3 = .0225
      // p(e1=.3,e2=F,e3=F,e4=T) = 0.25 * 0.7 * 0.7 = .1225     
      // p(e1=.2,e2=T,e3=F,e4=F) = 0.75 * 0.2 * 0.8 = .12
      // p(e1=.2,e2=F,e3=T,e4=F) = 0.75 * 0.8 * 0.2 = .12
      // p(e1=.3,e2=T,e3=F,e4=F) = 0.25 * 0.3 * 0.7 = .0525
      // p(e1=.3,e2=F,e3=T,e4=F) = 0.25 * 0.7 * 0.3 = .0525
      // MPE: e1=.2,e2=F,e3=F,e4=T
      alg.mostLikelyValue(e1) should be(0.2 +- 0.0000001)
      alg.mostLikelyValue(e2) should equal(false)
      alg.mostLikelyValue(e3) should equal(false)
      alg.mostLikelyValue(e4) should equal(true)
      alg.kill
    }

    "given a flat model with evidence should produce the right answer" in {
      Universe.createNew()
      val e1 = Select(0.5 -> 0.2, 0.5 -> 0.3)
      e1.addConstraint((d: Double) => if (d < 0.25) 3.0 else 1.0)
      val e2 = Flip(e1)
      val e3 = Flip(e1)
      val e4 = e2 === e3
      e4.observe(true)
      val alg = StructuredMPEVE()
      alg.start
      // p(e1=.2,e2=T,e3=T,e4=T) = 0.75 * 0.2 * 0.2 = .03
      // p(e1=.2,e2=F,e3=F,e4=T) = 0.75 * 0.8 * 0.8 = .48
      // p(e1=.3,e2=T,e3=T,e4=T) = 0.25 * 0.3 * 0.3 = .0225
      // p(e1=.3,e2=F,e3=F,e4=T) = 0.25 * 0.7 * 0.7 = .1225     
      // MPE: e1=.2,e2=F,e3=F,e4=T
      alg.mostLikelyValue(e1) should be(0.2 +- 0.0000001)
      alg.mostLikelyValue(e2) should equal(false)
      alg.mostLikelyValue(e3) should equal(false)
      alg.mostLikelyValue(e4) should equal(true)
      alg.kill
    }

    "given a structured model with evidence should produce the right answer" in {
      Universe.createNew()
      val e1 = Flip(0.5)
      e1.setConstraint((b: Boolean) => if (b) 3.0; else 1.0)
      val e2 = Chain(e1, (b: Boolean) => {
        if (b) Flip(0.4) || Flip(0.2)
        else Flip(0.9) || Flip(0.2)
      })
      val e3 = If(e1, Flip(0.52), Flip(0.4))
      val e4 = e2 === e3
      e4.observe(true)
      // p(e1=T,e2=T,f1=T,f2=T,e3=T) = 0.75 * 0.4 * 0.2 * 0.52 = .0312
      // p(e1=T,e2=T,f1=T,f2=F,e3=T) = 0.75 * 0.4 * 0.8 * 0.52 = .1248
      // p(e1=T,e2=T,f1=F,f2=T,e3=T) = 0.75 * 0.6 * 0.2 * 0.52 = .0468
      // p(e1=T,e2=F,f1=F,f2=F,e3=F) = 0.75 * 0.6 * 0.8 * 0.48 = .1728
      // p(e1=F,e2=T,f1=T,f2=T,e3=T) = 0.25 * 0.9 * 0.2 * 0.4 = .018
      // p(e1=F,e2=T,f1=T,f2=F,e3=T) = 0.25 * 0.9 * 0.8 * 0.4 = .072
      // p(e1=F,e2=T,f1=F,f2=T,e3=T) = 0.25 * 0.1 * 0.2 * 0.4 = .002
      // p(e1=F,e2=F,f1=F,f2=F,e3=F) = 0.25 * 0.1 * 0.8 * 0.6 = .012
      // MPE: e1=T,e2=F,e3=F,e4=T
      val alg = StructuredMPEVE()
      alg.start
      alg.mostLikelyValue(e1) should equal(true)
      alg.mostLikelyValue(e2) should equal(false)
      alg.mostLikelyValue(e3) should equal(false)
      alg.mostLikelyValue(e4) should equal(true)
      alg.kill
    }

  }

  "Marginal MAP VE" when {
    "given a model with MAP queries on all elements" should {
      "produce the right answer without evidence" in {
        Universe.createNew()
        val a = Flip(0.8)
        val b11 = Flip(0.7)
        val b12 = Flip(0.6)
        val b1 = b11 && b12
        val b2 = Constant(false)
        val b = If(a, b1, b2)
        // Even though b is a Chain, the result elements of b are MAP elements
        // This should produce the same result as an MPE query

        // p(a=T,b11=T,b12=T) = 0.8 * 0.7 * 0.6 = 0.336
        // p(a=T,b11=T,b12=F) = 0.8 * 0.7 * 0.4 = 0.224
        // p(a=T,b11=F,b12=T) = 0.8 * 0.3 * 0.6 = 0.144
        // p(a=T,b11=F,b12=F) = 0.8 * 0.3 * 0.4 = 0.096
        // p(a=F,b11=T,b12=T) = 0.2 * 0.7 * 0.6 = 0.084
        // p(a=F,b11=T,b12=F) = 0.2 * 0.7 * 0.4 = 0.054
        // p(a=F,b11=F,b12=T) = 0.2 * 0.3 * 0.6 = 0.036
        // p(a=F,b11=F,b12=F) = 0.2 * 0.3 * 0.4 = 0.024
        // MAP: a=T,b11=T,b12=T which implies b1=T,b2=F,b=T
        val alg = StructuredMarginalMAPVE(a, b11, b12, b1, b2, b)
        alg.start
        alg.mostLikelyValue(a) should equal(true)
        alg.mostLikelyValue(b11) should equal(true)
        alg.mostLikelyValue(b12) should equal(true)
        alg.mostLikelyValue(b1) should equal(true)
        alg.mostLikelyValue(b2) should equal(false)
        alg.mostLikelyValue(b) should equal(true)
        alg.kill
      }

      "produce the right answer with evidence" in {
        Universe.createNew()
        val a = Flip(0.8)
        val b11 = Flip(0.7)
        val b12 = Flip(0.6)
        val b1 = b11 && b12
        val b2 = Constant(false)
        val b = If(a, b1, b2)
        b.observe(false)
        // Even though b is a Chain, the result elements of b are MAP elements
        // This should produce the same result as an MPE query

        // These weights are not normalized
        // p(a=T,b11=T,b12=T) = 0
        // p(a=T,b11=T,b12=F) = 0.8 * 0.7 * 0.4 = 0.224
        // p(a=T,b11=F,b12=T) = 0.8 * 0.3 * 0.6 = 0.144
        // p(a=T,b11=F,b12=F) = 0.8 * 0.3 * 0.4 = 0.096
        // p(a=F,b11=T,b12=T) = 0.2 * 0.7 * 0.6 = 0.084
        // p(a=F,b11=T,b12=F) = 0.2 * 0.7 * 0.4 = 0.054
        // p(a=F,b11=F,b12=T) = 0.2 * 0.3 * 0.6 = 0.036
        // p(a=F,b11=F,b12=F) = 0.2 * 0.3 * 0.4 = 0.024
        // MAP: a=T,b11=T,b12=F which implies b1=F,b2=F,b=F
        val alg = StructuredMarginalMAPVE(a, b11, b12, b1, b2, b)
        alg.start
        alg.mostLikelyValue(a) should equal(true)
        alg.mostLikelyValue(b11) should equal(true)
        alg.mostLikelyValue(b12) should equal(false)
        alg.mostLikelyValue(b1) should equal(false)
        alg.mostLikelyValue(b2) should equal(false)
        alg.mostLikelyValue(b) should equal(false)
        alg.kill
      }
    }

    "given a model with MAP queries on all permanent elements" should {
      "produce the right answer without evidence" in {
        Universe.createNew()
        val a = Flip(0.6)
        val b = If(a, Flip(0.3) && Flip(0.5), Flip(0.4))
        // The result elements of b are marginalized
        // Then, the first result element of b is effectively a Flip(0.15)

        // p(a=T,b=T) = 0.6 * 0.15 = 0.09
        // p(a=T,b=F) = 0.6 * 0.85 = 0.51
        // p(a=F,b=T) = 0.4 * 0.4 = 0.16
        // p(a=F,b=F) = 0.4 * 0.6 = 0.24
        // MAP: a=T,b=F
        val alg = StructuredMarginalMAPVE(a, b)
        alg.start
        alg.mostLikelyValue(a) should equal(true)
        alg.mostLikelyValue(b) should equal(false)
        alg.kill
      }

      "produce the right answer with evidence" in {
        Universe.createNew()
        val a = Flip(0.6)
        val b = If(a, Flip(0.3) && Flip(0.5), Flip(0.4))
        b.observe(true)
        // The result elements of b are marginalized
        // Then, the first result element of b is effectively a Flip(0.15)

        // These weights are not normalized
        // p(a=T,b=T) = 0.6 * 0.15 = 0.09
        // p(a=T,b=F) = 0
        // p(a=F,b=T) = 0.4 * 0.4 = 0.16
        // p(a=F,b=F) = 0
        // MAP: a=F,b=T
        val alg = StructuredMarginalMAPVE(a, b)
        alg.start
        alg.mostLikelyValue(a) should equal(false)
        alg.mostLikelyValue(b) should equal(true)
        alg.kill
      }
    }

    "given a model with MAP queries on a single element" should {
      "produce the right answer without evidence" in {
        Universe.createNew()
        val rolls = for { i <- 1 to 10 } yield Uniform(1,2,3,4)
        val c = Container(rolls: _*)
        val num4 = c.count(_ == 4)

        // num4 is effectively a binomial distribution with n=10, p=0.25
        // The mode is floor(p*(n+1))=2
        val alg = StructuredMarginalMAPVE(num4)
        alg.start
        alg.mostLikelyValue(num4) should equal(2)
        alg.kill
      }

      "produce the right answer with evidence" in {
        val rolls = for { i <- 1 to 10 } yield Uniform(1,2,3,4)
        val c = Container(rolls: _*)
        val num4 = c.count(_ == 4)

        num4.addCondition(_ >= 5)

        // Since the pmf of a binomial distribution is strictly decreasing past the mode,
        // the most likely value should be the least possible value given the evidence
        val alg = StructuredMarginalMAPVE(num4)
        alg.start
        alg.mostLikelyValue(num4) should equal(5)
        alg.kill
      }
    }

    "given a model with MAP queries on more than one element" should {
      "produce the right answer without evidence" in {
        Universe.createNew()
        val a = Flip(0.6)
        val b = If(a, Flip(0.7), Flip(0.4))
        val c = If(a, Flip(0.6), Flip(0.1))
        val d = If(b, Flip(0.1), Flip(0.6))

        // p(a=T,b=T,c=T,d=T)=0.6*0.7*0.6*0.1=0.0252
        // p(a=T,b=T,c=T,d=F)=0.6*0.7*0.6*0.9=0.2268
        // p(a=T,b=T,c=F,d=T)=0.6*0.7*0.4*0.1=0.0168
        // p(a=T,b=T,c=F,d=F)=0.6*0.7*0.4*0.9=0.1512
        // p(a=T,b=F,c=T,d=T)=0.6*0.3*0.6*0.6=0.0648
        // p(a=T,b=F,c=T,d=F)=0.6*0.3*0.6*0.4=0.0432
        // p(a=T,b=F,c=F,d=T)=0.6*0.3*0.4*0.6=0.0432
        // p(a=T,b=F,c=F,d=F)=0.6*0.3*0.4*0.4=0.0288
        // p(a=F,b=T,c=T,d=T)=0.4*0.4*0.1*0.1=0.0016
        // p(a=F,b=T,c=T,d=F)=0.4*0.4*0.1*0.9=0.0144
        // p(a=F,b=T,c=F,d=T)=0.4*0.4*0.9*0.1=0.0144
        // p(a=F,b=T,c=F,d=F)=0.4*0.4*0.9*0.9=0.1296
        // p(a=F,b=F,c=T,d=T)=0.4*0.6*0.1*0.6=0.0144
        // p(a=F,b=F,c=T,d=F)=0.4*0.6*0.1*0.4=0.0096
        // p(a=F,b=F,c=F,d=T)=0.4*0.6*0.9*0.6=0.1296
        // p(a=F,b=F,c=F,d=F)=0.4*0.6*0.9*0.4=0.0864

        // p(c=T,d=T)=0.0252+0.0648+0.0016+0.0144=0.106
        // p(c=T,d=F)=0.2268+0.0432+0.0144+0.0096=0.294
        // p(c=F,d=T)=0.0168+0.0432+0.0144+0.1296=0.204
        // p(c=F,d=F)=0.1512+0.0288+0.1296+0.0864=0.396 -> MAP

        val alg = StructuredMarginalMAPVE(c, d)
        alg.start()
        alg.mostLikelyValue(c) should equal(false)
        alg.mostLikelyValue(d) should equal(false)
        alg.kill()
      }

      "produce the right answer with evidence" in {
        Universe.createNew()
        val a = Flip(0.6)
        val b = If(a, Flip(0.7), Flip(0.4))
        val c = If(a, Flip(0.6), Flip(0.1))
        val d = If(b, Flip(0.1), Flip(0.6))

        // p(a=T,b=T,c=T,d=T)=0.6*0.7*0.6*0.1=0.0252
        // p(a=T,b=T,c=T,d=F)=0.6*0.7*0.6*0.9=0.2268
        // p(a=T,b=T,c=F,d=T)=0.6*0.7*0.4*0.1=0.0168
        // p(a=T,b=T,c=F,d=F)=0.6*0.7*0.4*0.9=0.1512
        // p(a=T,b=F,c=T,d=T)=0.6*0.3*0.6*0.6=0.0648
        // p(a=T,b=F,c=T,d=F)=0.6*0.3*0.6*0.4=0.0432
        // p(a=T,b=F,c=F,d=T)=0.6*0.3*0.4*0.6=0.0432
        // p(a=T,b=F,c=F,d=F)=0.6*0.3*0.4*0.4=0.0288
        // p(a=F,b=T,c=T,d=T)=0.4*0.4*0.1*0.1=0.0016
        // p(a=F,b=T,c=T,d=F)=0.4*0.4*0.1*0.9=0.0144
        // p(a=F,b=T,c=F,d=T)=0.4*0.4*0.9*0.1=0.0144
        // p(a=F,b=T,c=F,d=F)=0.4*0.4*0.9*0.9=0.1296
        // p(a=F,b=F,c=T,d=T)=0.4*0.6*0.1*0.6=0.0144
        // p(a=F,b=F,c=T,d=F)=0.4*0.6*0.1*0.4=0.0096
        // p(a=F,b=F,c=F,d=T)=0.4*0.6*0.9*0.6=0.1296
        // p(a=F,b=F,c=F,d=F)=0.4*0.6*0.9*0.4=0.0864

        // These weights are not normalized
        // p(c=T,d=T)=0.0252+0.0648+0.0016+0.0144=0.106
        // p(c=T,d=F)=0.2268+0.0432+0.0144+0.0096=0.294 -> MAP
        // p(c=F,d=T)=0.0168+0.0432+0.0144+0.1296=0.204
        // p(c=F,d=F)=0

        (c || d).observe(true)

        val alg = StructuredMarginalMAPVE(c, d)
        alg.start()
        alg.mostLikelyValue(c) should equal(true)
        alg.mostLikelyValue(d) should equal(false)
        alg.kill()
      }
    }
  }
}
