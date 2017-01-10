/*
 * StructuredBPTest.scala
 * Test of structure belief propagation algorithm.
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
import com.cra.figaro.algorithm.structured.algorithm.structured.StructuredBP
import com.cra.figaro.algorithm.lazyfactored.ValueSet._
import com.cra.figaro.language.Element.toBooleanElement
import com.cra.figaro.algorithm.structured.algorithm.structured.StructuredMPEBP

class StructuredBPTest extends WordSpec with Matchers {
  "Executing a recursive structured BP solver strategy" when {
    "given a flat model with an atomic flip without evidence" should {
      "produce the correct answer" in {
        Universe.createNew()
        val e2 = Flip(0.6)
        val e3 = Apply(e2, (b: Boolean) => b)
        StructuredBP.probability(e3, true) should be(0.6 +- 0.000000001)
      }
    }

    "given a flat model with a compound Flip without evidence" should {
      "produce the correct answer" in {
        Universe.createNew()
        val e1 = Select(0.25 -> 0.3, 0.25 -> 0.5, 0.25 -> 0.7, 0.25 -> 0.9)
        val e2 = Flip(e1)
        val e3 = Apply(e2, (b: Boolean) => b)
        StructuredBP.probability(e3, true) should be(0.6 +- 0.000000001)
      }
    }

    "given a flat model with evidence" should {
      "produce the correct answer" in {
        Universe.createNew()
        val e1 = Select(0.25 -> 0.3, 0.25 -> 0.5, 0.25 -> 0.7, 0.25 -> 0.9)
        val e2 = Flip(e1)
        val e3 = Apply(e2, (b: Boolean) => b)
        e3.observe(true)
        StructuredBP.probability(e1, 0.3) should be(0.125 +- 0.000000001)
      }
    }

    "given a model with multiple targets and no evidence" should {
      "produce the correct probability over both targets" in {
        Universe.createNew()
        val e1 = Select(0.25 -> 0.3, 0.25 -> 0.5, 0.25 -> 0.7, 0.25 -> 0.9)
        val e2 = Flip(e1)
        val e3 = Apply(e2, (b: Boolean) => b)
        val alg = StructuredBP(100, e2, e3)
        alg.start()
        alg.probability(e2, true) should be(0.6 +- 0.000000001)
        alg.probability(e3, true) should equal(0.6 +- 0.000000001)
      }
    }

    "given a model with multiple targets with evidence" should {
      "produce the correct probability over both targets" in {
        Universe.createNew()
        val e1 = Select(0.25 -> 0.3, 0.25 -> 0.5, 0.25 -> 0.7, 0.25 -> 0.9)
        val e2 = Flip(e1)
        val e3 = Apply(e2, (b: Boolean) => b)
        e3.observe(true)
        val alg = StructuredBP(100, e2, e1)
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
        val alg = StructuredBP(100, e3)
        alg.start()
        alg.probability(e3, true) should be(0.6 +- 0.000000001)
      }
    }

    /*
    "given a one-level nested model with nested evidence" should {
      "produce the correct answer" in {
        Universe.createNew()
        val e1 = Select(0.25 -> 0.3, 0.25 -> 0.5, 0.25 -> 0.7, 0.25 -> 0.9)
        val e2 = Flip(e1)
        val e3 = If(e2, { val e = Flip(0.5); e.observe(true); e }, Constant(false))
        val alg = StructuredBP(100, e3)
        alg.start()
        alg.probability(e3, true) should be(0.6 +- 0.000000001)
      }
    }
    * 
    */

    "given a two-level nested model" should {
      "produce the correct answer" in {
        Universe.createNew()
        val e1 = Select(0.25 -> 0.3, 0.25 -> 0.5, 0.25 -> 0.7, 0.25 -> 0.9)
        val e2 = Flip(e1)
        val e3 = If(e2, If(Flip(0.9), Constant(true), Constant(false)), Constant(false))
        val alg = StructuredBP(100, e3)
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
        StructuredBP.probability(e3, true) should be(0.12 +- 0.000000001)
      }
    }

    "expanding an argument that is used more than once" should {
      "only expand the argument once" in {
        var count = 0
        Universe.createNew()
        val e1 = Apply(Constant(true), (b: Boolean) => { count += 1; 5 })
        val e2 = e1 === e1
        StructuredBP.probability(e2, true) should equal(1.0)
        count should equal(1)
        // Note that this should now only expand once since Apply Maps have been added to Components
      }
    }

    // The below test is loopy so BP's answer can't be predicted easily
    //    "expanding an argument that needs another argument later expanded" should {
    //      "create values for the ancestor argument first" in {
    //        Universe.createNew()
    //        val e1 = Flip(0.4)
    //        val e2 = If(e1, Constant(1), Constant(2))
    //        val e3 = Apply(e2, e1, (i: Int, b: Boolean) => if (b) i + 1 else i + 2)
    //        // e3 is 2 iff e1 is true, because then e2 is 1
    //        StructuredBP.probability(e3, 2) should be (0.4 +- 0.000000001)
    //      }
    //    }

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
        StructuredBP.probability(e3, true) should be((0.5 * 0.4) +- 0.000000001)
        count should equal(2) // One each for p = true and p = false, but only expanded once
      }
    }

    "given a problem with unneeded elements in the universe" should {
      "not process the unneeded elements" in {
        var count = 0
        val e1 = Apply(Constant(1), (i: Int) => { count += 1; 5 })
        val e2 = Flip(0.5)
        StructuredBP.probability(e2, true) should equal(0.5)
        count should equal(0)
      }
    }
  }

  "MPE BP" when {
    "given a flat model without evidence should produce the right answer" in {
      Universe.createNew()
      val e1 = Select(0.75 -> 0.2, 0.25 -> 0.3)
      val e2 = Flip(e1)
      val e3 = Flip(e1)
      val e4 = e2 === e3
      val alg = StructuredMPEBP(20)
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
      val alg = StructuredMPEBP(20)
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
      val alg = StructuredMPEBP(20)
      alg.start
      alg.mostLikelyValue(e1) should equal(true)
      alg.mostLikelyValue(e2) should equal(false)
      alg.mostLikelyValue(e3) should equal(false)
      alg.mostLikelyValue(e4) should equal(true)
      alg.kill
    }
  }
}
