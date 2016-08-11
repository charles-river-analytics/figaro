/*
 * StructuredMMAPVETest.scala
 * Structured marginal MAP VE tests.
 *
 * Created By:      William Kretschmer (kretsch@mit.edu)
 * Creation Date:   Aug 11, 2016
 *
 * Copyright 2016 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.test.experimental.marginalmap

import com.cra.figaro.experimental.marginalmap.StructuredMarginalMAPVE
import com.cra.figaro.language._
import com.cra.figaro.library.atomic.discrete.Uniform
import com.cra.figaro.library.collection.Container
import com.cra.figaro.library.compound.If
import org.scalatest.{Matchers, WordSpec}

class StructuredMMAPVETest extends WordSpec with Matchers {
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
