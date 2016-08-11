/*
 * ProbEvidenceMarginalMAPTest.scala
 * Tests for probability of evidence-based marginal MAP.
 *
 * Created By:      William Kretschmer (kretsch@mit.edu)
 * Creation Date:   Aug 1, 2016
 *
 * Copyright 2016 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.test.experimental.marginalmap

import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.experimental.marginalmap.ProbEvidenceMarginalMAP
import com.cra.figaro.language._
import com.cra.figaro.library.atomic.continuous.Normal
import com.cra.figaro.library.compound.If
import org.scalatest.{Matchers, WordSpec}

class ProbEvidenceMarginalMAPTest extends WordSpec with Matchers {
  "Marginal MAP using probability of evidence" should {
    "increase temperature with additional iterations" in {
      Universe.createNew()
      val elem = Flip(0.6)

      val alg = ProbEvidenceMarginalMAP(1, ProposalScheme(elem), Schedule.default(), elem)
      alg.start()
      val temp1 = alg.getTemperature
      Thread.sleep(500)
      val temp2 = alg.getTemperature
      alg.kill()

      temp2 should be > temp1
    }

    "increase temperature faster with lower k" in {
      Universe.createNew()
      val elem = Flip(0.6)

      // k = 2.0
      val alg1 = ProbEvidenceMarginalMAP(100, 1, ProposalScheme(elem), Schedule.default(2.0), elem)
      alg1.start()
      val temp1 = alg1.getTemperature
      alg1.kill()

      // k = 4.0
      val alg2 = ProbEvidenceMarginalMAP(100, 1, ProposalScheme(elem), Schedule.default(4.0), elem)
      alg2.start()
      val temp2 = alg2.getTemperature
      alg2.kill()

      temp2 should be < temp1
    }
  }

  "Running anytime marginal MAP using probability of evidence" when {
    "given a model with a MAP query on a top-level parameter" should {
      "correctly estimate the parameter with evidence" in {
        Universe.createNew()
        val parameterMean = 5.0
        val parameterVariance = 1.0
        val variance = 1.0

        // We're using paramater as a prior to estimate a normal with known variance
        val parameter = Normal(parameterMean, parameterVariance)

        val observations = List(6.1, 7.3, 5.8, 5.3, 6.4)
        for(obs <- observations) {
          Normal(parameter, variance).observe(obs)
        }

        // The MAP value of parameter is just the the posterior mean
        val meanEstimate = (parameterMean / parameterVariance + observations.sum / variance) /
          (1.0 / parameterVariance + observations.length / variance)

        val alg = ProbEvidenceMarginalMAP(1, ProposalScheme(parameter), Schedule.default(), parameter)
        alg.start()
        Thread.sleep(2500)
        alg.stop()
        alg.mostLikelyValue(parameter) should equal(meanEstimate +- 0.01)
        alg.kill()
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
        val alg = ProbEvidenceMarginalMAP(200, a, b)
        alg.start()
        Thread.sleep(5000)
        alg.stop()
        alg.mostLikelyValue(a) should equal(true)
        alg.mostLikelyValue(b) should equal(false)
        alg.kill()
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
        val alg = ProbEvidenceMarginalMAP(200, a, b)
        alg.start()
        Thread.sleep(5000)
        alg.stop()
        alg.mostLikelyValue(a) should equal(false)
        alg.mostLikelyValue(b) should equal(true)
        alg.kill()
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

        val alg = ProbEvidenceMarginalMAP(50, c, d)
        alg.start()
        Thread.sleep(5000)
        alg.stop()
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

        val alg = ProbEvidenceMarginalMAP(50, c, d)
        alg.start()
        Thread.sleep(5000)
        alg.stop()
        alg.mostLikelyValue(c) should equal(true)
        alg.mostLikelyValue(d) should equal(false)
        alg.kill()
      }
    }
  }

  "Running one time marginal MAP using probability of evidence" when {
    "given a model with a MAP query on a top-level parameter" should {
      "correctly estimate the parameter with evidence" in {
        Universe.createNew()
        val parameterMean = 5.0
        val parameterVariance = 1.0
        val variance = 1.0

        // We're using paramater as a prior to estimate a normal with known variance
        val parameter = Normal(parameterMean, parameterVariance)

        val observations = List(6.1, 7.3, 5.8, 5.3, 6.4)
        for(obs <- observations) {
          Normal(parameter, variance).observe(obs)
        }

        // The MAP value of parameter is just the the posterior mean
        val meanEstimate = (parameterMean / parameterVariance + observations.sum / variance) /
          (1.0 / parameterVariance + observations.length / variance)

        val alg = ProbEvidenceMarginalMAP(10000, 1, ProposalScheme(parameter), Schedule.default(), parameter)
        alg.start()
        alg.mostLikelyValue(parameter) should equal(meanEstimate +- 0.01)
        alg.kill()
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
        val alg = ProbEvidenceMarginalMAP(2000, 200, a, b)
        alg.start()
        alg.mostLikelyValue(a) should equal(true)
        alg.mostLikelyValue(b) should equal(false)
        alg.kill()
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
        val alg = ProbEvidenceMarginalMAP(2000, 200, a, b)
        alg.start()
        alg.mostLikelyValue(a) should equal(false)
        alg.mostLikelyValue(b) should equal(true)
        alg.kill()
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

        val alg = ProbEvidenceMarginalMAP(2000, 50, c, d)
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

        val alg = ProbEvidenceMarginalMAP(2000, 50, c, d)
        alg.start()
        alg.mostLikelyValue(c) should equal(true)
        alg.mostLikelyValue(d) should equal(false)
        alg.kill()
      }
    }
  }
}
