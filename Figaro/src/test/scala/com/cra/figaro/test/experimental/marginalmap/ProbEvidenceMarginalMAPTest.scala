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
import com.cra.figaro.library.atomic.discrete.{Uniform, Util}
import com.cra.figaro.library.collection.Container
import com.cra.figaro.library.compound.If
import org.scalatest.{Matchers, WordSpec}

class ProbEvidenceMarginalMAPTest extends WordSpec with Matchers {
  // For testing, it's more efficient to use a linear schedule on simple models where we just want quick convergence,
  // rather than exploration of a large state space
  val linearSchedule = Schedule((temp, iter) => iter)

  def anytime(elems: Element[_]*) =
    ProbEvidenceMarginalMAP(0.05, 100, 100, ProposalScheme.default, linearSchedule, elems:_*)

  def oneTime(elems: Element[_]*) =
    ProbEvidenceMarginalMAP(2000, 0.05, 100, 100, ProposalScheme.default, linearSchedule, elems:_*)

  "Marginal MAP using probability of evidence" should {
    "increase temperature with additional iterations" in {
      Universe.createNew()
      val elem = Flip(0.6)

      val alg = ProbEvidenceMarginalMAP(elem)
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
      val alg1 = ProbEvidenceMarginalMAP(100, 0.05, 2, 100, ProposalScheme(elem), Schedule.default(2.0), elem)
      alg1.start()
      val temp1 = alg1.getTemperature
      alg1.kill()

      // k = 4.0
      val alg2 = ProbEvidenceMarginalMAP(100, 0.05, 2, 100, ProposalScheme(elem), Schedule.default(4.0), elem)
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

        val alg = ProbEvidenceMarginalMAP(0.05, 2, 1, ProposalScheme(parameter), linearSchedule, parameter)
        alg.start()
        Thread.sleep(2500)
        alg.stop()
        alg.mostLikelyValue(parameter) should equal(meanEstimate +- 0.05)
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

        val alg = anytime(c, d)
        alg.start()
        Thread.sleep(2500)
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

        val alg = anytime(c, d)
        alg.start()
        Thread.sleep(2500)
        alg.stop()
        alg.mostLikelyValue(c) should equal(true)
        alg.mostLikelyValue(d) should equal(false)
        alg.kill()
      }
    }

    "given evidence on MAP elements" should {
      "produce the right answer with a condition" in {
        Universe.createNew()
        val rolls = for { i <- 1 to 10 } yield Uniform(1,2,3,4)
        val c = Container(rolls: _*)
        val num4 = c.count(_ == 4)

        num4.addCondition(_ >= 5)

        // Since the pmf of a binomial distribution is strictly decreasing past the mode,
        // the most likely value should be the least possible value given the evidence
        val alg = anytime(num4)
        alg.start()
        Thread.sleep(10000)
        alg.stop()
        alg.mostLikelyValue(num4) should equal(5)
        alg.kill()
      }

      "produce the right answer with a constraint" in {
        Universe.createNew()
        val rolls = for { i <- 1 to 10 } yield Uniform(1,2,3,4)
        val c = Container(rolls: _*)
        val num4 = c.count(_ == 4)

        val constraint = (x: Int) => math.exp(- (x - 6) * (x - 6))
        num4.addConstraint(constraint)
        val max = (0 to 10).maxBy(x => Util.binomialDensity(10, 0.25, x) * constraint(x))
        max should equal(5)

        val alg = anytime(num4)
        alg.start()
        Thread.sleep(10000)
        alg.stop()
        alg.mostLikelyValue(num4) should equal(max)
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

        val alg = ProbEvidenceMarginalMAP(1000, 0.05, 2, 1, ProposalScheme(parameter), linearSchedule, parameter)
        alg.start()
        alg.mostLikelyValue(parameter) should equal(meanEstimate +- 0.05)
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

        val alg = oneTime(c, d)
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

        val alg = oneTime(c, d)
        alg.start()
        alg.mostLikelyValue(c) should equal(true)
        alg.mostLikelyValue(d) should equal(false)
        alg.kill()
      }
    }

    "given evidence on MAP elements" should {
      "produce the right answer with a condition" in {
        Universe.createNew()
        val rolls = for { i <- 1 to 10 } yield Uniform(1,2,3,4)
        val c = Container(rolls: _*)
        val num4 = c.count(_ == 4)

        num4.addCondition(_ >= 5)

        // Since the pmf of a binomial distribution is strictly decreasing past the mode,
        // the most likely value should be the least possible value given the evidence
        val alg = oneTime(num4)
        alg.start()
        alg.mostLikelyValue(num4) should equal(5)
        alg.kill()
      }

      "produce the right answer with a constraint" in {
        Universe.createNew()
        val rolls = for { i <- 1 to 10 } yield Uniform(1,2,3,4)
        val c = Container(rolls: _*)
        val num4 = c.count(_ == 4)

        val constraint = (x: Int) => math.exp(- (x - 6) * (x - 6))
        num4.addConstraint(constraint)
        val max = (0 to 10).maxBy(x => Util.binomialDensity(10, 0.25, x) * constraint(x))
        max should equal(5)

        val alg = oneTime(num4)
        alg.start()
        alg.mostLikelyValue(num4) should equal(max)
        alg.kill()
      }
    }
  }
}
