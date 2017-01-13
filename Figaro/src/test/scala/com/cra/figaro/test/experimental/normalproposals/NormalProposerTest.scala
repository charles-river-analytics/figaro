/*
 * NormalProposerTest.scala
 * Atomic continuous element tests.
 *
 * Created By:      William Kretschmer (kretsch@mit.edu)
 * Creation Date:   Aug 25, 2016
 *
 * Copyright 2016 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.test.experimental.normalproposals

import org.scalatest.Matchers
import org.scalatest.WordSpec
import scala.math.exp
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.experimental.normalproposals._
import com.cra.figaro.language._
import JSci.maths.statistics._
import com.cra.figaro.test.tags.NonDeterministic
import com.cra.figaro.ndtest._

// Largely copied from ContinuousTest, but applied only to NormalProposer elements using MH. The idea is that if we
// integrate the NormalProposer elements into the main library, then we can reuse the previous tests. Some additional
// tests were added for multiple Beta parameters, since Beta only conditionally uses Normal proposals.
class NormalProposerTest extends WordSpec with Matchers {

  val alpha: Double = 0.05

  def varStatistic(value: Double, target: Double, n: Int) = {
    val df = n - 1

    // df * sample value / target value is distributed as chisq with df degrees of freedom
    val chisq = df * value / target

    // (chisq - df) / sqrt(2 * df) is approximately N(0, 1) for high df
    val stat = (chisq - df) / math.sqrt(2 * df)

    stat
  }

  "An AtomicUniform" should {
    "compute the correct probability under Metropolis-Hastings" taggedAs NonDeterministic in {
      val ndtest = new NDTest {
        override def oneTest = {
          val target = 0.25
          Universe.createNew()
          val elem = Uniform(0.0, 2.0)
          val alg = MetropolisHastings(20000, ProposalScheme.default, elem)
          alg.start()
          val result = alg.probability(elem)(d => 0.7 <= d && d < 1.2)
          alg.stop()
          alg.kill()
          update(result, NDTest.TTEST, "AtomicUniformTestResults", target, alpha)
        }
      }

      ndtest.run(10)
    }
  }

  "An AtomicNormal" should {
    "compute the correct probability under Metropolis-Hastings" taggedAs NonDeterministic in {
      val ndtest = new NDTest {
        override def oneTest = {
          Universe.createNew()
          val elem = Normal(1.0, 2.0)
          val alg = MetropolisHastings(20000, ProposalScheme.default, elem)
          alg.start()
          val dist = new NormalDistribution(1.0, 2.0)
          val target = dist.cumulative(1.2) - dist.cumulative(0.7)
          val result = alg.probability(elem)(d => 0.7 <= d && d < 1.2)
          alg.stop()
          alg.kill()
          update(result - target, NDTest.TTEST, "AtomicNormalTestResults", 0.0, alpha)
        }
      }

      ndtest.run(10)
    }
  }
  
  "An AtomicExponential" should {
    "compute the correct probability under Metropolis-Hastings" taggedAs NonDeterministic in {
      val ndtest = new NDTest {
        override def oneTest = {
          Universe.createNew()
          val elem = Exponential(2.0)
          val alg = MetropolisHastings(20000, ProposalScheme.default, elem)
          alg.start()
          val dist = new ExponentialDistribution(2.0)
          val targetProb = dist.cumulative(1.2) - dist.cumulative(0.7)
          val result = alg.probability(elem)(d => 0.7 <= d && d < 1.2)
          alg.stop()
          alg.kill()
          update(result, NDTest.TTEST, "AtomicExponentialTestResults", targetProb, alpha)
        }
      }

      ndtest.run(10)
    }
  }

  "An AtomicGamma" when {
    "k > 1.0, theta = 1.0" should {
      "compute the correct value under Metropolis-Hastings" taggedAs NonDeterministic in {
        val ndtest = new NDTest {
          override def oneTest = {
            Universe.createNew()
            val k = 2.5
            val elem = Gamma(k)
            val alg = MetropolisHastings(20000, ProposalScheme.default, elem)
            alg.start()
            val dist = new GammaDistribution(k)
            val targetProb = dist.cumulative(1.2) - dist.cumulative(0.7)
            val result = alg.probability(elem)(d => 0.7 <= d && d < 1.2)
            alg.stop()
            alg.kill()
            update(result, NDTest.TTEST, "AtomicGammaTestResults", targetProb, alpha)
          }
        }

        ndtest.run(10)
      }
    }

    "k = 1.0, theta is not 1.0" should {
      "compute the correct probability under Metropolis-Hastings" taggedAs NonDeterministic in {
        val ndtest = new NDTest {
          override def oneTest = {
            Universe.createNew()
            val theta = 2.0
            val elem = Gamma(1.0, theta)
            val alg = MetropolisHastings(20000, ProposalScheme.default, elem)
            alg.start()
            // Using the fact that for Gamma(1,theta), the CDF is given by F(x) = 1 - exp(-x/theta)
            def cdf(x: Double) = 1 - exp(-x / theta)
            val targetProb = cdf(1.2) - cdf(0.7)
            val result = alg.probability(elem)(d => 0.7 <= d && d < 1.2)
            alg.stop()
            alg.kill()
            update(result, NDTest.TTEST, "AtomicGammaTestResults", targetProb, alpha)
          }
        }

        ndtest.run(10)
      }
    }

    "k = 1.0, theta = 1.0" should {
      "compute the correct probability under Metropolis-Hastings" taggedAs NonDeterministic in {
        val ndtest = new NDTest {
          override def oneTest = {
            Universe.createNew()
            val k = 1.0
            val elem = Gamma(k)
            val alg = MetropolisHastings(20000, ProposalScheme.default, elem)
            alg.start()
            val dist = new GammaDistribution(k)
            val targetProb = dist.cumulative(1.2) - dist.cumulative(0.7)
            val result = alg.probability(elem)(d => 0.7 <= d && d < 1.2)
            alg.stop()
            alg.kill()
            update(result, NDTest.TTEST, "AtomicGammaTestResults", targetProb, alpha)
          }
        }

        ndtest.run(10)
      }
    }

    "k < 1.0, theta = 1.0" should {
      "compute the correct probability under Metropolis-Hastings" taggedAs NonDeterministic in {
        val ndtest = new NDTest {
          override def oneTest = {
            Universe.createNew()
            val k = 0.6
            val elem = Gamma(k)
            val alg = MetropolisHastings(20000, ProposalScheme.default, elem)
            alg.start()
            val dist = new GammaDistribution(k)
            val targetProb = dist.cumulative(1.2) - dist.cumulative(0.7)
            val result = alg.probability(elem)(d => 0.7 <= d && d < 1.2)
            alg.stop()
            alg.kill()
            update(result, NDTest.TTEST, "AtomicGammaTestResults", targetProb, alpha)
          }
        }

        ndtest.run(10)
      }
    }
  }

  "An AtomicBeta" when {
    "a >= 1.0 and b >= 1.0" should {
      "compute the correct probability under Metropolis-Hastings" taggedAs NonDeterministic in {
        val ndtest = new NDTest {
          override def oneTest = {
            Universe.createNew()
            val a = 1.3
            val b = 2.7
            val elem = Beta(a, b)
            val alg = MetropolisHastings(20000, ProposalScheme.default, elem)
            alg.start()
            val dist = new BetaDistribution(a, b)
            val targetProb = dist.cumulative(0.3) - dist.cumulative(0.2)
            val result = alg.probability(elem)(d => 0.2 <= d && d < 0.3)
            alg.stop()
            alg.kill()
            update(result, NDTest.TTEST, "AtomicBetaTestResults", targetProb, alpha)
          }
        }

        ndtest.run(10)
      }
    }

    "a >= 1.0 and b < 1.0" should {
      "compute the correct probability under Metropolis-Hastings" taggedAs NonDeterministic in {
        val ndtest = new NDTest {
          override def oneTest = {
            Universe.createNew()
            val a = 1.2
            val b = 0.5
            val elem = Beta(a, b)
            val alg = MetropolisHastings(20000, ProposalScheme.default, elem)
            alg.start()
            val dist = new BetaDistribution(a, b)
            val targetProb = dist.cumulative(0.3) - dist.cumulative(0.2)
            val result = alg.probability(elem)(d => 0.2 <= d && d < 0.3)
            alg.stop()
            alg.kill()
            update(result, NDTest.TTEST, "AtomicBetaTestResults", targetProb, alpha)
          }
        }

        ndtest.run(10)
      }
    }

    "a < 1.0 and b >= 1.0" should {
      "compute the correct probability under Metropolis-Hastings" taggedAs NonDeterministic in {
        val ndtest = new NDTest {
          override def oneTest = {
            Universe.createNew()
            val a = 0.3
            val b = 1.0
            val elem = Beta(a, b)
            val alg = MetropolisHastings(20000, ProposalScheme.default, elem)
            alg.start()
            val dist = new BetaDistribution(a, b)
            val targetProb = dist.cumulative(0.3) - dist.cumulative(0.2)
            val result = alg.probability(elem)(d => 0.2 <= d && d < 0.3)
            alg.stop()
            alg.kill()
            update(result, NDTest.TTEST, "AtomicBetaTestResults", targetProb, alpha)
          }
        }

        ndtest.run(10)
      }
    }

    "a < 1.0 and b < 1.0" should {
      "compute the correct probability under Metropolis-Hastings" taggedAs NonDeterministic in {
        val ndtest = new NDTest {
          override def oneTest = {
            Universe.createNew()
            val a = 0.3
            val b = 0.6
            val elem = Beta(a, b)
            val alg = MetropolisHastings(20000, ProposalScheme.default, elem)
            alg.start()
            val dist = new BetaDistribution(a, b)
            val targetProb = dist.cumulative(0.3) - dist.cumulative(0.2)
            val result = alg.probability(elem)(d => 0.2 <= d && d < 0.3)
            alg.stop()
            alg.kill()
            update(result, NDTest.TTEST, "AtomicBetaTestResults", targetProb, alpha)
          }
        }

        ndtest.run(10)
      }
    }
  }
}
