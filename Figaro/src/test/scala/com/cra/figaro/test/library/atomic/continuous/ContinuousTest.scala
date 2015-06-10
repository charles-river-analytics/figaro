/*
 * ContinuousTest.scala  
 * Atomic continuous element tests.
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Jan 1, 2009
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.test.library.atomic.continuous

import org.scalatest.Matchers
import org.scalatest.WordSpec
import scala.math.{ exp, pow }
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.library.atomic.continuous._
import com.cra.figaro.language._
import JSci.maths.statistics._
import JSci.maths.SpecialMath.gamma
import org.apache.commons.math3.distribution.MultivariateNormalDistribution
import com.cra.figaro.test.tags.NonDeterministic
import com.cra.figaro.ndtest._

class ContinuousTest extends WordSpec with Matchers {

  val alpha: Double = 0.05

  def varStatistic(value: Double, target: Double, n: Int) = {
    val df = n - 1
    
    // df * sample value / target value is distributed as chisq with df degrees of freedom
    val chisq = df * value / target
    
    // (chisq - df) / sqrt(2 * df) is approximately N(0, 1) for high df
    val stat = (chisq - df) / math.sqrt(2 * df)
    
    stat
  }
  
  "A AtomicUniform" should {
    "have value within a range with probability equal to the fraction represented by the range" taggedAs (NonDeterministic) in {
      val ndtest = new NDTest {
        override def oneTest = {
          val target = 0.25
          Universe.createNew()
          val elem = Uniform(0.0, 2.0)
          val alg = Importance(20000, elem)
          alg.start()
          val result = alg.probability(elem, (d: Double) => 0.7 <= d && d < 1.2)
          alg.stop()
          alg.kill
          update(result, NDTest.TTEST, "AtomicUniformTestResults", target, alpha)
        }
      }

      ndtest.run(10)
    }

    "compute the correct probability under Metropolis-Hastings" taggedAs (NonDeterministic) in {
      val ndtest = new NDTest {
        override def oneTest = {
          val target = 0.25
          Universe.createNew()
          val elem = Uniform(0.0, 2.0)
          val alg = MetropolisHastings(20000, ProposalScheme.default, elem)
          alg.start()
          val result = alg.probability(elem, (d: Double) => 0.7 <= d && d < 1.2)
          alg.stop()
          alg.kill
          update(result, NDTest.TTEST, "AtomicUniformTestResults", target, alpha)
        }
      }

      ndtest.run(10)
    }

    "for an input within the interval have density equal to 1 divided by the size of the interval" in {
      Universe.createNew()
      val result = Uniform(0.0, 2.0).density(1.5)
      result should be(0.5 +- 0.00001)
    }

    "for an input outside the interval have density 0" in {
      Universe.createNew()
      val result = Uniform(0.0, 2.0).density(2.5)
      result should be(0.0 +- 0.00001)
    }

    "convert to the correct string" in {
      Universe.createNew()
      Uniform(0.0, 2.0).toString should equal("Uniform(0.0, 2.0)")
    }
  }

  "A CompoundUniform" should {
    "have value equal to the expectation over the parents of the uniform probability" taggedAs (NonDeterministic) in {
      val ndtest = new NDTest {
        override def oneTest = {
          val target = 0.1733
          Universe.createNew()
          val lower = Uniform(0.0, 1.0)
          val upper = Constant(2.0)
          val uniformComplex = Uniform(lower, upper)
          val alg = Importance(20000, uniformComplex)
          alg.start()
          // p(1.25 < x < 1.5 | lower = l) = 0.25 / (2-l)
          // Expectation of l = \int_{0}^{1} 1 / (2-l) dl = 0.25(-ln(2-1) + ln(2-0)) = 0.1733
          val result = alg.probability(uniformComplex, (d: Double) => 1.25 <= d && d < 1.5)
          alg.stop()
          alg.kill
          update(result, NDTest.TTEST, "CompoundUniformTestResults", target, alpha)
        }
      }

      ndtest.run(10)
    }

    "convert to the correct string" in {
      Universe.createNew()
      val lower = Uniform(0.0, 1.0)
      val upper = Constant(2.0)
      Uniform(lower, upper).toString should equal("Uniform(" + lower.toString + ", " + upper.toString + ")")
    }
  }

  "An AtomicNormal" should {
    "have value within a range with probability equal to the cumulative probability of the upper minus the lower" taggedAs (NonDeterministic) in {
      val ndtest = new NDTest {
        override def oneTest = {
          Universe.createNew()
          val elem = Normal(1.0, 2.0)
          val alg = Importance(20000, elem)
          alg.start()
          val dist = new NormalDistribution(1.0, 2.0)
          val target = dist.cumulative(1.2) - dist.cumulative(0.7)
          val result = alg.probability(elem, (d: Double) => 0.7 <= d && d < 1.2)
          alg.stop()
          alg.kill
          update(result - target, NDTest.TTEST, "AtomicNormalTestResults", 0.0, alpha)
        }
      }

      ndtest.run(10)
    }

    "compute the correct probability under Metropolis-Hastings" taggedAs (NonDeterministic) in {
      val ndtest = new NDTest {
        override def oneTest = {
          Universe.createNew()
          val elem = Normal(1.0, 2.0)
          val alg = MetropolisHastings(20000, ProposalScheme.default, elem)
          alg.start()
          val dist = new NormalDistribution(1.0, 2.0)
          val target = dist.cumulative(1.2) - dist.cumulative(0.7)
          val result = alg.probability(elem, (d: Double) => 0.7 <= d && d < 1.2)
          alg.stop()
          alg.kill
          update(result - target, NDTest.TTEST, "AtomicNormalTestResults", 0.0, alpha)
        }
      }

      ndtest.run(10)
    }

    "have the correct density" in {
      Universe.createNew()
      val elem = Normal(1.0, 2.0)
      val dist = new NormalDistribution(1.0, 2.0)
      val target = dist.probability(1.5)
      val result = elem.density(1.5)
      result should be(target +- 0.00001)
    }

    "convert to the correct string" in {
      Universe.createNew()
      Normal(1.0, 2.0).toString should equal("Normal(1.0, 2.0)")
    }
  }

  "A CompoundNormalMean" should {
    "have value within a range with probability equal to the expectation over the mean of the" +
      "cumulative probability of the upper minus the lower" taggedAs (NonDeterministic) in {
        val ndtest = new NDTest {
          override def oneTest = {
            Universe.createNew()
            val elem = Normal(Select(0.5 -> 0.5, 0.5 -> 1.0), 2.0)
            val alg = Importance(20000, elem)
            alg.start()
            val dist1 = new NormalDistribution(0.5, 2.0)
            val dist2 = new NormalDistribution(1.0, 2.0)
            def getProb(dist: ProbabilityDistribution) = dist.cumulative(1.2) - dist.cumulative(0.7)
            val target = 0.5 * getProb(dist1) + 0.5 * getProb(dist2)
            val result = alg.probability(elem, (d: Double) => 0.7 <= d && d < 1.2)
            alg.stop()
            alg.kill
            update(result - target, NDTest.TTEST, "CompoundNormalMeanResultsDiff", 0.0, alpha)
          }
        }

        ndtest.run(10)
      }

    "convert to the correct string" in {
      Universe.createNew()
      val sel = Select(0.5 -> 0.5, 0.5 -> 1.0)
      Normal(sel, 2.0).toString should equal("Normal(" + sel + ", 2.0)")
    }
  }

  "A CompoundNormalVariance" should {
    //Need to work this out on paper.
    "have value within a range with probability equal to the expectation over the mean and variance of the" +
      "cumulative probability of the upper minus the lower" in {
        val ndtest = new NDTest {
          override def oneTest = {
            Universe.createNew()
            val elem = Normal(2.0, Select(0.5 -> 2.0, 0.5 -> 3.0))
            val alg = Importance(20000, elem)
            alg.start()
            val dist1 = new NormalDistribution(2.0, 2.0)
            val dist2 = new NormalDistribution(2.0, 3.0)
            def getProb(dist: ProbabilityDistribution) = dist.cumulative(1.2) - dist.cumulative(0.7)
            val targetProb = 0.5 * getProb(dist1) + 0.5 * getProb(dist2)
            val result = alg.probability(elem, (d: Double) => 0.7 <= d && d < 1.2)
            alg.stop()
            alg.kill
            update(result, NDTest.TTEST, "CompoundNormalMeanTestResults", targetProb, alpha)
          }
        }

        ndtest.run(10)
      }

    "convert to the correct string" in {
      Universe.createNew()
      val g = Gamma(1.0, 5.0)
      Normal(2.0, Gamma(1.0, 5.0)).toString should equal("Normal(2.0, Gamma(1.0, 5.0))")
    }
  }

  "A CompoundNormal" should {
    "have value within a range with probability equal to the expectation over the mean and variance of the" +
      "cumulative probability of the upper minus the lower" taggedAs (NonDeterministic) in {
        val ndtest = new NDTest {
          override def oneTest = {
            Universe.createNew()
            val elem = Normal(Select(0.5 -> 0.5, 0.5 -> 1.0), Select(0.5 -> 2.0, 0.5 -> 3.0))
            val alg = Importance(20000, elem)
            alg.start()
            val dist1 = new NormalDistribution(0.5, 2.0)
            val dist2 = new NormalDistribution(1.0, 2.0)
            val dist3 = new NormalDistribution(0.5, 3.0)
            val dist4 = new NormalDistribution(1.0, 3.0)
            def getProb(dist: ProbabilityDistribution) = dist.cumulative(1.2) - dist.cumulative(0.7)
            val target = 0.25 * getProb(dist1) + 0.25 * getProb(dist2) + 0.25 * getProb(dist3) + 0.25 * getProb(dist4)
            val result = alg.probability(elem, (d: Double) => 0.7 <= d && d < 1.2)
            alg.stop()
            alg.kill
            update(result - target, NDTest.TTEST, "CompoundNormalTestResults", 0.0, alpha)
          }
        }

        ndtest.run(10)
      }

    "convert to the correct string" in {
      Universe.createNew()
      val sel1 = Select(0.5 -> 0.5, 0.5 -> 1.0)
      val sel2 = Select(0.5 -> 2.0, 0.5 -> 3.0)
      Normal(sel1, sel2).toString should equal("Normal(" + sel1 + ", " + sel2 + ")")
    }

    /*
    "produce the right probability when conditioned under Metropolis-Hastings" taggedAs (NonDeterministic) in {
      val ndtest = new NDTest {
        override def oneTest = {
          val sampleUniverse = Universe.createNew()
          val nSamples = Normal(2.5, 2.0)("", sampleUniverse)
          val samples = for (i <- 1 to 100)
            yield nSamples.generateValue(nSamples.generateRandomness())

//          val samplesMean = samples.sum / samples.size
//          val samplesVariance = samples.map(s => (s - samplesMean) * (s - samplesMean)).sum / (samples.size - 1)
          
          val universe = Universe.createNew()
          val mean = Uniform(-5, 5)("mean", universe)
          val variance = Uniform(0, 5)("variance", universe)
          for (sample <- samples) {
            val normal = Normal(mean, variance)
            normal.observe(sample)
          }
          val alg = MetropolisHastings(100000, ProposalScheme.default, mean, variance)
          alg.start()
          val result1 = alg.mean(mean)
          val result2 = alg.mean(variance)
          alg.stop()
          alg.kill
          update(result1, NDTest.TTEST, "CompoundNormalTestResultsMean", 2.5, alpha)
          
          val stat = varStatistic(result2, 2.0, 100)
          update(stat, NDTest.TTEST, "CompoundNormalTestResultsVar", 0.0, alpha)
        }
      }

      ndtest.run(10)
    }
    * 
    */

    "produce the right probability when conditioned under Importance Sampling" taggedAs (NonDeterministic) in {
      val ndtest = new NDTest {
        override def oneTest = {
          val sampleUniverse = Universe.createNew()
          val nSamples = Normal(2.5, 2.0)("", sampleUniverse)
          val samples = for (i <- 1 to 200)
            yield nSamples.generateValue(nSamples.generateRandomness())

          val universe = Universe.createNew()
          val mean = Uniform(-5, 5)("mean", universe)
          val variance = Uniform(0, 5)("variance", universe)
          for (sample <- samples) {
            val normal = Normal(mean, variance)
            normal.observe(sample)
          }

          val alg = Importance(2000, mean, variance)
          alg.start()
          val result1 = alg.mean(mean)
          val result2 = alg.mean(variance)
          alg.stop()
          alg.kill
          update(result1, NDTest.TTEST, "CompoundNormalTestResultsMean", 2.5, alpha)
          
          val stat = varStatistic(result2, 2.0, 100)
          update(stat, NDTest.TTEST, "CompoundNormalTestResultsVar", 0.0, alpha)
        }
      }

      ndtest.run(10)
    }

  }

  "A CompoundMultivariateNormal" should {
    val means = List(1.0, 2.0)
    val covariances = List(List(.25, .15), List(.15, .25))

    //    "have value within a range with probability equal to the cumulative probability of the upper minus the lower" in {
    //      Universe.createNew()
    //      val elem = Normal(1.0, 2.0)
    //      val alg = Importance(20000, elem)
    //      alg.start()
    //      val dist = new NormalDistribution(1.0, 2.0)
    //      val targetProb = dist.cumulative(1.2) - dist.cumulative(0.7)
    //      alg.probability(elem, (d: Double) => 0.7 <= d && d < 1.2) should be(targetProb +- 0.01)
    //    }

    //    "compute the correct probability under Metropolis-Hastings" in {
    //      Universe.createNew()
    //      val elem = Normal(1.0, 2.0)
    //      val alg = MetropolisHastings(20000, ProposalScheme.default, elem)
    //      alg.start()
    //      val dist = new NormalDistribution(1.0, 2.0)
    //      val targetProb = dist.cumulative(1.2) - dist.cumulative(0.7)
    //      alg.probability(elem, (d: Double) => 0.7 <= d && d < 1.2) should be(targetProb +- 0.01)
    //    }

    "have the correct density" in {
      Universe.createNew()
      val elem = MultivariateNormal(means, covariances)
      val dist = new MultivariateNormalDistribution(means.toArray, covariances.map((l: List[Double]) => l.toArray).toArray)
      val result = elem.density(List(1.5, 2.5))
      result should be(dist.density(Array(1.5, 2.5)) +- 0.00001)
    }

    "produce samples with the correct means and covariances" taggedAs (NonDeterministic) in {
      val ndtest = new NDTest {
        override def oneTest = {
          Universe.createNew()
          val elem = MultivariateNormal(means, covariances)

          var n = 0
          var sumx1 = 0.0
          var sumx2 = 0.0
          var ss1 = 0.0
          var ss2 = 0.0
          var sc12 = 0.0

          for (i <- 0 to 10000) {
            val rand = elem.generateRandomness()
            val values = elem.generateValue(rand)

            val x1 = values(0)
            val x2 = values(1)

            n += 1
            sumx1 += x1
            sumx2 += x2
            ss1 += x1 * x1
            ss2 += x2 * x2
            sc12 += x1 * x2
          }

          val mean1 = sumx1 / n
          val mean2 = sumx2 / n
          val var1 = (ss1 - (sumx1 * sumx1 / n)) / (n - 1)
          val var2 = (ss2 - (sumx2 * sumx2 / n)) / (n - 1)
          val cov = (sc12 - (sumx1 * sumx2 / n)) / (n - 1)

          update(mean1, NDTest.TTEST, "CompoundMultivariateNormalTestResultsMean1", means(0), alpha)
          update(mean2, NDTest.TTEST, "CompoundMultivariateNormalTestResultsMean2", means(1), alpha)
          update(var1, NDTest.TTEST, "CompoundMultivariateNormalTestResultsVar1", covariances(0)(0), alpha)
          update(var2, NDTest.TTEST, "CompoundMultivariateNormalTestResultsVar2", covariances(1)(1), alpha)
          update(cov, NDTest.TTEST, "CompoundMultivariateNormalTestResultsCov", covariances(0)(1), alpha)
        }
      }

      ndtest.run(10)
    }

    "convert to the correct string" in {
      Universe.createNew()
      MultivariateNormal(means, covariances).toString should equal("MultivariateNormal(" + means + ",\n" + covariances + ")")
    }
  }

  "An AtomicMultivariateNormal" should {
    val means = List(1.0, 2.0)
    val covariances = List(List(.25, .15), List(.15, .25))

    //    "have value within a range with probability equal to the cumulative probability of the upper minus the lower" in {
    //      Universe.createNew()
    //      val elem = Normal(1.0, 2.0)
    //      val alg = Importance(20000, elem)
    //      alg.start()
    //      val dist = new NormalDistribution(1.0, 2.0)
    //      val targetProb = dist.cumulative(1.2) - dist.cumulative(0.7)
    //      alg.probability(elem, (d: Double) => 0.7 <= d && d < 1.2) should be(targetProb +- 0.01)
    //    }

    //    "compute the correct probability under Metropolis-Hastings" in {
    //      Universe.createNew()
    //      val elem = Normal(1.0, 2.0)
    //      val alg = MetropolisHastings(20000, ProposalScheme.default, elem)
    //      alg.start()
    //      val dist = new NormalDistribution(1.0, 2.0)
    //      val targetProb = dist.cumulative(1.2) - dist.cumulative(0.7)
    //      alg.probability(elem, (d: Double) => 0.7 <= d && d < 1.2) should be(targetProb +- 0.01)
    //    }

    "have the correct density" in {
      Universe.createNew()
      val elem = MultivariateNormal(means, covariances)
      val dist = new MultivariateNormalDistribution(means.toArray, covariances.map((l: List[Double]) => l.toArray).toArray)
      val target = dist.density(Array(1.5, 2.5))
      val result = elem.density(List(1.5, 2.5))
      result should be(target +- 0.00001)

    }

    "produce samples with the correct means and covariances" taggedAs (NonDeterministic) in {
      val ndtest = new NDTest {
        override def oneTest = {
          Universe.createNew()
          val elem = MultivariateNormal(means, covariances)

          var n = 0
          var sumx1 = 0.0
          var sumx2 = 0.0
          var ss1 = 0.0
          var ss2 = 0.0
          var sc12 = 0.0

          for (i <- 0 to 10000) {
            val rand = elem.generateRandomness()
            val values = elem.generateValue(rand)

            val x1 = values(0)
            val x2 = values(1)

            n += 1
            sumx1 += x1
            sumx2 += x2
            ss1 += x1 * x1
            ss2 += x2 * x2
            sc12 += x1 * x2
          }

          val mean1 = sumx1 / n
          val mean2 = sumx2 / n
          val var1 = (ss1 - (sumx1 * sumx1 / n)) / (n - 1)
          val var2 = (ss2 - (sumx2 * sumx2 / n)) / (n - 1)
          val cov = (sc12 - (sumx1 * sumx2 / n)) / (n - 1)

          update(mean1, NDTest.TTEST, "AtomicMultivariateNormalTestResultsMean1", means(0), alpha)
          update(mean2, NDTest.TTEST, "AtomicMultivariateNormalTestResultsMean2", means(1), alpha)
          update(var1, NDTest.TTEST, "AtomicMultivariateNormalTestResultsVar1", covariances(0)(0), alpha)
          update(var2, NDTest.TTEST, "AtomicMultivariateNormalTestResultsVar2", covariances(1)(1), alpha)
          update(cov, NDTest.TTEST, "AtomicMultivariateNormalTestResultsCov", covariances(0)(1), alpha)
        }
      }

      ndtest.run(10)
    }

    "convert to the correct string" in {
      Universe.createNew()
      MultivariateNormal(means, covariances).toString should equal("MultivariateNormal(" + means + ",\n" + covariances + ")")
    }
  }

  "An AtomicExponential" should {
    "have value within a range with probability equal to the cumulative probability of the upper minus the lower" taggedAs (NonDeterministic) in {
      val ndtest = new NDTest {
        override def oneTest = {
          Universe.createNew()
          val elem = Exponential(2.0)
          val alg = Importance(20000, elem)
          alg.start()
          val dist = new ExponentialDistribution(2.0)
          val targetProb = dist.cumulative(1.2) - dist.cumulative(0.7)
          val result = alg.probability(elem, (d: Double) => 0.7 <= d && d < 1.2)
          alg.stop()
          alg.kill
          update(result, NDTest.TTEST, "AtomicExponentialTestResults", targetProb, alpha)
        }
      }

      ndtest.run(10)
    }

    "compute the correct probability under Metropolis-Hastings" taggedAs (NonDeterministic) in {
      val ndtest = new NDTest {
        override def oneTest = {
          Universe.createNew()
          val elem = Exponential(2.0)
          val alg = MetropolisHastings(20000, ProposalScheme.default, elem)
          alg.start()
          val dist = new ExponentialDistribution(2.0)
          val targetProb = dist.cumulative(1.2) - dist.cumulative(0.7)
          val result = alg.probability(elem, (d: Double) => 0.7 <= d && d < 1.2)
          alg.stop()
          alg.kill
          update(result, NDTest.TTEST, "AtomicExponentialTestResults", targetProb, alpha)
        }
      }

      ndtest.run(10)
    }

    "have the correct density" in {
      Universe.createNew()
      val elem = Exponential(2.0)
      val dist = new ExponentialDistribution(2.0)
      val result = elem.density(1.5)
      result should be(dist.probability(1.5) +- 0.00001)
    }

    "convert to the correct string" in {
      Universe.createNew()
      Exponential(2.0).toString should equal("Exponential(2.0)")
    }

  }

  "An CompoundExponential" should {
    "have value within a range with probability equal to the expectation over the mean of the" +
      "cumulative probability of the upper minus the lower" taggedAs (NonDeterministic) in {
        val ndtest = new NDTest {
          override def oneTest = {
            Universe.createNew()
            val elem = Exponential(Select(0.5 -> 1.0, 0.5 -> 2.0))
            val alg = Importance(20000, elem)
            alg.start()
            val dist1 = new ExponentialDistribution(1.0)
            val dist2 = new ExponentialDistribution(2.0)
            def getProb(dist: ProbabilityDistribution) = dist.cumulative(1.2) - dist.cumulative(0.7)
            val targetProb = 0.5 * getProb(dist1) + 0.5 * getProb(dist2)
            val result = alg.probability(elem, (d: Double) => 0.7 <= d && d < 1.2)
            alg.stop()
            alg.kill
            update(result, NDTest.TTEST, "CompoundExponentialTestResults", targetProb, alpha)
          }
        }

        ndtest.run(10)
      }

    "convert to the correct string" in {
      Universe.createNew()
      val sel = Select(0.5 -> 1.0, 0.5 -> 2.0)
      Exponential(sel).toString should equal("Exponential(" + sel + ")")
    }

    /*
    "produce the right probability when conditioned under Metropolis-Hastings" taggedAs (NonDeterministic) in {
      val ndtest = new NDTest {
        override def oneTest = {
          val sampleUniverse = Universe.createNew()
          val nSamples = Exponential(2)("", sampleUniverse)
          val samples = for (i <- 1 to 100)
            yield nSamples.generateValue(nSamples.generateRandomness())

          val universe = Universe.createNew()
          val lambda = Uniform(0, 10)("lambda", universe)
          for (sample <- samples) {
            val exponential = Exponential(lambda)
            exponential.observe(sample)
          }
          val alg = MetropolisHastings(200000, ProposalScheme.default, lambda)
          alg.start()
          val target = 2.0
          val result = alg.mean(lambda)
          alg.stop()
          alg.kill
          update(result, NDTest.TTEST, "CompoundExponentialTestResults", target, alpha)
        }
      }

      ndtest.run(10)
    }
    * 
    */

    "produce the right probability when conditioned under Importance Sampling" taggedAs (NonDeterministic) in {
      val ndtest = new NDTest {
        override def oneTest = {
          val sampleUniverse = Universe.createNew()
          val nSamples = Exponential(2)("", sampleUniverse)
          val samples = for (i <- 1 to 200)
            yield nSamples.generateValue(nSamples.generateRandomness())

          val universe = Universe.createNew()
          val lambda = Uniform(0, 10)("lambda", universe)
          for (sample <- samples) {
            val exponential = Exponential(lambda)
            exponential.observe(sample)
          }
          val alg = Importance(2000, lambda)
          alg.start()
          val target = 2.0
          val result = alg.mean(lambda)
          alg.stop()
          alg.kill
          update(result, NDTest.TTEST, "CompoundExponentialTestResults", target, alpha)
        }
      }

      ndtest.run(10)
    }
  }

  "A AtomicGamma" when {
    "k > 1.0, theta = 1.0" should {
      "have value within a range with probability equal to the cumulative probability of the upper minus the lower" taggedAs (NonDeterministic) in {
        val ndtest = new NDTest {
          override def oneTest = {
            Universe.createNew()
            val k = 2.5
            val elem = Gamma(k)
            val alg = Importance(20000, elem)
            alg.start()
            val dist = new GammaDistribution(k)
            val targetProb = dist.cumulative(1.2) - dist.cumulative(0.7)
            val result = alg.probability(elem, (d: Double) => 0.7 <= d && d < 1.2)
            alg.stop()
            alg.kill
            update(result, NDTest.TTEST, "AtomicGammaTestResults", targetProb, alpha)
          }
        }

        ndtest.run(10)
      }

      "compute the correct value under Metropolis-Hastings" taggedAs (NonDeterministic) in {
        val ndtest = new NDTest {
          override def oneTest = {
            Universe.createNew()
            val k = 2.5
            val elem = Gamma(k)
            val alg = MetropolisHastings(20000, ProposalScheme.default, elem)
            alg.start()
            val dist = new GammaDistribution(k)
            val targetProb = dist.cumulative(1.2) - dist.cumulative(0.7)
            val result = alg.probability(elem, (d: Double) => 0.7 <= d && d < 1.2)
            alg.stop()
            alg.kill
            update(result, NDTest.TTEST, "AtomicGammaTestResults", targetProb, alpha)
          }
        }

        ndtest.run(10)
      }

      "have the correct density" in {
        Universe.createNew()
        val elem = Gamma(2.5)
        val dist = new GammaDistribution(2.5)
        val result = elem.density(1.5)
        result should be(dist.probability(1.5) +- 0.00001)
      }

      "convert to the correct string" in {
        Universe.createNew()
        Gamma(2.5).toString should equal("Gamma(2.5)")
      }
    }

    "k = 1.0, theta is not 1.0" should {
      "have value within a range with probability equal to the cumulative probability of the upper minus the lower" taggedAs (NonDeterministic) in {
        val ndtest = new NDTest {
          override def oneTest = {
            Universe.createNew()
            val theta = 2.0
            val elem = Gamma(1.0, theta)
            val alg = Importance(20000, elem)
            alg.start()
            // Using the fact that for Gamma(1,theta), the CDF is given by F(x) = 1 - exp(-x/theta)
            def cdf(x: Double) = 1 - exp(-x / theta)
            val targetProb = cdf(1.2) - cdf(0.7)
            val result = alg.probability(elem, (d: Double) => 0.7 <= d && d < 1.2)
            alg.stop()
            alg.kill
            update(result, NDTest.TTEST, "AtomicGammaTestResults", targetProb, alpha)
          }
        }

        ndtest.run(10)
      }

      "compute the correct probability under Metropolis-Hastings" taggedAs (NonDeterministic) in {
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
            val result = alg.probability(elem, (d: Double) => 0.7 <= d && d < 1.2)
            alg.stop()
            alg.kill
            update(result, NDTest.TTEST, "AtomicGammaTestResults", targetProb, alpha)
          }
        }

        ndtest.run(10)
      }

      "have the correct density" in {
        Universe.createNew()
        val theta = 2.0
        val elem = Gamma(1, theta)
        // Using the fact that for Gamme(1,theta), the PDF is given by p(x) = exp(-x/theta)/theta
        val prob = exp(-1.5 / theta) / theta
        val result = elem.density(1.5)
        result should be(prob +- 0.00001)
      }

      "convert to the correct string" in {
        Universe.createNew()
        Gamma(2.5, 2.0).toString should equal("Gamma(2.5, 2.0)")
      }
    }

    "k = 1.0, theta = 1.0" should {
      "have value within a range with probability equal to the cumulative probability of the upper minus the lower" taggedAs (NonDeterministic) in {
        val ndtest = new NDTest {
          override def oneTest = {
            Universe.createNew()
            val k = 1.0
            val elem = Gamma(k)
            val alg = Importance(20000, elem)
            alg.start()
            val dist = new GammaDistribution(k)
            val targetProb = dist.cumulative(1.2) - dist.cumulative(0.7)
            val result = alg.probability(elem, (d: Double) => 0.7 <= d && d < 1.2)
            alg.stop()
            alg.kill
            update(result, NDTest.TTEST, "AtomicGammaTestResults", targetProb, alpha)
          }
        }

        ndtest.run(10)
      }

      "compute the correct probability under Metropolis-Hastings" taggedAs (NonDeterministic) in {
        val ndtest = new NDTest {
          override def oneTest = {
            Universe.createNew()
            val k = 1.0
            val elem = Gamma(k)
            val alg = MetropolisHastings(20000, ProposalScheme.default, elem)
            alg.start()
            val dist = new GammaDistribution(k)
            val targetProb = dist.cumulative(1.2) - dist.cumulative(0.7)
            val result = alg.probability(elem, (d: Double) => 0.7 <= d && d < 1.2)
            alg.stop()
            alg.kill
            update(result, NDTest.TTEST, "AtomicGammaTestResults", targetProb, alpha)
          }
        }

        ndtest.run(10)
      }
    }

    "k < 1.0, theta = 1.0" should {
      "have value within a range with probability equal to the cumulative probability of the upper minus the lower" taggedAs (NonDeterministic) in {
        val ndtest = new NDTest {
          override def oneTest = {
            Universe.createNew()
            val k = 0.6
            val elem = Gamma(k)
            val alg = Importance(20000, elem)
            alg.start()
            val dist = new GammaDistribution(k)
            val targetProb = dist.cumulative(1.2) - dist.cumulative(0.7)
            val result = alg.probability(elem, (d: Double) => 0.7 <= d && d < 1.2)
            alg.stop()
            alg.kill
            update(result, NDTest.TTEST, "AtomicGammaTestResults", targetProb, alpha)
          }
        }

        ndtest.run(10)
      }

      "compute the correct probability under Metropolis-Hastings" taggedAs (NonDeterministic) in {
        val ndtest = new NDTest {
          override def oneTest = {
            Universe.createNew()
            val k = 0.6
            val elem = Gamma(k)
            val alg = MetropolisHastings(20000, ProposalScheme.default, elem)
            alg.start()
            val dist = new GammaDistribution(k)
            val targetProb = dist.cumulative(1.2) - dist.cumulative(0.7)
            val result = alg.probability(elem, (d: Double) => 0.7 <= d && d < 1.2)
            alg.stop()
            alg.kill
            update(result, NDTest.TTEST, "AtomicGammaTestResults", targetProb, alpha)
          }
        }

        ndtest.run(10)
      }
    }
  }

  "A CompoundGammaK" should {
    "have value within a range with probability equal to the expectation over k of the" +
      "cumulative probability of the upper minus the lower" taggedAs (NonDeterministic) in {
        val ndtest = new NDTest {
          override def oneTest = {
            Universe.createNew()
            val elem = Gamma(Select(0.5 -> 2.0, 0.5 -> 3.0))
            val alg = Importance(20000, elem)
            alg.start()
            val dist1 = new GammaDistribution(2.0)
            val dist2 = new GammaDistribution(3.0)
            def getProb(dist: ProbabilityDistribution) = dist.cumulative(1.2) - dist.cumulative(0.7)
            val targetProb = 0.5 * getProb(dist1) + 0.5 * getProb(dist2)
            val result = alg.probability(elem, (d: Double) => 0.7 <= d && d < 1.2)
            alg.stop()
            alg.kill
            update(result, NDTest.TTEST, "CompoundGammaKTestResults", targetProb, alpha)
          }
        }

        ndtest.run(10)
      }

    "convert to the correct string" in {
      Universe.createNew()
      val sel = Select(0.5 -> 1.0, 0.5 -> 2.0)
      Gamma(sel).toString should equal("Gamma(" + sel + ")")
    }
  }

  "A CompoundGamma" should {
    "have value within a range with probability equal to the expectation over k and theta of the" +
      "cumulative probability of the upper minus the lower" taggedAs (NonDeterministic) in {
        val ndtest = new NDTest {
          override def oneTest = {
            Universe.createNew()
            val elem = Gamma(Select(0.5 -> 0.5, 0.5 -> 1.0), Constant(1.0))
            val alg = Importance(20000, elem)
            alg.start()
            val dist1 = new GammaDistribution(0.5)
            val dist2 = new GammaDistribution(1.0)
            def getProb(dist: ProbabilityDistribution) = dist.cumulative(1.2) - dist.cumulative(0.7)
            val targetProb = 0.5 * getProb(dist1) + 0.5 * getProb(dist2)
            val result = alg.probability(elem, (d: Double) => 0.7 <= d && d < 1.2)
            alg.stop()
            alg.kill
            update(result, NDTest.TTEST, "CompoundGammaTestResults", targetProb, alpha)
          }
        }

        ndtest.run(10)
      }

    "convert to the correct string" in {
      Universe.createNew()
      val sel1 = Select(0.5 -> 0.5, 0.5 -> 1.0)
      val sel2 = Select(0.5 -> 2.0, 0.5 -> 3.0)
      Gamma(sel1, sel2).toString should equal("Gamma(" + sel1 + ", " + sel2 + ")")
    }

    /*
    "produce the right probability when conditioned under Metropolis-Hastings" taggedAs (NonDeterministic) in {
      val ndtest = new NDTest {
        override def oneTest = {
          val sampleUniverse = Universe.createNew()
          val nSamples = Gamma(2, 2)("", sampleUniverse)
          val samples = for (i <- 1 to 100)
            yield nSamples.generateValue(nSamples.generateRandomness())

          val universe = Universe.createNew()
          val k = Uniform(0, 10)("k", universe)
          val theta = Uniform(0, 10)("theta", universe)
          for (sample <- samples) {
            val gamma = Gamma(k, theta)
            gamma.observe(sample)
          }
          val alg = MetropolisHastings(200000, ProposalScheme.default, k, theta)
          alg.start()
          val resultK = alg.mean(k)
          val resultTheta = alg.mean(theta)
          alg.stop()
          alg.kill
          update(resultK, NDTest.TTEST, "CompoundGammaTestResultsK", 2.0, alpha)
          update(resultTheta, NDTest.TTEST, "CompoundGammaTestResultsTheta", 2.0, alpha)
        }
      }

      ndtest.run(10)
    }
    * 
    */

    "produce the right probability when conditioned under Importance Sampling" taggedAs (NonDeterministic) in {
      val ndtest = new NDTest {
        override def oneTest = {
          val sampleUniverse = Universe.createNew()
          val nSamples = Gamma(2, 2)("", sampleUniverse)

          val samples = for (i <- 1 to 200)
            yield nSamples.generateValue(nSamples.generateRandomness())

          val universe = Universe.createNew()
          val k = Uniform(0, 10)("k", universe)
          val theta = Uniform(0, 10)("theta", universe)
          for (sample <- samples) {
            val gamma = Gamma(k, theta)
            gamma.observe(sample)
          }

          val alg = Importance(2000, k, theta)
          alg.start()
          val resultK = alg.mean(k)
          val resultTheta = alg.mean(theta)
          alg.stop()
          alg.kill
          update(resultK, NDTest.TTEST, "CompoundGammaTestResultsK", 2.0, alpha)
          update(resultTheta, NDTest.TTEST, "CompoundGammaTestResultsTheta", 2.0, alpha)
        }
      }

      ndtest.run(10)
    }
  }

  "A AtomicBeta" should {
    "have value within a range with probability equal to the cumulative probability of the upper minus the lower" taggedAs (NonDeterministic) in {
      val ndtest = new NDTest {
        override def oneTest = {
          Universe.createNew()
          val a = 1.2
          val b = 0.5
          val elem = Beta(a, b)
          val alg = Importance(20000, elem)
          alg.start()
          val dist = new BetaDistribution(a, b)
          val targetProb = dist.cumulative(0.3) - dist.cumulative(0.2)
          val result = alg.probability(elem, (d: Double) => 0.2 <= d && d < 0.3)
          alg.stop()
          alg.kill
          update(result, NDTest.TTEST, "AtomicBetaTestResults", targetProb, alpha)
        }
      }

      ndtest.run(10)
    }

    "compute the correct probability under Metropolis-Hastings" taggedAs (NonDeterministic) in {
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
          val result = alg.probability(elem, (d: Double) => 0.2 <= d && d < 0.3)
          alg.stop()
          alg.kill
          update(result, NDTest.TTEST, "AtomicBetaTestResults", targetProb, alpha)
        }
      }

      ndtest.run(10)
    }

    "have the correct density" in {
      Universe.createNew()
      val a = 1.2
      val b = 0.5
      val elem = Beta(a, b)
      val dist = new BetaDistribution(a, b)
      val result = elem.density(0.3)
      result should be(dist.probability(0.3) +- 0.00001)
    }

    "convert to the correct string" in {
      Universe.createNew()
      Beta(2.5, 0.7).toString should equal("Beta(2.5, 0.7)")
    }
  }

  "A CompoundBeta" should {
    "have value within a range with probability equal to the expectation over a and b of the" +
      "cumulative probability of the upper minus the lower" taggedAs (NonDeterministic) in {
        val ndtest = new NDTest {
          override def oneTest = {
            Universe.createNew()
            val elem = Beta(Select(0.5 -> 0.5, 0.5 -> 1.0), Select(0.5 -> 2.0, 0.5 -> 3.0))
            val alg = Importance(20000, elem)
            alg.start()
            val dist1 = new BetaDistribution(0.5, 2.0)
            val dist2 = new BetaDistribution(1.0, 2.0)
            val dist3 = new BetaDistribution(0.5, 3.0)
            val dist4 = new BetaDistribution(1.0, 3.0)
            def getProb(dist: ProbabilityDistribution) = dist.cumulative(0.3) - dist.cumulative(0.2)
            val targetProb = 0.25 * getProb(dist1) + 0.25 * getProb(dist2) + 0.25 * getProb(dist3) + 0.25 * getProb(dist4)
            val result = alg.probability(elem, (d: Double) => 0.2 <= d && d < 0.3)
            alg.stop()
            alg.kill
            update(result, NDTest.TTEST, "CompoundBetaTestResults", targetProb, alpha)
          }
        }

        ndtest.run(10)
      }

    "convert to the correct string" in {
      Universe.createNew()
      val sel1 = Select(0.5 -> 0.5, 0.5 -> 1.0)
      val sel2 = Select(0.5 -> 2.0, 0.5 -> 3.0)
      Beta(sel1, sel2).toString should equal("Beta(" + sel1 + ", " + sel2 + ")")
    }

    /*
    "produce the right probability when conditioned under Metropolis-Hastings" taggedAs (NonDeterministic) in {
      val ndtest = new NDTest {
        override def oneTest = {
          val sampleUniverse = Universe.createNew()
          val nSamples = Beta(2, 5)("", sampleUniverse)
          val samples = for (i <- 1 to 100)
            yield nSamples.generateValue(nSamples.generateRandomness())

          val universe = Universe.createNew()
          val a = Uniform(0, 10)("a", universe)
          val b = Uniform(0, 10)("b", universe)
          for (sample <- samples) {
            val beta = Beta(a, b)
            beta.observe(sample)
          }
          val alg = MetropolisHastings(200000, ProposalScheme.default, a, b)
          alg.start()
          val resultA = alg.mean(a)
          val resultB = alg.mean(b)
          alg.stop()
          alg.kill
          update(resultA, NDTest.TTEST, "CompoundBetaTestResultsA", 2.0, alpha)
          update(resultB, NDTest.TTEST, "CompoundBetaTestResultsB", 5.0, alpha)
        }
      }

      ndtest.run(10)
    }
    * 
    */

    "produce the right probability when conditioned under Importance Sampling" taggedAs (NonDeterministic) in {
      val ndtest = new NDTest {
        override def oneTest = {
          val sampleUniverse = Universe.createNew()
          val nSamples = Beta(2, 5)("", sampleUniverse)
          val samples = for (i <- 1 to 200)
            yield nSamples.generateValue(nSamples.generateRandomness())

          val universe = Universe.createNew()
          val a = Uniform(0, 10)("a", universe)
          val b = Uniform(0, 10)("b", universe)
          for (sample <- samples) {
            val beta = Beta(a, b)
            beta.observe(sample)
          }
          val alg = Importance(2000, a, b)
          alg.start()
          val resultA = alg.mean(a)
          val resultB = alg.mean(b)
          alg.stop()
          alg.kill
          update(resultA, NDTest.TTEST, "CompoundBetaTestResultsA", 2.0, alpha)
          update(resultB, NDTest.TTEST, "CompoundBetaTestResultsB", 5.0, alpha)
        }
      }

      ndtest.run(10)
    }
  }

  // We can test Dirichlets using the special case where alpha.length = 2
  "A AtomicDirichlet" should {
    "have value within a range with probability equal to the cumulative probability of the upper minus the lower" taggedAs (NonDeterministic) in {
      val ndtest = new NDTest {
        override def oneTest = {
          Universe.createNew()
          val a = 1.2
          val b = 0.5
          val elem = Dirichlet(a, b)
          val alg = Importance(20000, elem)
          alg.start()
          val dist = new BetaDistribution(a, b)
          val targetProb = dist.cumulative(0.3) - dist.cumulative(0.2)
          def check(ds: Array[Double]) = {
            val r = ds(0) / (ds(0) + ds(1))
            0.2 <= r && r < 0.3
          }
          val result = alg.probability(elem, (ds: Array[Double]) => check(ds))
          alg.stop()
          alg.kill
          update(result, NDTest.TTEST, "AtomicDirichletTestResults", targetProb, alpha)
        }
      }

      ndtest.run(10)
    }

    "produce the correct probability under Metropolis-Hastings" taggedAs (NonDeterministic) in {
      val ndtest = new NDTest {
        override def oneTest = {
          Universe.createNew()
          val a = 1.2
          val b = 0.5
          val elem = Dirichlet(a, b)
          val alg = MetropolisHastings(20000, ProposalScheme.default, elem)
          alg.start()
          val dist = new BetaDistribution(a, b)
          val targetProb = dist.cumulative(0.3) - dist.cumulative(0.2)
          def check(ds: Array[Double]) = {
            val r = ds(0) / (ds(0) + ds(1))
            0.2 <= r && r < 0.3
          }
          val result = alg.probability(elem, (ds: Array[Double]) => check(ds))
          alg.stop()
          alg.kill
          update(result, NDTest.TTEST, "AtomicDirichletTestResults", targetProb, alpha)
        }
      }

      ndtest.run(10)
    }

    "have the correct density" in {
      Universe.createNew()
      val a0 = 1.2
      val a1 = 0.5
      val elem = Dirichlet(a0, a1)
      val x0 = 0.3
      val x1 = 0.6
      val normalizer = gamma(a0 + a1) / (gamma(a0) * gamma(a1))
      val pow0 = pow(x0, a0 - 1)
      val pow1 = pow(x1, a1 - 1)
      val target = normalizer * pow(x0, a0 - 1) * pow(x1, a1 - 1)
      val result = elem.density(Array(x0, x1))
      result should be(target +- 0.00001)
    }

    "convert to the correct string" in {
      Universe.createNew()
      Dirichlet(2.5, 0.7).toString should equal("Dirichlet(2.5, 0.7)")
    }
  }

  "A CompoundDirichlet" should {
    "have value within a range with probability equal to the expectation over the alphas " +
      "of the cumulative probability of the upper minus the lower" taggedAs (NonDeterministic) in {
        val ndtest = new NDTest {
          override def oneTest = {
            Universe.createNew()
            val a = Select(0.5 -> 0.7, 0.5 -> 1.2)
            val b = Constant(0.5)
            val elem = Dirichlet(a, b)
            val alg = Importance(20000, elem)
            alg.start()
            val dist1 = new BetaDistribution(0.7, 0.5)
            val dist2 = new BetaDistribution(1.2, 0.5)
            def getProb(dist: ProbabilityDistribution) = dist.cumulative(0.3) - dist.cumulative(0.2)
            val targetProb = 0.5 * getProb(dist1) + 0.5 * getProb(dist2)
            def check(ds: Array[Double]) = {
              val r = ds(0) / (ds(0) + ds(1))
              0.2 <= r && r < 0.3
            }
            val result = alg.probability(elem, (ds: Array[Double]) => check(ds))
            alg.stop()
            alg.kill
            update(result, NDTest.TTEST, "CompoundDirichletTestResults", targetProb, alpha)
          }
        }

        ndtest.run(10)
      }

    "have the correct density" in {
      Universe.createNew()
      val a = 1.2
      val b = 0.5
      val elem = Beta(a, b)
      val dist = new BetaDistribution(a, b)
      val result = elem.density(0.3)
      result should be(dist.probability(0.3) +- 0.00001)
    }

    "convert to the correct string" in {
      Universe.createNew()
      val a = Select(0.5 -> 0.7, 0.5 -> 1.2)
      val b = Constant(0.5)
      Dirichlet(a, b).toString should equal("Dirichlet(" + a + ", " + b + ")")
    }

    /*
    "produce the right probability when conditioned under Metropolis-Hastings" taggedAs (NonDeterministic) in {
      val ndtest = new NDTest {
        override def oneTest = {
          val sampleUniverse = Universe.createNew()
          val nSamples = Dirichlet(1, 2, 3)("", sampleUniverse)
          val samples = for (i <- 1 to 100)
            yield nSamples.generateValue(nSamples.generateRandomness())

          val universe = Universe.createNew()
          val alpha1 = Uniform(0, 10)("a1", universe)
          val alpha2 = Uniform(0, 10)("a2", universe)
          val alpha3 = Uniform(0, 10)("a3", universe)
          for (sample <- samples) {
            val dirichlet = Dirichlet(alpha1, alpha2, alpha3)
            dirichlet.observe(sample)
          }
          val alg = MetropolisHastings(200000, ProposalScheme.default, alpha1, alpha2, alpha3)
          alg.start()
          val resultA = alg.mean(alpha1)
          val resultB = alg.mean(alpha2)
          val resultC = alg.mean(alpha3)
          alg.stop()
          alg.kill
          update(resultA, NDTest.TTEST, "CompoundDirichletTestResultsAlpha1", 1.0, alpha)
          update(resultB, NDTest.TTEST, "CompoundDirichletTestResultsAlpha2", 2.0, alpha)
          update(resultC, NDTest.TTEST, "CompoundDirichletTestResultsAlpha3", 3.0, alpha)
        }
      }

      ndtest.run(10)
    }
    * 
    */

    "produce the right probability when conditioned under Importance Sampling" taggedAs (NonDeterministic) in {
      val ndtest = new NDTest {
        override def oneTest = {
          val sampleUniverse = Universe.createNew()
          val nSamples = Dirichlet(1, 2, 3)("", sampleUniverse)
          val samples = for (i <- 1 to 200)
            yield nSamples.generateValue(nSamples.generateRandomness())

          val universe = Universe.createNew()
          val alpha1 = Uniform(0, 10)("a1", universe)
          val alpha2 = Uniform(0, 10)("a2", universe)
          val alpha3 = Uniform(0, 10)("a3", universe)
          for (sample <- samples) {
            val dirichlet = Dirichlet(alpha1, alpha2, alpha3)
            dirichlet.observe(sample)
          }
          val alg = Importance(2000, alpha1, alpha2, alpha3)
          alg.start()
          val resultA = alg.mean(alpha1)
          val resultB = alg.mean(alpha2)
          val resultC = alg.mean(alpha3)
          alg.stop()
          alg.kill
          update(resultA, NDTest.TTEST, "CompoundDirichletTestResultsAlpha1", 1.0, alpha)
          update(resultB, NDTest.TTEST, "CompoundDirichletTestResultsAlpha2", 2.0, alpha)
          update(resultC, NDTest.TTEST, "CompoundDirichletTestResultsAlpha3", 3.0, alpha)
        }
      }

      ndtest.run(10)
    }
  }
}
