/*
 * PBPTest.scala  
 * Particle Belief Propagation tests.
 * 
 * Created By:      Brian Ruttenberg (bruttenberg@cra.com)
 * Creation Date:   Jan 15, 2014
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.test.experimental.particlebp

import org.scalatest.WordSpec
import org.scalatest.Matchers
import com.cra.figaro.algorithm.factored._
import com.cra.figaro.algorithm.factored.beliefpropagation._
import com.cra.figaro.language._
import com.cra.figaro.library.compound.If
import com.cra.figaro.library.compound.^^
import com.cra.figaro.library.atomic.discrete.{ Uniform => DUniform }
import com.cra.figaro.library.atomic.continuous.{ Uniform => CUniform }
import com.cra.figaro.library.compound.IntSelector
import com.cra.figaro.algorithm.UnsupportedAlgorithmException
import com.cra.figaro.library.atomic.continuous.Normal
import com.cra.figaro.experimental.particlebp.ParticleBeliefPropagation
import com.cra.figaro.algorithm.factored.ParticleGenerator
import com.cra.figaro.language.Name.stringToName
import com.cra.figaro.library.atomic.continuous.{ Uniform => CUniform }
import com.cra.figaro.library.atomic.discrete.{ Uniform => DUniform }
import com.cra.figaro.library.atomic.discrete.Geometric
import com.cra.figaro.library.atomic.continuous.Beta
import com.cra.figaro.algorithm.sampling.Importance
import com.cra.figaro.experimental.particlebp.AutomaticDensityEstimator
import com.cra.figaro.algorithm.sampling.ProbEvidenceSampler
import com.cra.figaro.ndtest._
import org.apache.commons.math3.distribution.MultivariateNormalDistribution

class PBPTest extends WordSpec with Matchers {

  val globalTol = 0.025
  val alpha = 0.05

  "Running ParticleBeliefPropagation" should {

    "resample after the inner loop" in {
      Universe.createNew()
      val n = Normal(0.0, 1.0)
      val bpb = ParticleBeliefPropagation(1, 1, n)
      bpb.runInnerLoop(Set(), Set())
      val pbpSampler = ParticleGenerator(Universe.universe)
      val samples = pbpSampler(n)
      bpb.resample
      samples should not be pbpSampler(n)
    }

    "resample according to the resampler values " in {
      def mean(p: List[(Double, Double)]) = (0.0 /: p)((c: Double, n: (Double, Double)) => c + n._1 * n._2)
      Universe.createNew()
      val n = Normal(0.0, 1.0)
      val pbpSampler = ParticleGenerator(Universe.universe, new AutomaticDensityEstimator, 100, 100)
      val samples = pbpSampler(n)
      pbpSampler.resample(n, samples, List(samples), 0.05)
      val newSamples = pbpSampler(n)
      mean(newSamples) should be(mean(samples) +- 0.1)
    }

    "change the factor structure with new samples on the outer loop" in {
      Universe.createNew()
      val n = Normal(2.0, 2.0)
      val number = Apply(n, (d: Double) => d.round.toInt)
      val items = Chain(number, (num: Int) => {
        val f = for { i <- 0 until num } yield Flip(0.5)
        Inject(f: _*)
      })
      val pbpSampler = ParticleGenerator(Universe.universe)
      pbpSampler.update(n, pbpSampler.numArgSamples, List[(Double, _)]((1.0, 2.0)))
      val bpb = ParticleBeliefPropagation(1, 1, items)
      bpb.runOuterLoop
      val fg_2 = bpb.bp.factorGraph.getNodes.filter(p => p.isInstanceOf[VariableNode]).toSet

      pbpSampler.update(n, pbpSampler.numArgSamples, List[(Double, _)]((1.0, 3.0)))
      val dependentElems = Set[Element[_]](n, number, items)
      bpb.runInnerLoop(dependentElems, Set())
      // Currently have to subtract 3 since the old factors for n = 2 also get created since they exist in the chain cache
      val fg_3 = bpb.bp.factorGraph.getNodes.filter(p => p.isInstanceOf[VariableNode]).toSet
      val diff = fg_3 -- fg_2
      diff.nonEmpty should equal(true)
    }

    /* Due to the way that factors are implemented for Chain, all 
     * models that use chain will result in loops. To test a non-loopy
     * graph we have to not use chain, which IntSelector does not.
     */
    "with no loops in the factor graph give exact results" in {
      Universe.createNew()
      //com.cra.figaro.util.setSeed(13)
      val e1 = CUniform(1.5, 4.5)
      val ep = Apply(e1, (d: Double) => d.round.toInt)
      val e2 = IntSelector(ep)

      val bp = ParticleBeliefPropagation(5, 30, 100, 100, e2, e1, ep)
      bp.start

      val e2_0 = 0.33333333 * (0.5 + 0.3333333 + 0.25)
      val e2_1 = 0.33333333 * (0.5 + 0.3333333 + 0.25)
      val e2_2 = 0.33333333 * (0 + 0.3333333 + 0.25)
      val e2_3 = 0.33333333 * (0 + 0 + 0.25)

      val tol = 0.025
      val a = bp.distribution(e1).toList

      val b = bp.distribution(ep).toList
      bp.probability(e2, (i: Int) => i == 0) should be(e2_0 +- tol)
      bp.probability(e2, (i: Int) => i == 1) should be(e2_1 +- tol)
      bp.probability(e2, (i: Int) => i == 2) should be(e2_2 +- tol)
      bp.probability(e2, (i: Int) => i == 3) should be(e2_3 +- tol)
    }

    "correctly retrieve the last messages to recompute sample densities" in {
      Universe.createNew()
      val n = Normal(2.0, 2.0)
      val number = Apply(n, (d: Double) => d.round.toInt)
      val items = Chain(number, (num: Int) => {
        val f = for { i <- 0 until num } yield Flip(0.5)
        Inject(f: _*)
      })
      val bpb = ParticleBeliefPropagation(1, 1, items)
      bpb.runInnerLoop(Set(), Set())
      val msgs = bpb.getLastMessagesToNode(n)
      msgs.size should be > 1
      msgs(1).numVars should be(1)
      msgs(1).contents.map(_._2).sum should be(0.0 +- .01)
    }

    "with no conditions or constraints produce the correct result for a one time algorithm" in {
      val ndtest = new NDTest {
        override def oneTest = {
          Universe.createNew()
          val u = CUniform(0.3, 0.9)
          val f = Flip(u)
          val a = If(f, Select(0.3 -> 1, 0.7 -> 2), Constant(2))
          val result = test(f, 1, 30, 100, 100, (b: Boolean) => b, 0.6, globalTol)
          update(result, NDTest.TTEST, "NoConditionsOrConstraints", 0.6, alpha)
        }
      }
      ndtest.run(10)
    }

    "with a condition on a dependent element produce the result with the correct probability for a one time algorithm" in {
      val ndtest = new NDTest {
        override def oneTest = {
          Universe.createNew()
          val u = CUniform(0.3, 0.9)
          val f = Flip(u)
          val a = If(f, Select(0.3 -> 1, 0.7 -> 2), Constant(2))
          a.setCondition((i: Int) => i == 2)
          // U(true) = \int_{0.2}^{1.0) 0.7 p = 0.35 * 0.96
          // U(false) = \int_{0.2}^{1.0) (1-p)
          val u1 = 0.35 * 0.96
          val u2 = 0.32
          val result = test(f, 10, 30, 100, 100, (b: Boolean) => b, u1 / (u1 + u2), globalTol)
          update(result - (u1 / (u1 + u2)), NDTest.TTEST, "ConditionOnDependentElement", 0.0, alpha)
        }
      }
      ndtest.run(10)
    }

    "with a constraint on a dependent element produce the result with the correct probability for a one time algorithm" in {
      val ndtest = new NDTest {
        override def oneTest = {
          Universe.createNew()
          val u = CUniform(0.3, 0.9)
          val f = Flip(u)
          val a = If(f, Select(0.3 -> 1, 0.7 -> 2), Constant(2))
          a.setConstraint((i: Int) => i.toDouble)
          // U(true) = \int_{0.2}^{1.0} (0.3 + 2 * 0.7) p = 0.85 * 0.96
          // U(false) = \int_{0.2}^(1.0) (2 * (1-p)) = 0.64
          val u1 = 0.85 * 0.96
          val u2 = 0.64
          val result = test(f, 10, 30, 100, 100, (b: Boolean) => b, u1 / (u1 + u2), globalTol)

          update(result - (u1 / (u1 + u2)), NDTest.TTEST, "ConstraintOnDependentElement", 0.00, alpha)
        }
      }
      ndtest.run(10)

    }

    "with no conditions or constraints produce the correct result for a anytime algorithm" in {
      Universe.createNew()
      val u = CUniform(0.3, 0.9)
      val f = Flip(u)
      val a = If(f, Select(0.3 -> 1, 0.7 -> 2), Constant(2))
      test(f, 1000, 500, 100, 100, (b: Boolean) => b, 0.6, globalTol, false) should be(0.6 +- globalTol)
    }

    "with a condition on a dependent element produce the result with the correct probability for an anytime algorithm" in {
      val ndtest = new NDTest {
        override def oneTest = {
          Universe.createNew()
          val u = CUniform(0.3, 0.9)
          val f = Flip(u)
          val a = If(f, Select(0.3 -> 1, 0.7 -> 2), Constant(2))
          a.setCondition((i: Int) => i == 2)
          // U(true) = \int_{0.2}^{1.0) 0.7 p = 0.35 * 0.96
          // U(false) = \int_{0.2}^{1.0) (1-p)
          val u1 = 0.35 * 0.96
          val u2 = 0.32
          val result = test(f, 5000, 250, 100, 100, (b: Boolean) => b, u1 / (u1 + u2), globalTol, false)
          update(result, NDTest.TTEST, "ConditionOnDependentElement", u1 / (u1 + u2), alpha)
        }
      }
      ndtest.run(10)
    }

    "with a constraint on a dependent element produce the result with the correct probability for an anytime algorithm" in {
      val ndtest = new NDTest {
        override def oneTest = {
          Universe.createNew()
          val u = CUniform(0.3, 0.9)
          val f = Flip(u)
          val a = If(f, Select(0.3 -> 1, 0.7 -> 2), Constant(2))
          a.setConstraint((i: Int) => i.toDouble)
          // U(true) = \int_{0.2}^{1.0} (0.3 + 2 * 0.7) p = 0.85 * 0.96
          // U(false) = \int_{0.2}^(1.0) (2 * (1-p)) = 0.64
          val u1 = 0.85 * 0.96
          val u2 = 0.64
          val result = test(f, 5000, 250, 100, 100, (b: Boolean) => b, u1 / (u1 + u2), globalTol, false)
          update(result, NDTest.TTEST, "ConstraintOnDependentElement", u1 / (u1 + u2), alpha)
        }
      }
      ndtest.run(10)
    }

    "with an element that uses another element multiple times, " +
      "always produce the same value for the different uses" in {
        Universe.createNew()
        val f = CUniform(0.3, 0.9)
        val e = f === f
        test(e, 1, 30, 100, 100, (b: Boolean) => b, 1.0, globalTol) should be(1.0 +- globalTol)
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
      test(d, 1, 30, 100, 100, (b: Boolean) => b, 0.73, globalTol) should be(0.73 +- globalTol)
    }

    "on a different universe from the current universe, produce the correct result" in {
      val ndtest = new NDTest {
        override def oneTest = {
          val u1 = Universe.createNew()
          val u = CUniform(0.3, 0.9)
          val f = Flip(u)
          val a = If(f, Select(0.3 -> 1, 0.7 -> 2), Constant(2))
          Universe.createNew()
          val algorithm = ParticleBeliefPropagation(1, 20, 100, 100, f)(u1)
          algorithm.start()
          val result = algorithm.probability(f, (b: Boolean) => b)
          algorithm.kill()
          update(result, NDTest.TTEST, "DifferentUniverse", 0.6, alpha)
        }
      }
      ndtest.run(10)

    }

    /*
    "with a posterior different than the prior, converge upon the correct answer on a discrete variable" in {
      Universe.createNew()
      val fp = Geometric(0.9)
      val f = Flip(Apply(fp, (i: Int) => 1.0/(1.0+math.exp(-1*i.toDouble/10))))
      val s1 = Select(0.1 -> 1, 0.9 -> 2)
      val s2 = Select(0.7 -> 1, 0.2 -> 2, 0.1 -> 3)
      val c = Chain(f, (b: Boolean) => if (b) s1; else s2)
      c.observe(1)
      // ans obtained by importance sampling = 7.49
      val algorithm = ParticleBeliefPropagation(10, 40, 50, 50, fp)
      algorithm.start()
      algorithm.expectation(fp, (i: Int) => i.toDouble) should be(7.49 +- globalTol)
    }
    * 
    */

    "with a posterior different than the prior, converge upon the correct posterior distribution of a continuous variable" in {
      val ndtest = new NDTest {
        override def oneTest = {
          Universe.createNew()
          val fp = CUniform(0.0, 1.0)
          val f = Flip(fp)
          val s1 = Select(0.1 -> 1, 0.9 -> 2)
          val s2 = Select(0.7 -> 1, 0.2 -> 2, 0.1 -> 3)
          val c = Chain(f, (b: Boolean) => if (b) s1; else s2)
          c.observe(1)
          // ans = \int_0_1 [-.6*x + .7 dx]*x / \int_0_1 [-.6*x + .7 dx] = .15/.4 = .375
          val algorithm = ParticleBeliefPropagation(10, 40, 100, 100, fp)
          algorithm.start()
          // algorithm.expectation(fp, (i: Double) => i) should be(.375 +- globalTol)
          val result = algorithm.expectation(fp, (i: Double) => i)
          algorithm.kill

          update(result, NDTest.TTEST, "PosteriorDifferentThanPrior", 0.375, alpha)
        }
      }
      ndtest.run(10)
    }

    "with a posterior different than the prior, reduce the variance of the posterior as compared to BP" in {
      val origCov = 8.0
      val mvn = new MultivariateNormalDistribution(Array(0.0, 0.0), Array(Array(8.0, origCov), Array(origCov, 16.0)))
      Universe.createNew()
      val locX = CUniform(20, 80)("X", Universe.universe)
      val locY = CUniform(20, 80)("Y", Universe.universe)
      val loc = ^^(locX, locY)
      loc.addConstraint((l: (Double, Double)) => {
        mvn.density(Array((l._1 - 40.0), (l._2 - 40.0)))
      })
      val algorithm = ParticleBeliefPropagation(10, 4, 15, 15, loc, locX, locY)
      algorithm.start()
      val locE = algorithm.expectation(loc, (d: (Double, Double)) => d._1 * d._2)
      val cov = locE - algorithm.mean(locX) * algorithm.mean(locY)

      ParticleGenerator.clear
      val algorithm2 = ParticleBeliefPropagation(1, 20, 15, 15, loc, locX, locY)
      algorithm2.start()
      val locE2 = algorithm2.expectation(loc, (d: (Double, Double)) => d._1 * d._2)
      val cov2 = locE - algorithm2.mean(locX) * algorithm2.mean(locY)
      (math.abs(cov - origCov) < math.abs(origCov - cov2)) should be(true)

    }

    "with a dependent universe, correctly take into account probability of evidence in the dependent universe" in {
      Universe.createNew()
      val x = IntSelector(Constant(10))
      val y = IntSelector(Constant(10))
      val x1 = Apply(x, (i: Int) => i < 1)
      val y1 = Apply(y, (i: Int) => i < 2)
      val dependentUniverse = new Universe(List(x1, y1))
      val u1 = CUniform(0.0, 1.0)("", dependentUniverse)
      val u2 = CUniform(0.0, 2.0)("", dependentUniverse)

      val a = CachingChain(x1, y1, (x: Boolean, y: Boolean) => if (x || y) u1; else u2)("a", dependentUniverse)
      val condition = (d: Double) => d < 0.5
      val ve = ParticleBeliefPropagation(List((dependentUniverse, List(NamedEvidence("a", Condition(condition))))),
        (u: Universe, e: List[NamedEvidence[_]]) => () => ProbEvidenceSampler.computeProbEvidence(10000, e)(u),
        1, 40, x1)
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
      val ve = ParticleBeliefPropagation(3, 50, y)
      ve.start()
      ve.probability(y, true) should be(((0.1 * 0.2 + 0.9 * 0.2) / (0.1 * 0.2 + 0.9 * 0.2 + 0.9 * 0.8)) +- globalTol)
    }

  }

  /*
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
  * 
  */

  def test[T](target: Element[T], outer: Int, inner: Int,
    argSamples: Int, totalSamples: Int,
    predicate: T => Boolean, prob: Double, tol: Double, oneTime: Boolean = true): Double = {
    val algorithm = if (oneTime) {
      ParticleBeliefPropagation(outer, inner, argSamples, totalSamples, target)
    } else {
      ParticleBeliefPropagation(inner.toLong, argSamples, totalSamples, target)
    }
    algorithm.start()
    if (!oneTime) Thread.sleep(outer.toLong)
    algorithm.stop
    //    algorithm.probability(target, predicate) should be(prob +- tol)

    val result = algorithm.probability(target, predicate)
    algorithm.kill

    result
  }
}