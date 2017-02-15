/*
 * DiscreteTest.scala  
 * Atomic discrete element tests.
 * 
 * Created By:      Brian Ruttenberg (bruttenberg@cra.com)
 * Creation Date:   Jan 1, 2009
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.test.library.atomic.discrete

import org.scalatest.Matchers
import org.scalatest.WordSpec
import com.cra.figaro.algorithm.factored._
import com.cra.figaro.algorithm._
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.library.atomic.discrete._
import com.cra.figaro.language._
import com.cra.figaro.library.compound._
import JSci.maths.statistics.{ BinomialDistribution, GeometricDistribution, PoissonDistribution }

class DiscreteTest extends WordSpec with Matchers {
  "A AtomicUniform" should {
    "have a value within the options with probability equal to 1 divided by the number of options" in {
      Universe.createNew()
      val elem = Uniform(1, 2, 3, 4)
      val alg = Importance(20000, elem)
      alg.start()
      alg.probability(elem)(i => 1 <= i && i < 3) should be(0.5 +- 0.01)
    }

    "for an input within the options have density equal to 1 divided by the number of options" in {
      Universe.createNew()
      Uniform(1, 2, 3, 4).density(2) should be(0.25 +- 0.000000001)
    }

    "for an input outside the options have density 0" in {
      Universe.createNew()
      Uniform(1, 2, 3, 4).density(5) should be(0.0 +- 0.000000001)
    }

    "convert to the correct string" in {
      Universe.createNew()
      Uniform(1, 2, 3, 4).toString should equal("Uniform(1, 2, 3, 4)")
    }

    "allow range computation" in {
      Universe.createNew()
      val e1 = Uniform(1, 2, 3, 4)
      Values()(e1) should equal(Set(1, 2, 3, 4))
    }

    "work under variable elimination" in {
      Universe.createNew()
      val e1 = Uniform(1, 2, 3, 4)
      val e2 = CachingChain(e1, (i: Int) => if (i == 1) Flip(0.6); else Flip(0.2))
      e1.addCondition((i: Int) => i < 3)
      val ve = VariableElimination(e2)
      ve.start()
      val p = 0.5 * 0.6 + 0.5 * 0.2
      ve.probability(e2, true) should be(p +- 0.0000000001)
    }
  }

  "A CompoundUniform" should {
    "have a value with probability equal to the expectation over the option distributions of the value " +
      "times 1 divided by the number of options" in {
        Universe.createNew()
        val sel1 = Select(0.2 -> 1, 0.3 -> 2, 0.5 -> 3)
        val sel2 = Select(0.3 -> 3, 0.7 -> 7)
        val elem = Uniform(sel1, sel2)
        val alg = Importance(20000, elem)
        alg.start()
        val targetProb = 0.5 * 0.5 + 0.5 * 0.3
        alg.probability(elem)(_ == 3) should be(targetProb +- 0.01)
      }

    "convert to the correct string" in {
      Universe.createNew()
      val sel1 = Select(0.2 -> 1, 0.3 -> 2, 0.5 -> 3)
      val sel2 = Select(0.3 -> 3, 0.7 -> 7)
      Uniform(sel1, sel2).toString should equal("Uniform(" + sel1 + ", " + sel2 + ")")
    }
  }

  "A AtomicGeometric" should {
    "have a value with probability equal to the probability of having that many failures minus 1 followed by a " +
      "success" in {
        Universe.createNew()
        val elem = Geometric(0.9)
        val alg = Importance(20000, elem)
        alg.start()
        val targetProb = 0.9 * 0.9 * 0.1
        alg.probability(elem)(_ == 3) should be(targetProb +- 0.01)
      }

    "produce the correct result under Metropolis-Hastings" in {
      Universe.createNew()
      val elem = Geometric(0.9)
      val alg = MetropolisHastings(50000, ProposalScheme.default, elem)
      alg.start()
      val targetProb = 0.9 * 0.9 * 0.1
      alg.probability(elem)(_ == 3) should be(targetProb +- 0.01)
    }

    "have the correct density" in {
      Universe.createNew()
      val elem = Geometric(0.9)
      val dist = new GeometricDistribution(0.1) // JSci parameterizes the geometric by probability of success
      elem.density(3) should be(dist.probability(3.0) +- 0.00000001)
    }

    "convert to the correct string" in {
      Universe.createNew()
      Geometric(0.9).toString should equal("Geometric(0.9)")
    }
  }

  "A CompoundGeometric" should {
    "have a value with probability equal to the expectation over the parameter of the geometric probability" in {
      Universe.createNew()
      val elem = Geometric(Select(0.5 -> 0.7, 0.5 -> 0.9))
      val alg = Importance(20000, elem)
      alg.start()
      val targetProb = 0.5 * 0.9 * 0.9 * 0.1 + 0.5 * 0.7 * 0.7 * 0.3
      alg.probability(elem)(_ == 3) should be(targetProb +- 0.01)
    }

    "convert to the correct string" in {
      Universe.createNew()
      val sel = Select(0.5 -> 0.7, 0.5 -> 0.9)
      Geometric(sel).toString should equal("Geometric(" + sel + ")")
    }
  }

  "A AtomicPoisson" should {
    "have a value with probability equal to the Poisson probability" in {
      Universe.createNew()
      val elem = Poisson(0.9)
      val alg = Importance(20000, elem)
      alg.start()
      val dist = new PoissonDistribution(0.9)
      val targetProb = dist.probability(3)
      alg.probability(elem)(_ == 3) should be(targetProb +- 0.01)
    }

    "produce the correct result under Metropolis-Hastings" in {
      Universe.createNew()
      val elem = Poisson(0.9)
      val alg = MetropolisHastings(50000, ProposalScheme.default, elem)
      alg.start()
      val dist = new PoissonDistribution(0.9)
      val targetProb = dist.probability(3)
      alg.probability(elem)(_ == 3) should be(targetProb +- 0.01)
    }

    "have the correct density" in {
      Universe.createNew()
      val elem = Poisson(0.9)
      val dist = new PoissonDistribution(0.9)
      elem.density(3) should be(dist.probability(3.0) +- 0.00000001)
    }

    "convert to the correct string" in {
      Universe.createNew()
      Poisson(0.9).toString should equal("Poisson(0.9)")
    }
    
    "calculate an accurate density for a large parameter" in {
      val tolerance = 0.0001
      val p1 = Poisson(50)
      val d1 = p1.density(25)
      d1 should equal (0.00010873 +- tolerance)
      
      val p2 = Poisson(100)
      val d2 = p2.density(50)
      d2 should equal (0.00000015 +- tolerance)
    }
  }

  "A CompoundAtomic" should {
    "have a value with probability equal to the expectation over the parameter of the Poisson probability" in {
      Universe.createNew()
      val elem = Poisson(Select(0.5 -> 0.7, 0.5 -> 0.9))
      val alg = Importance(20000, elem)
      alg.start()
      val dist1 = new PoissonDistribution(0.7)
      val dist2 = new PoissonDistribution(0.9)
      val targetProb = 0.5 * dist1.probability(3) + 0.5 * dist2.probability(3)
      alg.probability(elem)(_ == 3) should be(targetProb +- 0.01)
    }

    "convert to the correct string" in {
      Universe.createNew()
      val sel = Select(0.5 -> 0.7, 0.5 -> 0.9)
      Poisson(sel).toString should equal("Poisson(" + sel + ")")
    }
  }

  "An AtomicBinomial" should {
    "have a value with probability equal to the Binomial probability" in {
      Universe.createNew()
      val elem = Binomial(5, 0.9)
      val alg = Importance(20000, elem)
      alg.start()
      val dist = new BinomialDistribution(5, 0.9)
      val targetProb = dist.probability(3)
      alg.probability(elem)(_ == 3) should be(targetProb +- 0.01)
    }

    "produce the correct result under Metropolis-Hastings" in {
      Universe.createNew()
      val elem = Binomial(5, 0.9)
      val alg = MetropolisHastings(50000, ProposalScheme.default, elem)
      alg.start()
      val dist = new BinomialDistribution(5, 0.9)
      val targetProb = dist.probability(3)
      alg.probability(elem)(_ == 3) should be(targetProb +- 0.01)
    }

    "have the correct density" in {
      Universe.createNew()
      val elem = Binomial(5, 0.9)
      val dist = new BinomialDistribution(5, 0.9)
      elem.density(3) should be(dist.probability(3) +- 0.00000001)
    }

    "convert to the correct string" in {
      Universe.createNew()
      Binomial(5, 0.9).toString should equal("Binomial(5, 0.9)")
    }

    "allow range computation" in {
      Universe.createNew()
      val e1 = Binomial(5, 0.9)
      Values()(e1) should equal(Set(0, 1, 2, 3, 4, 5))
    }

    "work under variable elimination" in {
      Universe.createNew()
      val e1 = Binomial(3, 0.9)
      val e2 = CachingChain(e1, (i: Int) => if (i == 0) Flip(0.6); else Flip(0.2))
      val ve = VariableElimination(e2)
      ve.start()
      val p0 = 0.1 * 0.1 * 0.1
      val p = p0 * 0.6 + (1 - p0) * 0.2
      ve.probability(e2, true) should be(p +- 0.0000000001)
    }
	      
    "calculate an accurate density using a large number of trials" in {
      val tolerance = 0.00001
      val b1 = Binomial(10,0.10)
      val d1 = b1.density(5)
      d1 should equal (0.00148 +- tolerance)
      
      val b2 = Binomial(30,0.50)
      val d2 = b2.density(15)
      d2 should equal (0.14446 +- tolerance)

      val b3 = Binomial(500,0.50)
      val d3 = b3.density(250)
      //500!/(250!(500-250)! 
      //=
      //116744315788277682920934734762176619659230081180311446124100284957811112673608473715666417775521605376810865902709989580160037468226393900042796872256
      //151 digits
     
      //(0.50^250)^2
      //= 3.054936 * 10-151
      d3 should equal (0.03566 +- tolerance)
    }
  }

  "A CompoundBinomial" should {
    "have a value with probability equal to the expectation over the parameter of the binomial probability" in {
      Universe.createNew()
      val elem = Binomial(Constant(5), Select(0.5 -> 0.7, 0.5 -> 0.9))
      val alg = Importance(20000, elem)
      alg.start()
      val dist1 = new BinomialDistribution(5, 0.7)
      val dist2 = new BinomialDistribution(5, 0.9)
      val targetProb = 0.5 * dist1.probability(3) + 0.5 * dist2.probability(3)
      alg.probability(elem)(_ == 3) should be(targetProb +- 0.01)
    }

    "convert to the correct string" in {
      Universe.createNew()
      val con = Constant(5)
      val sel = Select(0.5 -> 0.7, 0.5 -> 0.9)
      Binomial(con, sel).toString should equal("Binomial(" + con + ", " + sel + ")")
    }
  }

  "A AtomicSwitchingFlip" should {
    "have a value with probability equal to the argument" in {
      Universe.createNew()
      val elem = SwitchingFlip(0.8)
      val alg = Importance(40000, elem)
      alg.start()
      alg.probability(elem)(b => b) should be(0.8 +- 0.01)
    }

    "always produce the opposite value for the next randomness" in {
      Universe.createNew()
      val elem = SwitchingFlip(0.8)
      for { i <- 1 to 10 } { elem.nextRandomness(0.7)._1 should be >= (0.8) }
      for { i <- 1 to 10 } { elem.nextRandomness(0.9)._1 should be < (0.8) }
    }

    "have compensation factor equal to the ratio of the probability of the old value to the new value" in {
      Universe.createNew()
      val elem = SwitchingFlip(0.8)
      val nr1 = elem.nextRandomness(0.7)
      nr1._2 * nr1._3 should be(0.2 / 0.8 +- 0.00000001)
      val nr2 = elem.nextRandomness(0.9)
      nr2._2 * nr2._3 should be(0.8 / 0.2 +- 0.00000001)
    }

    "produce the right probability under Metropolis-Hastings" in {
      Universe.createNew()
      val elem = SwitchingFlip(0.7)
      val alg = MetropolisHastings(20000, ProposalScheme.default, elem)
      alg.start()
      alg.probability(elem)(b => b) should be(0.7 +- 0.01)
    }

    "produce the right probability when conditioned under Metropolis-Hastings" in {
      Universe.createNew()
      val elem1 = SwitchingFlip(0.2)
      val elem2 = If(elem1, SwitchingFlip(0.7), SwitchingFlip(0.4))
      elem2.observe(true)
      val alg = MetropolisHastings(200000, ProposalScheme.default, elem1)
      alg.start()
      val p1 = 0.2 * 0.7
      val p2 = 0.8 * 0.4
      alg.probability(elem1)(b => b) should be(p1 / (p1 + p2) +- 0.01)
    }
  }
}
