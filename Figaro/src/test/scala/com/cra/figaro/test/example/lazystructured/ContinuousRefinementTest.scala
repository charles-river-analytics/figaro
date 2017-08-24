/*
 * ContinuousRefinementTest.scala
 * Continuous refinement example tests.
 *
 * Created By:      William Kretschmer (kretsch@mit.edu)
 * Creation Date:   Aug 08, 2017
 *
 * Copyright 2017 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */
package com.cra.figaro.test.example.lazystructured

import com.cra.figaro.algorithm.structured.algorithm.{AnytimeStructuredProbQuery, StructuredProbQueryAlgorithm}
import com.cra.figaro.algorithm.structured.solver.marginalVariableElimination
import com.cra.figaro.algorithm.structured.strategy.range.RangingStrategy
import com.cra.figaro.algorithm.structured.strategy.refine._
import com.cra.figaro.algorithm.structured.strategy.solve._
import com.cra.figaro.language._
import com.cra.figaro.library.atomic.continuous.{Beta, Normal}
import com.cra.figaro.library.compound.If
import com.cra.figaro.test.tags.Example
import org.scalatest.{Matchers, WordSpec}

class ContinuousRefinementTest extends WordSpec with Matchers {
  "Continuous refinement" should {
    "converge to the posterior mean of the Beta" taggedAs Example in {
      val universe = Universe.createNew()
      // A simple model with continuous elements
      val e1 = Beta(2, 3)
      val e2 = Flip(e1)
      val e3 = Normal(0, 1)
      val e4 = Normal(1, 1)
      val e5 = If(e2, e3, e4)
      e5.addCondition(_ > 0)

      // Let's estimate the posterior mean of the Beta distribution
      val alg = new StructuredProbQueryAlgorithm(universe, e1) with AnytimeStructuredProbQuery {
        // Components associated with atomic elements
        lazy val atomicComponents = List(e1, e3, e4).map(collection(_))

        // Range by sampling 10 times from continuous atomic elements at each iteration
        override lazy val rangingStrategy: RangingStrategy = {
          RangingStrategy.default(10)
        }

        // Refine by first expanding the model, then round-robin sampling an atomic element at each iteration thereafter
        var iter = 0
        override def refiningStrategy(): RefiningStrategy = {
          iter += 1
          if(iter == 1) {
            // This strategy expands the model bottom-up, starting from the components given
            new BacktrackingStrategy(problem, List(collection(e5)))
          }
          else {
            // A top-down strategy refines the components given (e.g. by taking additional samples), then propagates
            // updates to all dependent elements
            new TopDownStrategy(collection, atomicComponents(iter % 3))
          }
        }

        // Solve using structured variable elimination
        override def solvingStrategy(): SolvingStrategy = {
          new ConstantStrategy(problem, structuredRaising, marginalVariableElimination)
        }

        // Use the optimized chain factor where applicable
        override def initialize(): Unit = {
          super.initialize()
          collection.useSingleChainFactor = true
        }
      }

      alg.start()
      Thread.sleep(10000)
      alg.stop()
      // The posterior density of e1 is proportional to x*(1-x)^2*(0.5*x + 0.8413*(1-x)), which integrates to 0.0587
      // Here, 0.8413 approximates the probability that a standard normal is greater than -1
      // Thus, the mean is approximately the integral from 0 to 1 of x^2*(1-x)^2*(0.5*x + 0.8413*(1-x)) / 0.0587 dx
      alg.mean(e1) should be(0.3808 +- 0.2)
      alg.kill()
    }
  }
}
