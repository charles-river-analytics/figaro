/*
 * ContinuousRefinement.scala
 * Example showing inference with refinement of continuous elements.
 *
 * Created By:      William Kretschmer (kretsch@mit.edu)
 * Creation Date:   Apr 14, 2017
 *
 * Copyright 2017 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.example.lazystructured

import com.cra.figaro.algorithm.structured.algorithm._
import com.cra.figaro.algorithm.structured.solver._
import com.cra.figaro.algorithm.structured.strategy.range.RangingStrategy
import com.cra.figaro.algorithm.structured.strategy.refine._
import com.cra.figaro.algorithm.structured.strategy.solve._
import com.cra.figaro.language._
import com.cra.figaro.library.atomic.continuous._
import com.cra.figaro.library.compound.If

/**
 * An example showing refinement of continuous elements using lazy structured factored inference.
 */
object ContinuousRefinement {
  def main(args: Array[String]): Unit = {
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

      override def initialize(): Unit = {
        super.initialize()
        // Use the optimized chain factor where applicable
        collection.useSingleChainFactor = true
      }
    }

    alg.start()
    println(s"Estimated posterior mean of $e1:")
    for(_ <- 1 to 20) {
      Thread.sleep(1000)
      println(alg.mean(e1))
    }
    alg.stop()
    alg.kill()
  }
}
