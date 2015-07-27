/*
 * WalkSAT.scala
 * A WalkSAT-like procedure for finding a valid state on a set of factors.
 *
 * Created By:      William Kretschmer (kretsch@mit.edu)
 * Creation Date:   July 20, 2015
 *
 * Copyright 2015 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.experimental.factored

import scala.collection.mutable.Map
import com.cra.figaro.algorithm.factored.factors.{Factor, Variable}
import com.cra.figaro.util._
import scala.annotation.tailrec

class StateNotFoundException extends RuntimeException

// TODO: consider turning this into a class that extends from Algorithm
// Also consider forward sampling first if there is no hard evidence
object WalkSAT {
  /**
   * Produce an assignment of values to variables constrained by the given factors.
   * @param factors The factors with which to constrain the variables
   * @param prob The probability of reassigning a random variable in a factor instead of taking the greedy approach.
   * @param maxIterations Maximum number of iterations to run before throwing an exception for taking too long.
   */
  def apply(factors: List[Factor[Double]], prob: Double = 0.1, maxIterations: Int = 100000): Map[Variable[_], Int] = {
    val variables = factors.flatMap(_.variables).distinct
    val currentSamples: Map[Variable[_], Int] = Map(variables.map(v => (v, 0)):_*)

    walkSAT(maxIterations)

    @tailrec
    def walkSAT(iterations: Int): Unit = {
      // Throw an exception when the maximum allowed iterations have been performed
      if(iterations == 0) throw new StateNotFoundException
      // Lists of satisfied and dissatisfied factors by the current samples
      val (satisfied, dissatisfied) = factors.partition(f => {
        val indices = f.variables.map(currentSamples(_))
        f.get(indices) > 0.0
      })

      // Stop when all factors are satisfied
      if(!dissatisfied.isEmpty) {
        // Pick a random dissatisfied factor
        val factor = dissatisfied(random.nextInt(dissatisfied.length))

        // With some probability, pick a random variable and sample uniformly from its range
        if(random.nextDouble() < prob) {
          val variable = factor.variables(random.nextInt(factor.numVars))
          val sample = random.nextInt(variable.size)
          // Update currentSamples
          currentSamples(variable) = sample
        }
        // Otherwise, change the variable with the corresponding sample
        // that leaves the fewest dissatisfied factors that were previously satisfied
        else {
          val (variable, sample, _) = factor.variables.flatMap(v => {
            (0 until v.size).map(index => {
              // This line prevents picking the same sample again
              if(index == currentSamples(v)) (v, index, Int.MaxValue)
              else {
                // Count the number of previously satisfied factors that become dissatisfied
                val count = satisfied.count(f => {
                  val indices = f.variables.map(vf => if(vf == v) index else currentSamples(vf))
                  f.get(indices) <= 0.0
                })
                (v, index, count)
              }
            })
          }).minBy(_._3)
          // Update currentSamples
          currentSamples(variable) = sample
        }
        walkSAT(iterations - 1)
      }
    }

    currentSamples
  }
}