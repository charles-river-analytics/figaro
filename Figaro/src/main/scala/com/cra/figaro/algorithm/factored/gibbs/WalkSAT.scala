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

package com.cra.figaro.algorithm.factored.gibbs

import com.cra.figaro.algorithm.factored.factors._
import com.cra.figaro.util._
import com.cra.figaro.language._
import scala.annotation.tailrec
import scala.collection.mutable.{Map => MutableMap}
import com.cra.figaro.algorithm.factored.factors._
import scala.collection.mutable.{Map => MutableMap}

class StateNotFoundException extends RuntimeException

object WalkSAT {
  /**
   * Produce an assignment of values to variables constrained by the given factors.
   * @param factors The factors with which to constrain the variables.
   * @param variables The set of variables over which to compute a sample.
   * @param semiring The semiring used in the factors.
   * @param prob The probability of reassigning a random variable in a factor instead of taking the greedy approach.
   * @param maxIterations Maximum number of iterations to run before throwing an exception for taking too long.
   */
  def apply[T](factors: List[Factor[T]], variables: Set[Variable[_]], semiring: Semiring[T], chainMapper: Chain[_,_] => Set[Variable[_]],
    prob: Double = 0.1, maxIterations: Int = 100000): MutableMap[Variable[_], Int] = {
    val currentSamples = MutableMap[Variable[_], Int]()
    val nonConstraintFactors = factors.filterNot(_.isConstraint)
    val variableParents = MutableMap[Variable[_], Set[Variable[_]]]()

    // Compute the set of parents of each variable that determine its value in generative order
    // Similar but not identical to variableParentMap used for Gibbs sampling
    variables.foreach(_ match {
      case ev: ElementVariable[_] => ev.element match {
        case a: Apply[_] => variableParents(ev) = a.args.map(Variable(_)).toSet
        case _ =>
      }

      case icv: InternalChainVariable[_] => {
        val chain = icv.chain.asInstanceOf[Chain[_, _]]
        val chainResults: Set[Variable[_]] = chainMapper(chain)
        //val chainResults: Set[Variable[_]] = LazyValues(chain.universe).getMap(chain).values.map(Variable(_)).toSet
        variableParents(icv) = chainResults + Variable(chain.parent)
        variableParents(icv.chainVar) = Set(icv)
      }
      case _ =>
    })

    pseudoForwardSample(variables)
    walkSAT(maxIterations)

    @tailrec
    def pseudoForwardSample(toSample: Set[Variable[_]]): Unit = {
      // Look for a variable who has no parents yet to be sampled
      val variableOption = toSample.find(variable => variableParents.getOrElse(variable, Set()).intersect(toSample).isEmpty)
      variableOption match {
        case Some((variableToSample)) => {
          // Get the adjacent factors
          val adjacentFactors = nonConstraintFactors.filter(f => f.variables.contains(variableToSample))
          val varAndParents = variableParents.getOrElse(variableToSample, Set()) + variableToSample
          // Marginalize to the variable and its parents
          val parentFactors = adjacentFactors.map(_.marginalizeTo(varAndParents.toList:_*))
          // Produce a sample
          val sampleOption = (0 until variableToSample.size).find(sample => parentFactors.forall(factor => {
            factor.get(factor.variables.map(currentSamples.getOrElse(_, sample))) != semiring.zero
          }))
          // Update and repeat until all variables are sampled
          currentSamples(variableToSample) = sampleOption.getOrElse(0)
          pseudoForwardSample(toSample - variableToSample)
        }
        case _ => {
          // In this case toSample is either empty, or we are in an invalid state
          // If the latter we have to set the remaining variables to something for WalkSAT
          toSample.foreach(currentSamples(_) = 0)
        }
      }
    }

    @tailrec
    def walkSAT(iterations: Int): Unit = {
      // Throw an exception when the maximum allowed iterations have been performed
      if(iterations == 0) throw new StateNotFoundException
      // Lists of satisfied and dissatisfied factors by the current samples
      val (satisfied, dissatisfied) = factors.partition(f => {
        val indices = f.variables.map(currentSamples(_))
        f.get(indices) != semiring.zero
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
                  f.get(indices) == semiring.zero
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