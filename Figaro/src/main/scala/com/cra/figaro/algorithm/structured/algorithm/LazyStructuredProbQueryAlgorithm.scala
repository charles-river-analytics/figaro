/*
 * LazyStructuredProbQueryAlgorithm.scala
 * Lazy SFI algorithms that compute probability bounds.
 *
 * Created By:      William Kretschmer (kretsch@mit.edu)
 * Creation Date:   Jan 09, 2017
 *
 * Copyright 2017 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */
package com.cra.figaro.algorithm.structured.algorithm

import com.cra.figaro.algorithm._
import com.cra.figaro.language._
import akka.pattern.ask
import com.cra.figaro.algorithm.factored.factors.factory.Factory
import com.cra.figaro.algorithm.factored.factors.{BoundsSumProductSemiring, Factor, SumProductSemiring}
import com.cra.figaro.algorithm.lazyfactored.Regular
import com.cra.figaro.algorithm.structured.{Bounds, Lower, Upper}
import com.cra.figaro.algorithm.structured.solver._

abstract class LazyStructuredProbQueryAlgorithm(val universe: Universe, val queryTargets: Element[_]*)
  extends StructuredAlgorithm with LazyStructured {

  override def problemTargets = queryTargets.toList

  // Solutions are bounds factors marginalized to individual targets.
  protected var targetFactors: Map[Element[_], Factor[(Double, Double)]] = Map()

  override def processSolutions(solutions: Map[Bounds, Solution]): Unit = {
    // Get the factors associated with lower and upper constraints. It might be the case that the map doesn't contain
    // both keys because the lower and upper factors are the same; in this case we use getOrElse, and will have
    // lowerFactors == upperFactors.
    val lowerFactors = solutions.getOrElse(Lower, solutions(Upper))._1
    val upperFactors = solutions.getOrElse(Upper, solutions(Lower))._1
    val jointLower = lowerFactors.foldLeft(Factory.unit(SumProductSemiring()))(_.product(_))
    val jointUpper = upperFactors.foldLeft(Factory.unit(SumProductSemiring()))(_.product(_))
    val jointBounds = boundsFactor(jointLower, jointUpper)
    targetFactors = queryTargets.map(target => (target, jointBounds.marginalizeTo(collection(target).variable))).toMap
  }

  /**
   * This method is equivalent to the `normalizeAndAbsorbWithBounds` method in `LazyVariableElimination`. It takes a
   * factor of unnormalized lower bounds for regular values and *, and another factor containing unnormalized upper
   * bounds for regular values and *, and computes normalized lower and upper bounds for each regular value. These
   * bounds guarantee that the true probability of the regular value lies between the bounds.
   *
   * This consists of several steps:
   * (1) Compute the sums of the unnormalized lower and upper bounds. These constitute lower and upper bounds on the
   * normalizing factor, respectively.
   * (2) Compute the normalized lower bounds, which are the unnormalized lower bounds divided by the upper bound on the
   * normalizing factor. The normalized lower bound on a regular value represents definitely allocated probability to
   * that value, that cannot possibly be allocated to any other regular value. This fact will be used in calculating one
   * of the upper bounds.
   * (3) Compute the first upper bound. This consists of all the probability mass that has not definitely been allocated
   * to other values. Since the resulting factor might have more than one variable, we need to determine which rows
   * probability mass can definitely not be allocated to a particular row. This is determined by the consistent
   * subroutine below. The first upper bound on the normalized probability of a given row is equal to 1 - the sum of
   * normalized lower bounds of rows that are not consistent with this row.
   * (4) For the second upper bound, we use the opposite approach. We add together all the unnormalized upper bounds of
   * rows that are consistent with this row, and then divide by the lower bound on the normalizing factor.
   * (5) Finally, we set the lower and upper bounds of the row in the result to be the computed normalized lower bound
   * and the lesser of the two computed upper bounds.
   * @param lowerFactor Unnormalized lower bounds.
   * @param upperFactor Unnormalized upper bounds.
   * @return A factor containing bounds on each of the regular values.
   */
  def boundsFactor(lowerFactor: Factor[Double], upperFactor: Factor[Double]): Factor[(Double, Double)] = {
    assert(lowerFactor.variables == upperFactor.variables)
    val result = Factory.defaultFactor(lowerFactor.parents, lowerFactor.output, BoundsSumProductSemiring())

    // First, we compute the sum of unnormalized lower and upper bounds.
    var lowerTotal = 0.0
    var upperTotal = 0.0
    for { indices <- result.getIndices } {
      val lower = lowerFactor.get(indices)
      val upper = upperFactor.get(indices)
      lowerTotal += lower
      upperTotal += upper
    }

    // Index into each variable of * (or -1 if all values are regular)
    val starIndices = result.variables.map(_.range.indexWhere(!_.isRegular))

    // Determine if the second row can be absorbed into the first. This is true iff, for each position, either the two
    // values are equal, or the second value is *.
    def consistent(indices1: List[Int], indices2: List[Int]): Boolean = {
      indices1.zip(indices2).zip(starIndices).forall{ case ((index1, index2), starIndex) =>
        // The two extended values are equal iff index1 == index1
        // Otherwise, compare index2 to the index of * (which could be -1 if * is not present)
        index1 == index2 || index2 == starIndex
      }
    }

    /*
     * There are two possible upper bounds of a row. The first is 1 - the definite lower bounds of incompatible rows,
     * which we compute by summing the incompatible rows in the unnormalized lower factor, using upperTotal as an
     * upper bound on the normalizing factor. The second is the sum of all compatible rows' unnormalized upper bounds
     * divided by the lower bound on the normalizing factor. We use the lesser of these two bounds.
     */
    for { indices <- result.getIndices } {
      // Compute the inconsistent and consistent totals
      var inconsistentLowerTotal = 0.0
      var consistentUpperTotal = 0.0
      for { otherIndices <- result.getIndices } {
        if (consistent(indices, otherIndices)) consistentUpperTotal += upperFactor.get(otherIndices)
        else inconsistentLowerTotal += lowerFactor.get(otherIndices)
      }
      // Get the definitely allocated probability mass: upperTotal is an upper bound on the normalizing factor.
      val lowerBound = lowerFactor.get(indices) / upperTotal
      // Use the lesser of the two upper bounds described above
      val upperBound = (1.0 - inconsistentLowerTotal / upperTotal).min(consistentUpperTotal / lowerTotal)
      result.set(indices, (lowerBound, upperBound))
    }

    result
  }

  class NotATargetException[T](target: Element[T]) extends AlgorithmException

  /**
   * Check if the call to query the target is valid.
   */
  protected def check[T](target: Element[T]): Unit = {
    if (!active) throw new AlgorithmInactiveException
    if (!(queryTargets contains target)) throw new NotATargetException(target)
  }

  /**
   * Return an estimate from the marginal probability distribution over the target of the lower and upper bounds,
   * respectively, of the particular value given.
   * Particular algorithms must implement this method.
   */
  def computeProbabilityBounds[T](target: Element[T], value: T): (Double, Double) = {
    val targetVar = collection(target).variable
    val index = targetVar.range.indexOf(Regular(value))
    if(index == -1) (0.0, 1.0)
    else targetFactors(target).get(List(index))
  }

  /**
   * Helper method implemented in OneTime and Anytime versions of this algorithm.
   */
  protected def doProbabilityBounds[T](target: Element[T], value: T): (Double, Double)

  /**
   * Return an estimate from the marginal probability distribution over the target of the lower and upper bounds,
   * respectively, of the particular value given.
   * Throws NotATargetException if called on a target that is not in the list of targets of the algorithm.
   * Throws AlgorithmInactiveException if the algorithm is inactive.
   */
  def probabilityBounds[T](target: Element[T], value: T): (Double, Double) = {
    check(target)
    doProbabilityBounds(target, value)
  }

  /**
   * Return an estimate from the marginal probability distribution over the target of the lower and upper bounds of each
   * value in the range of the target. Returns a stream of tuples (lower, upper, value).
   * Particular algorithms must implement this method.
   */
  def computeAllProbabilityBounds[T](target: Element[T]): Stream[(Double, Double, T)] = {
    val targetVar = collection(target).variable
    val factor = targetFactors(target)
    val allBounds = for((xvalue, index) <- targetVar.range.zipWithIndex if xvalue.isRegular) yield {
      val (lower, upper) = factor.get(List(index))
      (lower, upper, xvalue.value)
    }
    allBounds.toStream
  }

  /**
   * Helper method implemented in OneTime and Anytime versions of this algorithm.
   */
  protected def doAllProbabilityBounds[T](target: Element[T]): Stream[(Double, Double, T)]

  /**
   * Return an estimate from the marginal probability distribution over the target of the lower and upper bounds of each
   * value in the range of the target. Returns a stream of tuples (lower, upper, value).
   * Throws NotATargetException if called on a target that is not in the list of targets of the algorithm.
   * Throws AlgorithmInactiveException if the algorithm is inactive.
   */
  def allProbabilityBounds[T](target: Element[T]): Stream[(Double, Double, T)] = {
    check(target)
    doAllProbabilityBounds(target)
  }
}

trait OneTimeLazyStructuredProbQuery extends LazyStructuredProbQueryAlgorithm with OneTimeStructured {
  override protected def doProbabilityBounds[T](target: Element[T], value: T): (Double, Double) = {
    computeProbabilityBounds(target, value)
  }

  override protected def doAllProbabilityBounds[T](target: Element[T]): Stream[(Double, Double, T)] = {
    computeAllProbabilityBounds(target)
  }
}

trait AnytimeLazyStructuredProbQuery extends LazyStructuredProbQueryAlgorithm with AnytimeStructured {
  /**
   * A message instructing the handler to compute the probability bounds of the target element and value.
   */
  case class ComputeProbabilityBounds[T](target: Element[T], value: T) extends Service

  /**
   * A message instructing the handler to compute all probability bounds of the target element.
   */
  case class ComputeAllProbabilityBounds[T](target: Element[T]) extends Service

  /**
   * A message from the handler containing the lower and upper bounds of the probability of value queried.
   */
  case class ProbabilityBounds(bounds: (Double, Double)) extends Response

  /**
   * A message from the handler containing all lower and upper bounds of the target queried.
   */
  case class AllProbabilityBounds[T](allBounds: Seq[(Double, Double, T)]) extends Response

  override def handle(service: Service): Response =
    service match {
      case ComputeProbabilityBounds(target, value) =>
        ProbabilityBounds(computeProbabilityBounds(target, value))
      case ComputeAllProbabilityBounds(target) =>
        AllProbabilityBounds(computeAllProbabilityBounds(target))
    }

  override protected def doProbabilityBounds[T](target: Element[T], value: T): (Double, Double) = {
    awaitResponse(runner ? Handle(ComputeProbabilityBounds(target, value)), messageTimeout.duration) match {
      case ProbabilityBounds(bounds) => bounds
      case _ => (0.0, 1.0)
    }
  }

  override protected def doAllProbabilityBounds[T](target: Element[T]): Stream[(Double, Double, T)] = {
    awaitResponse(runner ? Handle(ComputeAllProbabilityBounds(target)), messageTimeout.duration) match {
      case AllProbabilityBounds(allBounds) => allBounds.asInstanceOf[Stream[(Double, Double, T)]]
      case _ => Stream()
    }
  }
}
