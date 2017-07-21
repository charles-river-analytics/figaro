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

import com.cra.figaro.algorithm.factored.factors.{Factor, Variable}
import com.cra.figaro.algorithm.structured.{Lower, Upper}
import com.cra.figaro.algorithm.{AnytimeBoundsProbQuery, BoundsProbQueryAlgorithm, OneTimeBoundsProbQuery}
import com.cra.figaro.language._

abstract class LazyStructuredProbQueryAlgorithm(universe: Universe, queryTargets: Element[_]*)
  extends StructuredProbQueryAlgorithm(universe, queryTargets:_*)
  with LazyStructured with BoundsProbQueryAlgorithm {

  override protected def useBoundsString: String =
    "use a bounding method instead, or a ranging strategy that avoids *"

  /**
   * Get the marginalized lower/upper factors for a particular target. This method is intended to be called once per
   * inference query, and avoids concurrency issues. It guarantees that the factors returned are consistent and from a
   * single solving run. Returns a pair containing, respectively, the marginalized factor computed using lower
   * constraint bounds, and the marginalized factor computed using upper constraint bounds.
   */
  protected def boundFactors[T](target: Element[T]): (Factor[Double], Factor[Double]) = {
    val solutions = targetFactors
    // It is possible that the lower and upper factors are the same, in which case the solutions will contain an entry
    // for Lower or Upper, but not both. This returns the same factor both times.
    val lowerFactor = solutions.getOrElse(Lower, solutions(Upper))(target)
    val upperFactor = solutions.getOrElse(Upper, solutions(Lower))(target)
    // The factors should be over the same variable, or should both be empty because the variable's range is {*}.
    assert(lowerFactor.variables.size <= 1)
    assert(lowerFactor.variables == upperFactor.variables)
    (lowerFactor, upperFactor)
  }

  /**
   * Computes bounds on each regular and extended value given lower and upper factors marginalized to a single variable.
   * The first entry contains bounds on each regular value as a list of triples (lower, upper, value). The second and
   * third entries are, respectively, the lower and upper bounds on *. These are bounds on the probability mass that
   * could be placed on values not yet in the range of the target.
   */
  protected def regularAndStarBounds[T](lowerFactor: Factor[Double], upperFactor: Factor[Double]): (List[(Double, Double, T)], Double, Double) = {
    val variable = lowerFactor.variables.headOption.asInstanceOf[Option[Variable[T]]]
    // The factors are empty because the variable's range is {*}
    if(variable.isEmpty) return (List(), 1.0, 1.0)

    // Lower and upper bounds on the partition function are given, respectively, by summing entries in the lower and
    // upper factors
    val partitionLower = lowerFactor.foldLeft(0.0, _ + _)
    val partitionUpper = upperFactor.foldLeft(0.0, _ + _)

    val range = variable.get.range
    val starIndex = range.indexWhere(!_.isRegular)
    // Entries in the lower and upper factors corresponding to *
    val starLower = if(starIndex >= 0) lowerFactor.get(List(starIndex)) else 0.0
    val starUpper = if(starIndex >= 0) upperFactor.get(List(starIndex)) else 0.0

    // List of bounds on each regular value
    val regularBounds = for((xvalue, idx) <- range.zipWithIndex if xvalue.isRegular) yield {
      // Entries in the lower and upper factors corresponding to this regular value
      val valueLower = lowerFactor.get(List(idx))
      val valueUpper = upperFactor.get(List(idx))
      // There are two lower bounds on the probability of this value. One corresponds to the probability known to be
      // allocated to this value. The other corresponds 1 minus the upper bound on probabilities allocated to other
      // values (including *), which we compute by taking the sum of all other upper entries and subtracting the entry
      // for this value. We take whichever bound is greater.
      val lowerProbValue = (valueLower / partitionUpper).max(1.0 - (partitionUpper - valueUpper) / partitionLower)
      // There are two lower bounds on the probability of this value. One corresponds to the probability that could be
      // allocated to this value. The other corresponds 1 minus the lower bound on probabilities allocated to other
      // values (not including *), which we compute by taking the sum of all other lower entries and subtracting the
      // entry for this value and *. We take whichever bound is lesser.
      val upperProbValue = ((valueUpper + starUpper) / partitionLower).min(1.0 - (partitionLower - valueLower - starLower) / partitionUpper)
      // Collect the probability bounds, as well as the function evaluated at this value
      (lowerProbValue, upperProbValue, xvalue.value)
    }
    // The same bounds as above, but for *. These are bounds on the probability mass that could be allocated to values
    // that are not yet computed.
    val lowerProbStar = (starLower / partitionUpper).max(1.0 - (partitionUpper - starUpper) / partitionLower)
    val upperProbStar = (starUpper / partitionLower).min(1.0 - (partitionLower - starLower) / partitionUpper)

    (regularBounds, lowerProbStar, upperProbStar)
  }

  override def computeAllProbabilityBounds[T](target: Element[T]): Stream[(Double, Double, T)] = {
    val (lowerFactor, upperFactor) = boundFactors(target)
    regularAndStarBounds(lowerFactor, upperFactor)._1.toStream
  }

  /**
   * Helper function to compute lower and upper bounds on the given function using the optional bounds given and the
   * regular values of a target variable. If bounds are given, it checks that all regular values are within the desired
   * bounds. Otherwise, it returns the strongest possible bounds given the current regular values. If there are no
   * regular values, it returns (-Infinity, Infinity).
   */
  protected def boundFunction[T](regularValues: Set[T], function: (T) => Double, lower: Option[Double], upper: Option[Double]): (Double, Double) = {
    val lowerBound =
      if(lower.isDefined) {
        // Return the given lower bound and require that every regular value is greater than or equal to it
        val lowerValue = lower.get
        for(rvalue <- regularValues) {
          require(function(rvalue) >= lowerValue, s"regular value out of bounds: ${function(rvalue)} < $lowerValue")
        }
        lowerValue
      }
      // Return -Infinity if there are no regular values
      else if(regularValues.isEmpty) Double.NegativeInfinity
      // Return the least value of the function on the regular values
      else function(regularValues.minBy(function))
    val upperBound =
      if(upper.isDefined) {
        // Return the given upper bound and require that every regular value is less than or equal to it
        val upperValue = upper.get
        for(rvalue <- regularValues) {
          require(function(rvalue) <= upperValue, s"regular value out of bounds: ${function(rvalue)} > $upperValue")
        }
        upperValue
      }
      // Return Infinity if there are no regular values
      else if(regularValues.isEmpty) Double.PositiveInfinity
      // Return the greatest value of the function on the regular values
      else function(regularValues.maxBy(function))
    (lowerBound, upperBound)
  }

  override def computeExpectationBounds[T](target: Element[T], function: (T) => Double,
                                           lower: Option[Double], upper: Option[Double]): (Double, Double) = {
    val (lowerFactor, upperFactor) = boundFactors(target)
    // Both factors should be over the same variable, or they are both empty because the variable has range {*}
    // In either case, this gets the set of regular values of the variable
    val regularValues = lowerFactor.variables.headOption.map(_.valueSet.regularValues).getOrElse(Set())
    // Get the function bounds
    val (actualLower, actualUpper) = boundFunction(regularValues.asInstanceOf[Set[T]], function, lower, upper)
    // Get the probability bounds on regular and star values
    val (regularBounds, lowerProbStar, upperProbStar) = regularAndStarBounds[T](lowerFactor, upperFactor)
    // To compute lower or upper bounds on the expectation of the function, we essentially try to allocate as much
    // probability mass as possible to the values where the function is, respectively, as large or as small as possible.
    // First, we compute the total probability assigned to each value, regular or *.
    val allocatedProb = regularBounds.foldLeft(0.0)(_ + _._1) + lowerProbStar
    // We compute the mass that can be allocated elsewhere
    val maxUnallocatedProb = 1.0 - allocatedProb
    // To allocate the mass as described above, we first sort the bounds by the evaluation of the function
    val regularBoundsByFunctionSorted = regularBounds.map(triple => (triple._1, triple._2, function(triple._3))).sortBy(_._3)
    // The following function greedily allocates as much mass as possible to the first values in the list, given a list
    // of (lower, upper, value) tuples. It then computes the expectation of the resulting distribution by summing the
    // products of probabilities and  function evaluations.
    def allocateInOrder(list: List[(Double, Double, Double)]): Double = {
      // Probability mass not yet allocated
      var totalUnallocatedProb = maxUnallocatedProb
      val products = for((lowerProbValue, upperProbValue, value) <- list) yield {
        // Allocate as much remaining mass as possible to the current value
        if (totalUnallocatedProb > 0.0) {
          val probValue = upperProbValue.min(lowerProbValue + totalUnallocatedProb)
          totalUnallocatedProb -= probValue - lowerProbValue
          probValue * value
        }
        else lowerProbValue * value
      }
      products.sum
    }

    // The expectations of the distributions below are the respective lower and upper bounds on the expectation of the
    // function. For the lower bound, we assign * to actualLower, and proceed in ascending order by the value of the
    // function. For the upper bound, we assign * to actualUpper, and proceed in descending order by the value of the
    // function.
    val lowerBound = allocateInOrder((lowerProbStar, upperProbStar, actualLower) :: regularBoundsByFunctionSorted)
    val upperBound = allocateInOrder((lowerProbStar, upperProbStar, actualUpper) :: regularBoundsByFunctionSorted.reverse)
    (lowerBound, upperBound)
  }
}

trait OneTimeLazyStructuredProbQuery extends LazyStructuredProbQueryAlgorithm
  with OneTimeLazyStructured with OneTimeStructuredProbQuery with OneTimeBoundsProbQuery

trait AnytimeLazyStructuredProbQuery extends LazyStructuredProbQueryAlgorithm
  with AnytimeLazyStructured with AnytimeStructuredProbQuery with AnytimeBoundsProbQuery
