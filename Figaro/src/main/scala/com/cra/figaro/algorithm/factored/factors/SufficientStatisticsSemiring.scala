/*
 * SufficientStatisticsSemiring.scala
 * Sum and product operations defined for sufficient statistics according to a semiring algebraic structure.
 * 
 * Created By:      Michael Howard (mhoward@cra.com)
 * Creation Date:   Jun 6, 2013
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.algorithm.factored.factors

import com.cra.figaro.language._
import scala.collection._
import scala.collection.mutable.{ Set, Map }

/**
 * Sum and product operations defined for sufficient statistics. 
 * Statistics consist of a probability and counts of the number of times various values have been seen.
 *  
 *  @param parameterMap Map of parameters to their sufficient statistics. Expectation 
 *  Maximization determines the parameterMap automatically from the parameters. 
 */
class SufficientStatisticsSemiring(parameterMap: immutable.Map[Parameter[_], Seq[Double]])
  extends Semiring[(Double, mutable.Map[Parameter[_], Seq[Double]])] {
  
  /**
   * 0 probability and a vector of zeros for all parameters. The vector for a parameter
   * must be of length equal to number of possible observations of the parameter.
   */
  val zero = (0.0, mutable.Map(parameterMap.toSeq: _*))
  
  /**
   * 1 probability and a vector of zeros for all parameters. The vector for a parameter
   * must be of length equal to number of possible observations of the parameter.
   */
  val one = (1.0, mutable.Map(parameterMap.toSeq: _*))

  /**
   * Probabilities are multiplied using standard multiplication. 
   * Sufficient statistics for each parameter are summed together.
   */
  def product(xVector: (Double, Map[Parameter[_], Seq[Double]]), yVector: (Double, Map[Parameter[_], Seq[Double]])): (Double, Map[Parameter[_], Seq[Double]]) = {	  
    (simpleProduct(xVector._1, yVector._1), mapProduct(xVector, yVector))
  }

  private def componentProduct(xVector: Seq[Double], yVector: Seq[Double]): Seq[Double] = {
    require(xVector.size == yVector.size)
    (for ((x, y) <- xVector zip yVector) yield x * y)
  }

  /**
   * Probabilities are added using standard addition.
   * Sufficient statistics for each parameter are weighted by their respective probabilities and summed together, 
   * then divided by the sum of both probabilities.
   */
  def sum(xVector: (Double, Map[Parameter[_], Seq[Double]]), yVector: (Double, Map[Parameter[_], Seq[Double]])): (Double, Map[Parameter[_], Seq[Double]]) = {
    (simpleSum(xVector._1, yVector._1), mapSum(xVector, yVector))
  }

  private def mapSum(xVector: (Double, Map[Parameter[_], Seq[Double]]), yVector: (Double, Map[Parameter[_], Seq[Double]])): Map[Parameter[_], Seq[Double]] = {
    require(xVector._2.size == yVector._2.size)
    val result: Map[Parameter[_], Seq[Double]] = Map()
    for (x <- xVector._2.keys) {
      result += x -> weightedComponentSum(xVector._2(x), yVector._2(x), xVector._1, yVector._1)
    }
    result
  }

  private def mapProduct(xVector: (Double, Map[Parameter[_], Seq[Double]]), yVector: (Double, Map[Parameter[_], Seq[Double]])): Map[Parameter[_], Seq[Double]] = {
    val result: Map[Parameter[_], Seq[Double]] = Map()
    require(xVector._2.size == yVector._2.size)
    for (x <- xVector._2.keys) {
      result += x -> simpleComponentSum(xVector._2(x), yVector._2(x))
    }
    result

  }

  private def simpleComponentSum(xVector: Seq[Double], yVector: Seq[Double]): Seq[Double] = {
    require(xVector.size == yVector.size)
    (for ((x, y) <- xVector zip yVector) yield x + y)
  }

  private def weightedComponentSum(xVector: Seq[Double], yVector: Seq[Double], xProb: Double, yProb: Double): Seq[Double] = {
    require(xVector.size == yVector.size)

    val divisor = xProb + yProb
    if (divisor > 0) {
      (for ((x, y) <- xVector zip yVector) yield (xProb * x + yProb * y) / divisor)
    } else {
      (for ((x, y) <- xVector zip yVector) yield 0.0)
    }

  }

  /*
   * Usual multiplication.
   */
 private def simpleProduct(x: Double, y: Double): Double = {
    x * y
  }

  /*
   * Usual addition.
   */
  private def simpleSum(x: Double, y: Double): Double = {
    x + y
  }
}

object SufficientStatisticsSemiring
{
	def apply(parameterMap : immutable.Map[Parameter[_], Seq[Double]]) = new SufficientStatisticsSemiring(parameterMap)
}
