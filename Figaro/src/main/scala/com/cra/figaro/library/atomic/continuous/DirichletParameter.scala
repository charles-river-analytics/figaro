/*
 * DirichletParameter.scala
 * Elements representing learnable Dirichlet parameters.
 *
 * Created By:      Michael Howard (mhoward@cra.com)
 * Creation Date:   Jul 17, 2013
 *
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.library.atomic.continuous

import com.cra.figaro.algorithm.ValuesMaker
import com.cra.figaro.algorithm.lazyfactored.ValueSet
import com.cra.figaro.language._
import scala.Array.canBuildFrom
import scala.collection.mutable

/**
 * A Dirichlet distribution which can be learned by observing elements which use it as a parameter.
 * 
 * @param alphas The prior concentration parameters
 */
class AtomicDirichletParameter(name: Name[Array[Double]], val alphas: Array[Double], collection: ElementCollection)
  extends AtomicDirichlet(name, alphas, collection) with Parameter[Array[Double]] with ValuesMaker[Array[Double]] {

  /**
   * The learned concentration parameters of the Dirichlet distribution
   */
  var concentrationParameters: mutable.Seq[Double] = mutable.Seq(alphas: _*)

  /**
   * Returns an element that models the learned distribution.
   */
  def getLearnedElement[T](outcomes: List[T]): AtomicSelect[T] = {
    new AtomicSelect("", expectedValue.toList zip outcomes, collection)
  }
  
  def maximize(sufficientStatistics: Seq[Double]) = {
    require(sufficientStatistics.size == concentrationParameters.size)

    for (i <- sufficientStatistics.indices) {
      concentrationParameters(i) = sufficientStatistics(i) + alphas(i)
    }

  }

  private val vector = alphas.map(a => 0.0)

  /**
   * The number of concentration parameters in the Dirichlet distribution.
   */
  val size = alphas.size
  
  private[figaro] override def sufficientStatistics[A](i: Int): Seq[Double] = {
    val result = vector
    require(i < result.size)
    result.update(i, 1.0)
    result
  }

  override def sufficientStatistics[A](a: A): Seq[Double] = {
    val result = vector
    result
  }

  override def zeroSufficientStatistics: Seq[Double] = {
    val result = vector
    result
  }

  override def expectedValue: Array[Double] = {

    val sumObservedAlphas = concentrationParameters reduceLeft (_ + _)
    val result = new Array[Double](concentrationParameters.size)

    concentrationParameters.zipWithIndex.foreach {
      case (v, i) => {
          result(i) = (v) / (sumObservedAlphas)
        }
    }

    result

  }

  def makeValues(depth: Int) = ValueSet.withoutStar(Set(expectedValue))

  override def toString = "Dirichlet Parameter(" + concentrationParameters.mkString(", ") + ")"

}

object DirichletParameter extends Creatable {

  /**
   * Create a Dirichlet parameter in which the parameters are constants.
   */
  def apply(alphas: Double*)(implicit name: Name[Array[Double]], collection: ElementCollection) =
    new AtomicDirichletParameter(name, alphas.toArray, collection)

  type ResultType = Array[Double]
  
  def create(args: List[Element[_]]) = apply(args.map(_.asInstanceOf[Double]): _*)
}
