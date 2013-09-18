/*
 * BetaParameter.scala
 * Elements representing learnable Beta parameters.
 *
 * Created By:      Michael Howard (mhoward@cra.com)
 * Creation Date:   Jun 6, 2013
 *
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 */

package com.cra.figaro.library.atomic.continuous

import com.cra.figaro.algorithm._
import com.cra.figaro.language._

/**
 * A learnable beta distribution, which can be learned by observing elements which use it as a parameter.
 * The prior value for alpha and beta are given by the 'a' and 'b' arguments.
 */
class AtomicBetaParameter(name: Name[Double], a: Double, b: Double, collection: ElementCollection)
  extends AtomicBeta(name, a, b, collection) with Parameter[Double] with ValuesMaker[Double] {
  
  /**
   * The learned alpha parameter
   */
  var alpha = a
  /**
   * The learned beta parameter
   */
  var beta = b
  
  /**
   * Returns an empty sufficient statistics vector. 
   */
  override def zeroSufficientStatistics(): Seq[Double] = {
      Seq(0.0, 0.0)
    }

  /**
   * Returns an element that models the learned distribution.
   */
  def getLearnedElement: AtomicFlip = {
      new AtomicFlip("", expectedValue, collection)
    }

  override def sufficientStatistics[Boolean](b: Boolean): Seq[Double] = {
    if (b == true) {
      Seq(1.0, 0.0)
    } else {
      Seq(0.0, 1.0)
    }
  }
  
  private[figaro] override def sufficientStatistics[Boolean](i: Int): Seq[Double] = {
    if (i == 0) {
      Seq(1.0, 0.0)
    } else {
      Seq(0.0, 1.0)
    }
  }


  def expectedValue: Double = {
    (alpha) / (alpha + beta)
  }

  def makeValues = List(expectedValue)

  def maximize(sufficientStatistics: Seq[Double]) {

    require(sufficientStatistics.size == 2)
    alpha = sufficientStatistics(0) + a
    beta = sufficientStatistics(1) + b

  }

  override def toString = "BetaParameter(" + (alpha) + ", " + (beta) + ")"
}

object BetaParameter extends Creatable {

  /**
   * Create a beta parameter with prior hyperparameters a and b
   */
  def apply(a: Double, b: Double)(implicit name: Name[Double], collection: ElementCollection) =
    new AtomicBetaParameter(name, a, b, collection)

  type ResultType = Double

  def create(args: List[Element[_]]) = apply(args(0).asInstanceOf[Double], args(1).asInstanceOf[Double])
}