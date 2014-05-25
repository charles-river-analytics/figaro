/*
 * Dirichlet.scala
 * Elements representing Dirichlet distributions.
 *
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Jan 1, 2009
 *
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.library.atomic.continuous

import com.cra.figaro.language._
import scala.math.pow
import JSci.maths.SpecialMath.gamma

/**
 * Dirichlet distributions in which the parameters are constants.
 */
class AtomicDirichlet(name: Name[Array[Double]], alphas: Array[Double], collection: ElementCollection)
  extends Element[Array[Double]](name, collection) with Atomic[Array[Double]] {
  type Randomness = Array[Double]

  def generateRandomness(): Array[Double] = {
    val gs = alphas map (Util.generateGamma(_))
    val sum = gs reduceLeft (_ + _)
    gs map (_ / sum)
  }

  def generateValue(rand: Randomness) = rand

  private val sumAlphas = alphas reduceLeft (_ + _)
  private val prodGammas = alphas map (gamma(_)) reduceLeft (_ * _)

  /**
   * The normalizing factor.
   */
  private val normalizer = gamma(sumAlphas) / prodGammas

  private def onePow(xAlpha: (Double, Double)) = pow(xAlpha._1, xAlpha._2 - 1)

  /**
   * Density of a value.
   */
  def density(xs: Array[Double]) =
    (1.0 /: (xs zip alphas))(_ * onePow(_)) * normalizer

  override def toString = "Dirichlet(" + alphas.mkString(", ") + ")"
}

/**
 * Dirichlet distributions in which the parameters are elements.
 */
class CompoundDirichlet(name: Name[Array[Double]], alphas: Array[Element[Double]], collection: ElementCollection)
  extends NonCachingChain[List[Double], Array[Double]](
    name,
    new Inject("", alphas, collection),
    (aa: Seq[Double]) => new AtomicDirichlet("", aa.toArray, collection),
    collection) {
  override def toString = "Dirichlet(" + alphas.mkString(", ") + ")"
}

object Dirichlet extends Creatable {
  /**
   * Create a Dirichlet distribution in which the parameters are constants.
   */
  def apply(alphas: Double*)(implicit name: Name[Array[Double]], collection: ElementCollection) =
    new AtomicDirichlet(name, alphas.toArray, collection)

  /**
   * Create a Dirichlet distribution in which the parameters are elements.
   */
  def apply(alphas: Element[Double]*)(implicit name: Name[Array[Double]], collection: ElementCollection) =
    new CompoundDirichlet(name, alphas.toArray, collection)

  type ResultType = Array[Double]

  def create(args: List[Element[_]]) = apply(args.map(_.asInstanceOf[Element[Double]]): _*)
}
