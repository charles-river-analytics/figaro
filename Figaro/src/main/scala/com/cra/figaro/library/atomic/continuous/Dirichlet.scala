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

import scala.collection.mutable
import scala.math.log
import scala.math.pow

import com.cra.figaro.algorithm.ValuesMaker
import com.cra.figaro.algorithm.lazyfactored.ValueSet
import com.cra.figaro.language._
import com.cra.figaro.util._

import JSci.maths.SpecialMath._
import argonaut._
import argonaut.Argonaut._
/**
 * Dirichlet distributions in which the parameters are constants.
 * These Dirichlet elements can also serve as parameters for ParameterizedSelect.
 *
 * @param alphas the prior concentration parameters
 */
class AtomicDirichlet(name: Name[Array[Double]], val alphas: Array[Double], collection: ElementCollection)
  extends Element[Array[Double]](name, collection) with Atomic[Array[Double]] with ArrayParameter with ValuesMaker[Array[Double]] with Dirichlet{

  /**
   * The number of concentration parameters in the Dirichlet distribution.
   */
  val size = alphas.size
  def alphaValues: Array[Double] = concentrationParameters.toArray
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

  /**
   * The learned concentration parameters of the Dirichlet distribution
   */
  var concentrationParameters: mutable.Seq[Double] = mutable.Seq(alphas: _*)

  /**
   * Returns an element that models the learned distribution.
   *
   * @deprecated
   */
  def getLearnedElement[T](outcomes: List[T]): AtomicSelect[T] = {
    new AtomicSelect("", MAPValue.toList zip outcomes, collection)
  }

  def maximize(sufficientStatistics: Seq[Double]) = {
    require(sufficientStatistics.size == concentrationParameters.size)
    for (i <- sufficientStatistics.indices) {
      concentrationParameters(i) = sufficientStatistics(i) + alphas(i)
    }
  }

  private val vector = alphas.map(a => 0.0)

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
    val result = new Array[Double](size)

    concentrationParameters.zipWithIndex.foreach {
      case (v, i) => {
        result(i) = (v) / (sumObservedAlphas)
      }
    }

    result

  }

  override def MAPValue: Array[Double] = {
    val sumObservedAlphas = concentrationParameters reduceLeft (_ + _)
    val result = new Array[Double](size)

    concentrationParameters.zipWithIndex.foreach {
      case (v, i) => {
        result(i) =
          if (sumObservedAlphas == size) 1.0 / size
          else (v - 1) / (sumObservedAlphas - size)
      }
    }
    result
  }

  def makeValues(depth: Int) = ValueSet.withoutStar(Set(MAPValue))

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
    collection)
  with Dirichlet {

  def alphaValues = alphas.map(_.value)

  override def toString = "Dirichlet(" + alphas.mkString(", ") + ")"
}

trait Dirichlet extends Continuous[Array[Double]] {

  /**
   * Current alpha values.
   */
  def alphaValues: Array[Double]

  private def sumAlphasLogGamma = logGamma(alphaValues.sum)
  private def prodGammasLog = alphaValues.map(logGamma).sum

  /**
   * The normalizing factor.
   */
  private def normalizer = sumAlphasLogGamma - prodGammasLog

  def logp(values: Array[Double]) =
    bound(
      alphaValues.zip(values).map { v =>
        val (a, x) = v
        (a - 1) * log(x)
      }.sum + normalizer,
      alphaValues.map(_ > 0): _*)

}

object Dirichlet extends Creatable {

  //Needs to be a nested field or a jEmptyArray
  implicit def DirichletEncodeJson: EncodeJson[Dirichlet] = EncodeJson((d: Dirichlet) =>
  ("name" := d.name.string) ->: ("alphaValues" := jArray((for (a <- d.alphaValues) yield { jNumber(a) }).toList)) ->: jEmptyObject)

  implicit def DirichletDecodeJson(implicit collection: ElementCollection): DecodeJson[AtomicDirichlet] =
    DecodeJson(c => for {
      alphaValues <- (c --\ "alphaValues").as[List[Double]]
      name <- (c --\ "name").as[String]
    } yield Dirichlet(alphaValues.toArray)(name, collection))

  /**
   * Create a Dirichlet distribution in which the parameters are constants.
   */
  def apply(alphas: Double*)(implicit name: Name[Array[Double]], collection: ElementCollection) =
    new AtomicDirichlet(name, alphas.toArray, collection)
  
  def apply(alphas: Array[Double])(implicit name: Name[Array[Double]], collection: ElementCollection) =
    new AtomicDirichlet(name, alphas, collection)
  
  /**
   * Create a Dirichlet distribution in which the parameters are elements.
   */
  def apply(alphas: Element[Double]*)(implicit name: Name[Array[Double]], collection: ElementCollection) =
    new CompoundDirichlet(name, alphas.toArray, collection)
  


  type ResultType = Array[Double]

  def create(args: List[Element[_]]) = apply(args.map(_.asInstanceOf[Element[Double]]): _*)
}
