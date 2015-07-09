/*
 * ResultsData.scala 
 * Trait and classes representing data input by the user. Includes discrete (distribution List) and continuous (element)
 * 
 * Created By:      Glenn Takata (gtakata@cra.com)
 * Creation Date:   Apr 9, 2015
 * 
 * Copyright 2015 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */
package com.cra.figaro.util.visualization.results

import org.apache.commons.math3.distribution._
import scala.collection.mutable.{ ListBuffer }
import com.cra.figaro.language.{ Atomic, Element, HasDensity }
import com.cra.figaro.library.atomic.continuous.{ AtomicExponential, AtomicNormal, AtomicUniform }

/**
 * @author Glenn Takata (gtakata@comcast.net)
 */
trait ResultsData {
  val name: String
  def resultString: String
  def distribution: List[(Double, _)]
}

case class DiscreteData[T](override val name: String, override val distribution: List[(Double, T)]) extends ResultsData {
  override def resultString: String = {
    val buffer = new StringBuffer("{")
    for ((prob, value) <- distribution) {
      buffer.append("(").append("%06.4f".format(prob)).append("->").append(value).append(")")
    }
    buffer.append("}")
    return buffer.toString()
  }
}

case class ContinuousData[T](override val name: String, val element: Element[_]) extends ResultsData {
  val intervals = 200
  
  override def resultString: String = {
    if (element.isInstanceOf[AtomicNormal]) {
      val normal = element.asInstanceOf[AtomicNormal]
      val mean = normal.mean
      val variance = normal.variance
      s"Normal($mean, $variance)"
    } else if (element.isInstanceOf[AtomicUniform]) {
      val uniform = element.asInstanceOf[AtomicUniform]
      val lower = uniform.lower
      val upper = uniform.upper
      s"Uniform($lower, $upper)"
    } else if (element.isInstanceOf[AtomicExponential]) {
      val exponential = element.asInstanceOf[AtomicExponential]
      val lambda = exponential.lambda
      s"Exponential($lambda)"
    } else {
      ""
    }
  }

  override def distribution: List[(Double, Double)] = {
    if (element.isInstanceOf[AtomicNormal]) {
      val normal = element.asInstanceOf[AtomicNormal]
      val min = normal.mean - 3 * normal.standardDeviation
      val max = normal.mean + 3 * normal.standardDeviation
      constructDistribution(new NormalDistribution(normal.mean, normal.standardDeviation), min, max)
    } else if (element.isInstanceOf[AtomicUniform]) {
      val uniform = element.asInstanceOf[AtomicUniform]
      val min = uniform.lower
      val max = uniform.upper
      constructDistribution(new UniformRealDistribution(min, max), min, max)
    } else if (element.isInstanceOf[AtomicExponential]) {
      val exponential = element.asInstanceOf[AtomicExponential]
      val min = 0
      val max = 3.0 / exponential.lambda
      constructDistribution(new ExponentialDistribution(1 / exponential.lambda), min, max)
    } else {
      val buffer = new ListBuffer[(Double, Double)]()
      buffer.toList
    }
  }

  private def constructDistribution(function: AbstractRealDistribution, min: Double, max: Double): List[(Double, Double)] = {
    val buffer = new ListBuffer[(Double, Double)]()
    val spacing = (max - min) / intervals
    var previous = min
    for (x <- 1 until intervals) {
      val value = min + x * spacing
      val density = function.density(value)
      buffer.append((density, value))
      previous = value
    }
    buffer.toList
  }
}