/*
 * ModelParameters.scala
 * Collections for defining prior and posterior parameters.
 * 
 * Created By:      Michael Howard (mhoward@cra.com)
 * Creation Date:   Oct 29, 2014
 * 
 * Copyright 2014 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.patterns.learning

import com.cra.figaro.language.Element
import com.cra.figaro.language.ElementCollection
import com.cra.figaro.language.Reference.stringToReference
import com.cra.figaro.library.atomic.continuous.AtomicBeta
import com.cra.figaro.library.atomic.continuous.AtomicDirichlet
import scala.collection.mutable.ListBuffer
import com.cra.figaro.language.DoubleParameter
import com.cra.figaro.language.ArrayParameter
import com.cra.figaro.language.Parameter

/**
 * Case classes defining type parameters of parameter elements.
 * These are used for matching on return types of parameter collections, and for
 * correctly instantiating elements from the posterior and prior parameters.
 */
abstract class ParameterType
/**
 * Parameters whose MAP value is a double
 */
case class PrimitiveDouble(val d: Double) extends ParameterType {
  override def toString = d.toString()
}
/**
 * Learnable parameters whose MAP value is a double
 */
case class ParameterDouble(val p: Parameter[Double]) extends ParameterType {
  override def toString = p.toString()
}
/**
 * Parameters whose MAP value is an array of doubles
 */
case class PrimitiveArray(val a: Array[Double]) extends ParameterType {
  override def toString = a.toString()
}
/**
 * Learnable parameters whose MAP value is an array of doubles
 */
case class ParameterArray(val p: Parameter[Array[Double]]) extends ParameterType {
  override def toString = p.toString()
}

object ParameterType {
  def apply(d: Double) = new PrimitiveDouble(d)
  def apply(p: Parameter[_]) = {
    p match {
      case b: AtomicBeta => new ParameterDouble(b)
      case d: AtomicDirichlet => new ParameterArray(d)
    }
  }
  def apply(a: Array[Double]) = new PrimitiveArray(a)
}

/**
 * Defines a collection of prior or posterior parameters, obtained from a set of ModelParameters
 */
trait ParameterCollection {
  def get(s: String): ParameterType
}

/**
 * A class representing the prior and posterior parameters of a model
 */
class ModelParameters extends ElementCollection {

  /**
   * Add a parameter to the collection
   */
  override def add[T](element: Element[T]) = {
    element match {
      case p: Parameter[T] => super.add(p)
      case default => {
        //Do not add non-parameter elements to the set of model parameters.
      }
    }
  }

  /**
   * Convert the contents of to a list of parameter elements
   */
  def convertToParameterList: List[Parameter[_]] = {
    val l = ListBuffer.empty[Parameter[_]]
    for (p <- this.namedElements) {
      p match {
        case a: Parameter[_] => {
          l += a
        }
        case default => {
          //Do not add non-parameter elements to the result list. This should never be reached.
        }
      }
    }
    l.toList
  }

  private object PriorParameterCollection extends ParameterCollection {
    def get(s: String): ParameterType = {
      val p = getElementByReference(s)
      val result = p match {
        case p: Parameter[_] => ParameterType(p)
        case default => throw new IllegalArgumentException("Cannot retrieve non-parameter elements from parameter collection.")
      }
      result
    }

    def apply(s: String): ParameterType = {
      val p = getElementByReference(s)
      val result = p match {
        case p: Parameter[_] => ParameterType(p)
        case default => throw new IllegalArgumentException("Cannot retrieve non-parameter elements from parameter collection.")
      }
      result
    }
  }

  private object PosteriorParameterCollection extends ParameterCollection {
    def get(s: String): ParameterType = {
      val p = getElementByReference(s)
      val result = p match {
        case x: Parameter[_] => {
          x match {
            case d: DoubleParameter => ParameterType(d.MAPValue)
            case a: ArrayParameter => ParameterType(a.MAPValue)
            case default => throw new IllegalArgumentException("Cannot retrieve non-parameter elements from parameter collection.")
          }
        }
      }
      result
    }

    def apply(s: String): ParameterType = {
      val p = getElementByReference(s)
      val result = p match {
        case x: Parameter[_] => {
          x match {
            case d: DoubleParameter => ParameterType(d.MAPValue)
            case a: ArrayParameter => ParameterType(a.MAPValue)
            case default => throw new IllegalArgumentException("Cannot retrieve non-parameter elements from parameter collection.")
          }
        }
      }
      result
    }
  }

  /**
   * Get the collection of prior parameters
   */
  def priorParameters: ParameterCollection = {
    val p = PriorParameterCollection
    p
  }

  /**
   * Get the collection of posterior parameters (after learning)
   */
  def posteriorParameters: ParameterCollection = {
    val p = PosteriorParameterCollection
    p
  }

}

object ModelParameters {
  /**
   * Create a new set of model parameters.
   */
  def apply() = new ModelParameters()
}