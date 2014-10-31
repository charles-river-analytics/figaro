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
//Double, array[double], (or matrix[double]
//Or element[double], element[array[double]]


abstract class ParameterType 
case class PrimitiveDouble(val d: Double) extends ParameterType
case class ParameterDouble(val p: Parameter[Double]) extends ParameterType 
case class PrimitiveArray(val a: Array[Double]) extends ParameterType
case class ParameterArray(val p: Parameter[Array[Double]]) extends ParameterType

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


abstract class ParameterCollection {
  def get(s: String): ParameterType
}

class ModelParameters extends ElementCollection {

  override def add[T](element: Element[T]) = {
    element match {
      case p: Parameter[T] => super.add(p)
    }
  }
  
  
  def convertToParameterList: List[Parameter[_]] = {
    val l = ListBuffer.empty[Parameter[_]]
    for (p <- this.namedElements) {
      p match {
        case a: Parameter[_] => {
           l += a 
        }
        case _ => {
          
        }
      }
    }
    l.toList
  }
  
  
  private class PriorParameterCollection extends ParameterCollection {
    def get(s: String): ParameterType = {
      val p = ModelParameters.super.getElementByReference(s)
      val result = p match {
        case p: Parameter[_] => ParameterType(p)
      }
      result
    }
  }
  
  private class PosteriorParameterCollection extends ParameterCollection {
    def get(s: String): ParameterType = {      
      val p = ModelParameters.super.getElementByReference(s)
      val result = p match {//Kind of lame to handle every parameter individually...
        case x: Parameter[_] => { 
          x match {
            case d: DoubleParameter => ParameterType(d.MAPValue)
            case a: ArrayParameter => ParameterType(a.MAPValue)
          }
        }
      }
      result   
    }
  }

  def priorParameters: ParameterCollection = {
    val p = new PriorParameterCollection()
    p
  }

  def posteriorParameters: ParameterCollection = {
    val p = new PosteriorParameterCollection()
    p
  }

}

object ModelParameters {
  def apply() = new ModelParameters()
}