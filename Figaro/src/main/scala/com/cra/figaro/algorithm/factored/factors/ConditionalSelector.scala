package com.cra.figaro.algorithm.factored.factors

import scala.reflect.runtime.universe._

class ConditionalSelector[T: TypeTag](parents: List[Variable[_]], output: List[Variable[_]]) 
  extends SparseFactor[T](parents, output) {

  override def createFactor[T: TypeTag](parents: List[Variable[_]], output: List[Variable[_]]) =
    new SparseFactor[T](parents, output)

  defaultValue = tpe match {
    case t if t =:= typeOf[Double] => 1.0
    case t if t =:= typeOf[Int] => 1
    case t if t =:= typeOf[Boolean] => true
    case t if t =:= typeOf[(Double, Double)] => (1.0, 0.0)
    case _ => /*println("unknown type " + tpe)*/
      1.0
  }
  
  /**
   * List all the indices for this factor
   */
  override def getIndices = {
     generateAllIndices 
  }
}