/*
 * ConditionalSelector.scala   
 * Needs description
 * 
 * Created By:      Glenn Takata (gtakata@cra.com)
 * Creation Date:   Feb 26, 2015
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

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