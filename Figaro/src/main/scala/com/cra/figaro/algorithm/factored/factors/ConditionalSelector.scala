/*
 * ConditionalSelector.scala
 * Implementation of conditional selector (in chain) using sparse
 * factors.
 * 
 * Created By:      Glenn Takata (gtakata@cra.com)
 * Creation Date:   Feb 20, 2015
 * 
 * Copyright 2015 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */package com.cra.figaro.algorithm.factored.factors

import scala.reflect.runtime.universe._

/**
 * ConditionalSelector Factor. A conditional selector factor is associated chain elements and represents the relationship
 * between parent elements and outcome elements. It is a special form of sparse factor where the default element is not 
 * 0 but 1 (don't care) which is the most frequent in this type of factor.
 * 
 * @author Glenn Takata Feb 20, 2015
 *
 * @param <T>
 */
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
 override def getIndices = new Indices(variables)

}