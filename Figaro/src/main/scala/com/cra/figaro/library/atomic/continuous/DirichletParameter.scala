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

object DirichletParameter extends Creatable {

  /**
   * Create a Dirichlet parameter in which the parameters are constants.
   * 
   * @deprecated
   */
  def apply(alphas: Double*)(implicit name: Name[Array[Double]], collection: ElementCollection) =
    new AtomicDirichlet(name, alphas.toArray, collection)

  type ResultType = Array[Double]
  
  def create(args: List[Element[_]]) = apply(args.map(_.asInstanceOf[Double]): _*)
}
