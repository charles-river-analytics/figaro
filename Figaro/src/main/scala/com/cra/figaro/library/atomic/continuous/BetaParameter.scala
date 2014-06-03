/*
 * BetaParameter.scala
 * Elements representing learnable Beta parameters.
 *
 * Created By:      Michael Howard (mhoward@cra.com)
 * Creation Date:   Jun 6, 2013
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

object BetaParameter extends Creatable {

  /**
   * Create a beta parameter with prior hyperparameters a and b
   * 
   * @depracated
   */
  def apply(a: Double, b: Double)(implicit name: Name[Double], collection: ElementCollection) =
    new AtomicBeta(name, a, b, collection)

  type ResultType = Double

  def create(args: List[Element[_]]) = apply(args(0).asInstanceOf[Double], args(1).asInstanceOf[Double])
}
