/*
 * StarFactory.scala
 * Description needed
 * 
 * Created By:      Glenn Takata (gtakata@cra.com)
 * Creation Date:   Dec 15, 2014
 * 
 * Copyright 2014 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.algorithm.factored.factors.factory

import com.cra.figaro.language._
import com.cra.figaro.algorithm.factored.factors._
import com.cra.figaro.algorithm.lazyfactored._

/**
 * A Sub-Factory to make Star Factors from arbitrary elements
 */
object StarFactory {
  
  /**
   * Make a StarFactor from an Element <p.
   * 
   * This Factor has only one value whose probability is 1.0
   */
   def makeStarFactor[T](elem: Element[T]): List[Factor[Double]] = {
    val elemVar = Variable(elem)
    require(elemVar.range.size == 1 && elemVar.range(0) == Star[T], "Trying to create a star factor from a value set that is not only star")
    val factor = new BasicFactor[Double](List(), List(elemVar))
    factor.set(List(0), 1.0)
    List(factor)
  }
}