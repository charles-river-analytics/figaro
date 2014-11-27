/**
 *
 */
package com.cra.figaro.library.factors.factory

import com.cra.figaro.language._
import com.cra.figaro.library.factors._
import com.cra.figaro.algorithm.lazyfactored._

/**
 * @author gtakata
 *
 */
object StarFactory {
   def makeStarFactor[T](elem: Element[T]): List[Factor[Double]] = {
    val elemVar = Variable(elem)
    require(elemVar.range.size == 1 && elemVar.range(0) == Star[T], "Trying to create a star factor from a value set that is not only star")
    val factor = new BasicFactor[Double](List(), List(elemVar))
    factor.set(List(0), 1.0)
    List(factor)
  }
}