/*
 * Abstraction.scala
 * Abstractions of elements to a smaller set of values.
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Jan 1, 2009
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.algorithm

import com.cra.figaro.algorithm.factored._
import com.cra.figaro.language._
import scala.language.postfixOps

/**
 * Algorithms can be instructed to use an abstraction scheme to reason about an element (see
 * AbstractionScheme.scala). This is achieved by adding an Abstraction pragma to the element. This pragma
 * can be created most simply by calling Abstraction(numAbstractPoints), which specifies the desired number
 * of abstract points for the abstraction. If an implicit abstraction scheme has been defined for the type
 * of element, it will be used implicitly here. Otherwise, to specify that scheme should be used, call
 * Abstraction(numAbstractPoints)(scheme).
 *
 * Abstraction also takes an optional second argument, which is numConcretePointsPerAbstractPoint. This
 * argument is used to generate the sample concrete points from which the abstract points are selected, in
 * case the full set of concrete points cannot be generated (e.g., for continuous elements). The value of
 * this argument is multiplied by the number of abstract points to determine the total number of concrete
 * points. The default value for this argument is 10. This is indicated by attaching an Abstraction to the
 * element.
 */

class Abstraction[T](val numAbstractPoints: Int,
  val numConcretePointsPerAbstractPoint: Int = 10,
  val scheme: AbstractionScheme[T]) extends Pragma[T]

/**
 * Constructors for Abstraction
 */
object Abstraction {
  /**
   * @param numAbstractPoints > 0
   * @param numConcretePointsPerAbstractPoint > 0
   */
  def apply[T](numAbstractPoints: Int, numConcretePointsPerAbstractPoint: Int = 10)(implicit scheme: AbstractionScheme[T]): Abstraction[T] =
    new Abstraction(numAbstractPoints, numConcretePointsPerAbstractPoint, scheme)

  /*
   * Determine the abstraction associated with an element from its list of pragmas.
   */
  private[figaro] def fromPragmas[T](pragmas: List[Pragma[T]]): Option[Abstraction[T]] =
    pragmas collect { case a: Abstraction[_] => a } headOption
}
