/*
 * Inject.scala
 * Element that converts a sequence of elements into an element over sequences.
 *
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Jan 1, 2009
 *
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.language

import com.cra.figaro.algorithm._
import com.cra.figaro.util._

/**
 * Element that converts a sequence of elements into an element over sequences.
 *
 * @param xs The sequence of elements to be converted.
 */
class Inject[T](name: Name[List[T]], val xs: Seq[Element[T]], collection: ElementCollection)
  extends Deterministic[List[T]](name, collection) with IfArgsCacheable[List[T]] {
  /**
   * The type over which the sequence is defined.
   */
  type BaseType = T

  def args = xs.toList

  def generateValue() = {
    xs foreach (x => if (x.value == null) x.generate())
    (xs map (_.value)).toList
  }

  override def toString = "Inject(" + xs.mkString(", ") + ")"
}

object Inject {
  /**
   * Element that converts a sequence of elements into an element over sequences.
   */
  def apply[T](xs: Element[T]*)(implicit name: Name[List[T]], collection: ElementCollection) =
    new Inject(name, xs.toList, collection)
}
