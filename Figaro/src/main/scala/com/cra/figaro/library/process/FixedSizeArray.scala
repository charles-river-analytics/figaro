/*
 * FixedSizeArray.scala
 * Class for a fixed size array
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Nov 27, 2014
 * 
 * Copyright 2014 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.library.process

import com.cra.figaro.language._

/**
 * Doc needed
 */
class FixedSizeArray[Value](
    val size: Int,
    val generator: Int => Element[Value],
    override val name: Name[_] = "",
    override val collection: ElementCollection = Universe.universe
) extends Container[Int, Value] {
  val indices = 0 until size

  override def rangeCheck(index: Int) = index >= 0 && index < size

  def generate(index: Int) = generator(index)

  def generate(indices: List[Int]) = {
    Map(indices.map(index => (index, generator(index))):_*)
  }
}

