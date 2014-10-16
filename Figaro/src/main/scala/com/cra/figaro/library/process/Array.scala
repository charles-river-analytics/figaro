package com.cra.figaro.library.process

import com.cra.figaro.language._

abstract class FixedArray[Value](size: Int)  
extends Container[Int, Value] {
  val indices = 0 until size

  override def rangeCheck(index: Int) = index >= 0 && index < size
}

class FixedIndependentArray[Value](
    val size: Int,
    val generator: Int => Element[Value],
    override val name: Name[_] = "",
    override val collection: ElementCollection = Universe.universe 
) extends FixedArray[Value](size) with IndependentProcess[Int, Value]

