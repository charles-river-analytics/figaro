package com.cra.figaro.library.process

import com.cra.figaro.language._

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

