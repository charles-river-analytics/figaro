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

package com.cra.figaro.library.collection

import com.cra.figaro.language._

/**
 * A Figaro collection representing a fixed number of elements. The indices into the collection are the integers
 * from 0 to the size - 1.
 *
 * @param size the number of elements in the collection
 * @param generator a function to generate the elements in the collection, given the index
 */
class FixedSizeArray[Value](
    val size: Int,
    val generator: Int => Element[Value]
) extends Container[Int, Value] {
  val indices = 0 until size

  override def rangeCheck(index: Int) = index >= 0 && index < size

  def generate(index: Int) = generator(index)

  def generate(indices: List[Int]) = {
    Map(indices.map(index => (index, generator(index))):_*)
  }

  /**
   * Concatenate this container with another one. In the result, all the elements in this container will precede all
   * the elements in the other container. The result is an array containing all the elements of both containers.
   */
  def concat[Index2](that: FixedSizeArray[Value]) = {
    new FixedSizeArray(
      this.indices.size + that.indices.size,
      (i: Int) => if (i < indices.size) this(i) else that(i - indices.size)
    )
  }
}
