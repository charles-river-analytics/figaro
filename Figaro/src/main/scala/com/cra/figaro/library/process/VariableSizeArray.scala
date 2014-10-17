/*
 * 
package com.cra.figaro.library.process

import com.cra.figaro.language._

object VariableSizeArray {
  def apply[Value](numItems: Element[Int], generator: Int => Element[Value]) = 
    new FiniteContainerElement[Int, Value](
      Apply(numItems, (size: Int) => new FixedIndependentArray(size, generator))
    )
}
 
*/ 