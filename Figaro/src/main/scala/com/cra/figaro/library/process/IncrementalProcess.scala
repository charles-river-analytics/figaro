package com.cra.figaro.library.process

import com.cra.figaro.language._

trait IncrementalContainer[Index, Value] extends Process[Index, Value] {
  /**
   * Produce the elements representing the value of the process at the given indices.
   * Ensures that any dependencies between the elements are represented.
   * Also return a new container such that when elements for future indices are produced,
   * any dependencies between those elements and the ones for these indices are represented.
   * This method must be implemented by implementations of IncrementalContainer.
   * The first return value maps each provided index to the corresponding element.
   * The second return values is the new container.
   * This method does not assume that the indices have already been range checked.
   */
  protected def generateIncremental(indices: List[Index]): (Map[Index, Element[Value]], Process[Index, Value]) 
}
