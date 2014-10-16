/*

package com.cra.figaro.library.process

import com.cra.figaro.language._

class ContainerElement[Index, Value](val element: Element[Container[Index, Value]]) {
  /**
   * Creates an element whose value is the value at the corresponding index in the value of the container element.
   */
  def get(i: Index): Element[Option[Value]] = {
    Chain(element, (p: Container[Index, Value]) => p.get(i))
  }
  
  /**
   * Map the given function pointwise through the value of the container element.
   */
  def map[Value2](f: Value => Value2): ContainerElement[Index, Value2] = {
    new ContainerElement(Apply(element, (p: Container[Index, Value]) => p.map(f)))
  }
  
  /**
   * Chain the given function pointwise through the value of the container element.
   */
  def chain[Value2](f: Value => Element[Value2]): ContainerElement[Index, Value2] = {
    new ContainerElement(Apply(element, (p: Container[Index, Value]) => p.chain(f)))
  }
}

*/