/*

package com.cra.figaro.library.process

import com.cra.figaro.language._

class FiniteContainerElement[Index, Value](val element: Element[FiniteContainer[Index, Value]]) 
{
  /**
   * Creates an element whose value is the value at the corresponding index in the value of the container element,
   * if the index is in range, None otherwise.
   */
  def get(i: Index): Element[Option[Value]] = {
    Chain(element, (p: Container[Index, Value]) => p.get(i))
  }
  
  /**
   * Map the given function pointwise through the value of the container element.
   */
  def map[Value2](f: Value => Value2): FiniteContainerElement[Index, Value2] = {
    new FiniteContainerElement(Apply(element, (p: FiniteContainer[Index, Value]) => p.map(f)))
  }
  
  /**
   * Chain the given function pointwise through the value of the container element.
   */
  def chain[Value2](f: Value => Element[Value2]): FiniteContainerElement[Index, Value2] = {
    new FiniteContainerElement(Apply(element, (p: FiniteContainer[Index, Value]) => p.chain(f)))
  }
  
  /**
   * Produce the element over values obtained by selecting a particular container and folding through its values.
   */
  def foldLeft[Value2](start: Value2, f: (Value2, Value) => Value2): Element[Value2] = {
    CachingChain(element, (p: FiniteContainer[Index, Value]) => p.foldLeft(start, f))
  }

  /**
   * Produce the element over values obtained by selecting a particular container and folding through its values.
   */
  def foldRight[Value2](start: Value2, f: (Value, Value2) => Value2): Element[Value2] = {
    CachingChain(element, (p: FiniteContainer[Index, Value]) => p.foldRight(start, f))
  }

  /**
   * Produce the element over values obtained by selecting a particular container and reducing through its values.
   */
  def reduce(f: (Value, Value) => Value): Element[Value] = {
    CachingChain(element, (p: FiniteContainer[Index, Value]) => p.reduce(f))
  }
  
}

*/