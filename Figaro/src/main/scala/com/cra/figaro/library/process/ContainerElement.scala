/*
 * ContainerElement.scala
 * Class for a Container element
 *
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Oct 14, 2014
 *
 * Copyright 2014 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.library.process

import com.cra.figaro.language._

/**
 * Represents an element whose value is a container.
 *
 * Elements that are created by operations are put in the same universe as this element.
 */
class ContainerElement[Index, Value](val element: Element[Container[Index, Value]])
{
  /**
   * Creates an element whose value is the value at the corresponding index of the value of the process element.
   */
  def apply(i: Index): Element[Value] = {
    if (!element.active) element.activate()
    CachingChain(element, (c: Container[Index, Value]) => c(i))("", element.universe)
  }

  /**
   * Creates an element whose value is the value at the corresponding index in the value of the container element,
   * if the index is in range, None otherwise.
   */
  def get(i: Index): Element[Option[Value]] = {
    if (!element.active) element.activate()
    CachingChain(element, (c: Container[Index, Value]) => c.get(i))("", element.universe)
  }

  /**
   * Map the given function pointwise through the value of the container element.
   */
  def map[Value2](f: Value => Value2): ContainerElement[Index, Value2] = {
    if (!element.active) element.activate()
    new ContainerElement(Apply(element, (c: Container[Index, Value]) => c.map(f))("", element.universe))
  }

  /**
   * Chain the given function pointwise through the value of the container element.
   */
  def chain[Value2](f: Value => Element[Value2]): ContainerElement[Index, Value2] = {
    if (!element.active) element.activate()
    new ContainerElement(Apply(element, (c: Container[Index, Value]) => c.chain(f))("", element.universe))
  }

  /**
   * Produce the element over values obtained by selecting a particular container and folding through its values.
   */
  def foldLeft[Value2](start: Value2)(f: (Value2, Value) => Value2): Element[Value2] = {
    if (!element.active) element.activate()
    CachingChain(element, (c: Container[Index, Value]) => c.foldLeft(start)(f))("", element.universe)
  }

  /**
   * Produce the element over values obtained by selecting a particular container and folding through its values.
   */
  def foldRight[Value2](start: Value2)(f: (Value, Value2) => Value2): Element[Value2] = {
    if (!element.active) element.activate()
    CachingChain(element, (c: Container[Index, Value]) => c.foldRight(start)(f))("", element.universe)
  }

  /**
   * Produce the element over values obtained by selecting a particular container and reducing through its values.
   */
  def reduce(f: (Value, Value) => Value): Element[Value] = {
    if (!element.active) element.activate()
    CachingChain(element, (c: Container[Index, Value]) => c.reduce(f))("", element.universe)
  }

  /**
   * Aggregate the results of applying an operator to each element.
   */
  def aggregate[Value2](start: => Value2)(seqop: (Value2, Value) => Value2, combop: (Value2, Value2) => Value2): Element[Value2] = {
    if (!element.active) element.activate()
    foldLeft(start)((v1: Value2, v2: Value) => combop(v1, seqop(v1, v2)))
  }

  /**
   * Returns an element representing the number of elements in the container whose values satisfy the predicate.
   */
  def count(f: (Value) => Boolean): Element[Int] = {
    foldLeft(0)((i: Int, v: Value) => if (f(v)) i + 1 else i)
  }

  /**
   * Returns an element representing whether the value of any element in the container satisfies the predicate.
   */
  def exists(pred: Value => Boolean): Element[Boolean] = {
    foldLeft(false)((b: Boolean, v: Value) => pred(v) || b)
  }

  /**
   * Returns an element representing whether the values of all elements in the container satisfy the predicate.
   */
  def forall(pred: Value => Boolean): Element[Boolean] = {
    foldLeft(true)((b: Boolean, v: Value) => pred(v) && b)
  }

  /**
   * Returns an element representing the length of the container.
   */
  def length: Element[Int] = {
    foldLeft(0)((i: Int, v: Value) => i + 1)
  }

  /**
   * Concatenate the value of this container element with the value of another container element.
   */
  def concat[Index2](that: ContainerElement[Index2, Value]): ContainerElement[Int, Value] = {
    val resultElem: Element[Container[Int, Value]] =
      new Apply2("", this.element, that.element, (c1: Container[Index, Value], c2: Container[Index2, Value]) => c1.concat(c2), this.element.universe)
    new ContainerElement(resultElem)
  }
}
