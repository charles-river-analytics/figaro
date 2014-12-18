/*
 * ProcessElement.scala
 * Class for an element whose value is a process
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
 * Represents an element whose value is a process.
 *
 * Elements that are created by operations are put in the same universe as this element.
 */
class ProcessElement[Index, Value](val element: Element[Process[Index, Value]]) {
  /**
   * Creates an element whose value is the value at the corresponding index of the value of the process element.
   */
  def apply(i: Index): Element[Value] = {
    Chain(element, (p: Process[Index, Value]) => p(i))("", element.universe)
  }

  /**
   * Safely creates an element whose value is the optional value at the corresponding index of the value
   * of the process element. If the value of the process element does not have the corresponding index, the value
   * of this element is None.
   */
  def get(i: Index): Element[Option[Value]] = {
    Chain(element, (p: Process[Index, Value]) => p.get(i))("", element.universe)
  }

  /**
   * Map the given function pointwise through the value of the process element.
   */
  def map[Value2](f: Value => Value2): ProcessElement[Index, Value2] = {
    new ProcessElement(Apply(element, (p: Process[Index, Value]) => p.map(f))("", element.universe))
  }

  /**
   * Chain the given function pointwise through the value of the process element.
   */
  def chain[Value2](f: Value => Element[Value2]): ProcessElement[Index, Value2] = {
    new ProcessElement(Apply(element, (p: Process[Index, Value]) => p.chain(f))("", element.universe))
  }
}

