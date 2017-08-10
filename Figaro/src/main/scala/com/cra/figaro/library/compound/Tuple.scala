/*
 * Tuple.scala
 * Creation of tuple elements.
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Oct 18, 2010
 * 
 * Copyright 2017 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */ 

package com.cra.figaro.library.compound

import com.cra.figaro.language._

object ^^ {
  /**
   * Create an element over pairs.
   */
  def apply[T1,T2](arg1: Element[T1], arg2: Element[T2])
                  (implicit name: Name[(T1,T2)], collection: ElementCollection): Element[(T1,T2)] =
    Apply(arg1, arg2, (t1: T1, t2: T2) => (t1, t2))(name, collection)

  /**
   * Create an element over triples.
   */
  def apply[T1,T2,T3](arg1: Element[T1], arg2: Element[T2], arg3: Element[T3])
                     (implicit name: Name[(T1,T2,T3)], collection: ElementCollection):

  Element[(T1,T2,T3)] =
    Apply(arg1, arg2, arg3, (t1: T1, t2: T2, t3: T3) => (t1, t2, t3))(name, collection)

  /**
   * Create an element over quadruples.
   */
  def apply[T1,T2,T3,T4](arg1: Element[T1], arg2: Element[T2], arg3: Element[T3], arg4: Element[T4])
                        (implicit name: Name[(T1,T2,T3,T4)], collection: ElementCollection):
  Element[(T1,T2,T3,T4)] =
    Apply(arg1, arg2, arg3, arg4, (t1: T1, t2: T2, t3: T3, t4: T4) => (t1, t2, t3, t4))(name, collection)

  /**
   * Create an element over quintuples.
   */
  def apply[T1,T2,T3,T4,T5](arg1: Element[T1], arg2: Element[T2], arg3: Element[T3], arg4: Element[T4],
                            arg5: Element[T5])
                           (implicit name: Name[(T1,T2,T3,T4,T5)], collection: ElementCollection):
  Element[(T1,T2,T3,T4,T5)] =
    Apply(arg1, arg2, arg3, arg4, arg5,
      (t1: T1, t2: T2, t3: T3, t4: T4, t5: T5) => (t1, t2, t3, t4, t5))(name, collection)
}
