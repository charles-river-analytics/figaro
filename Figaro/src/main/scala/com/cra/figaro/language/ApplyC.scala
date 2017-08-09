/*
 * ApplyC.scala
 * This class is a workaround for adding easier type inference to Apply class.  Since Apply object is already using apply method with different number of arguments it is not easy to find a workaround adding currying support to apply methods in Apply class.
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Jan 1, 2009
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.language

/**

  */
object ApplyC {
  /**
    * Application of a function to one argument.
    */
  def apply[T1, U](arg1: Element[T1])(fn: T1 => U)(implicit name: Name[U], collection: ElementCollection) =
    new Apply1(name, arg1, fn, collection)

  /**
    * Application of a function to two arguments.
    */
  def apply[T1, T2, U](arg1: Element[T1], arg2: Element[T2])(fn: (T1, T2) => U)(implicit name: Name[U], collection: ElementCollection) =
    new Apply2(name, arg1, arg2, fn, collection)

  /**
    * Application of a function to three arguments.
    */
  def apply[T1, T2, T3, U](arg1: Element[T1], arg2: Element[T2], arg3: Element[T3])(fn: (T1, T2, T3) => U)(implicit name: Name[U], collection: ElementCollection) =
    new Apply3(name, arg1, arg2, arg3, fn, collection)

  /**
    * Application of a function to four arguments.
    */
  def apply[T1, T2, T3, T4, U](arg1: Element[T1], arg2: Element[T2], arg3: Element[T3], arg4: Element[T4])
                              (fn: (T1, T2, T3, T4) => U)(implicit name: Name[U], collection: ElementCollection) =
    new Apply4(name, arg1, arg2, arg3, arg4, fn, collection)

  /**
    * Application of a function to five arguments.
    */
  def apply[T1, T2, T3, T4, T5, U](arg1: Element[T1], arg2: Element[T2], arg3: Element[T3], arg4: Element[T4],
                                   arg5: Element[T5])
                                  (fn: (T1, T2, T3, T4, T5) => U)(implicit name: Name[U], collection: ElementCollection) =
    new Apply5(name, arg1, arg2, arg3, arg4, arg5, fn, collection)
}
