/*
 * Apply.scala
 * Elements that lift Scala functions to Figaro.
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Jan 1, 2009
 * 
 * Copyright 2017 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.language

import scala.collection.mutable.Map

/**
 * Abstract base class for elements representing function application.
 * The function is cached to force different applications of the function to the same argument to result in equal values.
 */
abstract class Apply[U](name: Name[U], collection: ElementCollection)
  extends Deterministic[U](name, collection) with IfArgsCacheable[U] {
}

/**
 * Application of a function to one argument.
 */
class Apply1[T1, U](name: Name[U], val arg1: Element[T1], val fn: T1 => U, collection: ElementCollection)
  extends Apply[U](name, collection) {
  def args: List[Element[_]] = List(arg1)

  type Arg1Type = T1

  def generateValue() = {
    if (arg1.value == null) arg1.generate() 
    fn(arg1.value)
  }

  override def toString = "Apply(" + arg1 + ", " + fn + ")"
}

/**
 * Application of a function to two arguments.
 */
class Apply2[T1, T2, U](name: Name[U], val arg1: Element[T1], val arg2: Element[T2],
  val fn: (T1, T2) => U, collection: ElementCollection)
  extends Apply[U](name, collection) {
  def args: List[Element[_]] = List(arg1, arg2)

  type Arg1Type = T1
  type Arg2Type = T2

  def generateValue() = {
    if (arg1.value == null) arg1.generate()
    if (arg2.value == null) arg2.generate()
    fn(arg1.value, arg2.value)
  }

  override def toString = "Apply(" + arg1 + ", " + arg2 + ", " + fn + ")"
}

/**
 * Application of a function to three arguments.
 */
class Apply3[T1, T2, T3, U](name: Name[U], val arg1: Element[T1], val arg2: Element[T2], val arg3: Element[T3],
  val fn: (T1, T2, T3) => U, collection: ElementCollection)
  extends Apply[U](name, collection) {
  def args: List[Element[_]] = List(arg1, arg2, arg3)

  type Arg1Type = T1
  type Arg2Type = T2
  type Arg3Type = T3

  def generateValue() = {
    if (arg1.value == null) arg1.generate()
    if (arg2.value == null) arg2.generate()
    if (arg3.value == null) arg3.generate()
    fn(arg1.value, arg2.value, arg3.value)
  }

  override def toString = "Apply(" + arg1 + ", " + arg2 + ", " + arg3 + ", " + fn + ")"
}

/**
 * Application of a function to four arguments.
 */
class Apply4[T1, T2, T3, T4, U](name: Name[U], val arg1: Element[T1], val arg2: Element[T2], val arg3: Element[T3],
  val arg4: Element[T4], val fn: (T1, T2, T3, T4) => U, collection: ElementCollection)
  extends Apply[U](name, collection) {
  def args: List[Element[_]] = List(arg1, arg2, arg3, arg4)

  type Arg1Type = T1
  type Arg2Type = T2
  type Arg3Type = T3
  type Arg4Type = T4

  def generateValue() = {
    if (arg1.value == null) arg1.generate()
    if (arg2.value == null) arg2.generate()
    if (arg3.value == null) arg3.generate()
    if (arg4.value == null) arg4.generate()
    fn(arg1.value, arg2.value, arg3.value, arg4.value)
  }

  override def toString = "Apply(" + arg1 + ", " + arg2 + ", " + arg3 + ", " + arg4 + ", " + fn + ")"
}

/**
 * Application of a function to five arguments.
 */
class Apply5[T1, T2, T3, T4, T5, U](name: Name[U], val arg1: Element[T1], val arg2: Element[T2], val arg3: Element[T3],
  val arg4: Element[T4], val arg5: Element[T5], val fn: (T1, T2, T3, T4, T5) => U, collection: ElementCollection)
  extends Apply[U](name, collection) {
  def args: List[Element[_]] = List(arg1, arg2, arg3, arg4, arg5)

  type Arg1Type = T1
  type Arg2Type = T2
  type Arg3Type = T3
  type Arg4Type = T4
  type Arg5Type = T5

  def generateValue() = {
    if (arg1.value == null) arg1.generate()
    if (arg2.value == null) arg2.generate()
    if (arg3.value == null) arg3.generate()
    if (arg4.value == null) arg4.generate()
    if (arg5.value == null) arg5.generate()
    fn(arg1.value, arg2.value, arg3.value, arg4.value, arg5.value)
  }

  override def toString =
    "Apply(" + arg1 + ", " + arg2 + ", " + arg3 + ", " + arg4 + ", " + arg5 + ", " + fn + ")"
}

object Apply {
  /**
   * Application of a function to one argument.
   */
  def apply[T1, U](arg1: Element[T1], fn: T1 => U)(implicit name: Name[U], collection: ElementCollection) =
    new Apply1(name, arg1, fn, collection)

  /**
   * Application of a function to two arguments.
   */
  def apply[T1, T2, U](arg1: Element[T1], arg2: Element[T2], fn: (T1, T2) => U)(implicit name: Name[U], collection: ElementCollection) =
    new Apply2(name, arg1, arg2, fn, collection)

  /**
   * Application of a function to three arguments.
   */
  def apply[T1, T2, T3, U](arg1: Element[T1], arg2: Element[T2], arg3: Element[T3],
    fn: (T1, T2, T3) => U)(implicit name: Name[U], collection: ElementCollection) =
    new Apply3(name, arg1, arg2, arg3, fn, collection)

  /**
   * Application of a function to four arguments.
   */
  def apply[T1, T2, T3, T4, U](arg1: Element[T1], arg2: Element[T2], arg3: Element[T3], arg4: Element[T4],
    fn: (T1, T2, T3, T4) => U)(implicit name: Name[U], collection: ElementCollection) =
    new Apply4(name, arg1, arg2, arg3, arg4, fn, collection)

  /**
   * Application of a function to five arguments.
   */
  def apply[T1, T2, T3, T4, T5, U](arg1: Element[T1], arg2: Element[T2], arg3: Element[T3], arg4: Element[T4],
    arg5: Element[T5],
    fn: (T1, T2, T3, T4, T5) => U)(implicit name: Name[U], collection: ElementCollection) =
    new Apply5(name, arg1, arg2, arg3, arg4, arg5, fn, collection)
}
