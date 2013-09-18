/*
 * Reference.scala
 * References
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Jan 1, 2009
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 */

package com.cra.figaro.language

import scala.language.implicitConversions

/**
 * A reference to an element.
 */
sealed trait Reference[+T]

/**
 * A direct reference to an element by its name.
 * 
 * @param string A string representation of the name
 */
case class Name[+T](string: String) extends Reference[T]

/**
 * An indirect reference to an element, first using the head to refer to an element collection, and then
 * referring to the tail from within the element collection.
 * 
 * @param head The name of the element whose value is the element collection that resolves the tail.
 * @param tail The remaining reference to be resolved.
 */
case class Indirect[+T](head: Name[ElementCollection], tail: Reference[T]) extends Reference[T]

object Reference {
  /**
   * Implicitly convert a string to a reference. Names within the string are separated by dots.
   */
  implicit def stringToReference[T](string: String): Reference[T] =
    makeReference(string.split('.').toList)

  private def makeReference[T](strings: List[String]): Reference[T] =
    strings match {
      case List(string) => Name[T](string)
      case head :: tail => Indirect(Name[ElementCollection](head), makeReference(tail))
      case List() => throw new IllegalArgumentException("Empty reference")
    }

}

object Name {
  /**
   * The default name is the empty string.
   */
  implicit def default[T]: Name[T] = Name[T]("")

  /**
   * Implicitly convert a string to a name.
   */
  implicit def stringToName[T](s: String): Name[T] = Name[T](s)

  /**
   * Implicitly convert a name to a string.
   */
  implicit def nameToString[T](m: Name[T]) = m.string
}