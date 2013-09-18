/*
 * Parameterized.scala
 * Elements which accept learnable parameters
 * 
 * Created By:      Michael Howard (mhoward@cra.com)
 * Creation Date:   Jun 1, 2013
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 */

package com.cra.figaro.language

  /**
  * Trait of elements which accept learnable parameters. 
  * Parameterized elements are compound elements whose outcome is determined by a learnable parameter.
  */

trait Parameterized[T] extends Element[T] {
  /**
  * The parameter for this element.
  */
  val parameter: Parameter[_]
}