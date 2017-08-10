/*
 * Pragma.scala
 * Pragmas: hints to algorithms.
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

/**
 * Pragmas are hints to algorithms that are associated with elements. A Pragma is parameterized by the type parameter
 * of the element to which it is attached. Pragmas are added to elements using Element's addPragma method. More
 * recently added pragmas can shadow older pragmas of the same type. Pragmas can be removed using Element's
 * removePragma method.
 */

trait Pragma[T] 
