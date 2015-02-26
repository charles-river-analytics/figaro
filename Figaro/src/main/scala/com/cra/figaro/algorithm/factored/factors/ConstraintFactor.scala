/*
 * ConstraintFactor.scala
 * Sparse implementation of factors over values.
 * 
 * Created By:      Glenn Takata (gtakata@cra.com)
 * Creation Date:   Feb 25, 2015
 * 
 * Copyright 2015 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */
package com.cra.figaro.algorithm.factored.factors

import com.cra.figaro.util._
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map
import scala.util.control.Breaks._
import com.cra.figaro.language.Element
import com.cra.figaro.algorithm.lazyfactored.Extended
import scala.reflect.runtime.universe._

/**
 * Constraint Factor. A factor is associated with a set of variables and specifies a value for every
 * combination of assignments to those variables. Factors are parameterized by the types of values they contain.
 * 
 * This factor is used to model constraints on elements.
 */
class ConstraintFactor[T](parents: List[Variable[_]], output: List[Variable[_]])(implicit tag: TypeTag[T])
  extends BasicFactor[T](parents, output){
    override val isConstraint = true
} 
