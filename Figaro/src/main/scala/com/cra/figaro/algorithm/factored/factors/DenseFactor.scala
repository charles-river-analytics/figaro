/*
 * DenseFactor.scala
 * Default implementation of factors over values.
 *
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Jan 1, 2009
 *
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */
package com.cra.figaro.algorithm.factored.factors

import com.cra.figaro.util._
import scala.annotation.tailrec
import scala.collection.mutable.Map
import com.cra.figaro.language._
import com.cra.figaro.algorithm.lazyfactored.Extended
import scala.reflect.runtime.universe._
import scala.collection.mutable.WrappedArray
import scala.reflect.ClassTag
import scala.collection.mutable.ArraySeq

/**
 * Dense implementation of BasicFactor using an Array for storage
 *
 * Factors are parameterized by the type of the Variables they contain and contain a semiring
 * that defines the mathematical operation to be performed on the values
 * Parent variables are distinguished from the output variable.
 *
 */
class DenseFactor[T](val parents: List[Variable[_]], val output: List[Variable[_]], val semiring: Semiring[T] = SumProductSemiring().asInstanceOf[Semiring[T]])
  extends BasicFactor[T] {

  override def createFactor[T](parents: List[Variable[_]], output: List[Variable[_]], _semiring: Semiring[T] = semiring): Factor[T] =
    new DenseFactor[T](parents, output, _semiring)
    
  val contents: ArraySeq[T] = ArraySeq.fill(size)(semiring.zero)
  
  def getContents(): Traversable[T] = contents
  
  def stringContents(): String = contents.mkString(",")
  
  def contentsContains(index: List[Int]): Boolean = true
    
  val variableMag = 1 +: {for{i <- 1 until variables.size} yield {
    variables.take(i).map(_.size).product
  }}.toList
  
  def computeIndex(indices: List[Int]): Int = {
    {for(i <- 0 until indices.size) yield {indices(i)*variableMag(i)}}.toList.sum
  }
  
  
  /**
   * Get the value associated with a row. The row is identified by an list of indices
   * into the ranges of the variables over which the factor is defined.
   */
  def get(indices: List[Int]): T = {
    contents(computeIndex(indices))
  }

  def set(indices: List[Int], value: T): Factor[T]  = {
    contents.update(computeIndex(indices), value)
    this
  }
}
