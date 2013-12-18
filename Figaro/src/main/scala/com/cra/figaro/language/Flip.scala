/*
 * Flip.scala
 * Weighted coin tosses
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

import com.cra.figaro.library.atomic.continuous._
import com.cra.figaro.util._

/**
 * Weighted coin tosses, where the weight itself might be random.
 */
trait Flip extends Element[Boolean] with Cacheable[Boolean] {
  type Randomness = Double

  def generateRandomness() = random.nextDouble()

  def generateValue(rand: Randomness) = rand < probValue

  protected def probValue: Double
}

/**
 * A coin toss in which the weight is a fixed constant.
 */
class AtomicFlip(name: Name[Boolean], val prob: Double, collection: ElementCollection)
  extends Element[Boolean](name, collection) with Atomic[Boolean] with Flip {
  protected def probValue = prob

  def density(b: Boolean) = if (b) prob; else 1.0 - prob

  override def toString = "Flip(" + prob + ")"
}

/**
 * A coin toss where the weight is itself an element.
 */
class CompoundFlip(name: Name[Boolean], val prob: Element[Double], collection: ElementCollection)
  extends Element[Boolean](name, collection) with Flip {
  def args: List[Element[_]] = List(prob)

  protected def probValue = prob.value

  override def toString = "Flip(" + prob + ")"
}

/**
 * A coin toss where the weight is specified by a learnable parameter.
 */
class ParameterizedFlip(name: Name[Boolean], override val parameter: AtomicBetaParameter, collection: ElementCollection)
  extends Element[Boolean](name, collection) with Flip with Parameterized[Boolean] {
  def args: List[Element[_]] = List(parameter)

  protected def probValue = parameter.value

  override def toString = "Parameterized Flip(" + parameter + ")"
}

object Flip extends Creatable {
  /**
   * A coin toss in which the weight is a fixed constant.
   */
  def apply(prob: Double)(implicit name: Name[Boolean], collection: ElementCollection) =
    new AtomicFlip(name, prob, collection)

  /**
   * A coin toss where the weight is itself an element.
   */
  def apply(prob: Element[Double])(implicit name: Name[Boolean], collection: ElementCollection) =
    new CompoundFlip(name, prob, collection)

  /**
  * A coin toss where the weight is specified by a learnable parameter.
  */
  def apply(prob: AtomicBetaParameter)(implicit name: Name[Boolean], collection: ElementCollection) =
    new ParameterizedFlip(name, prob, collection)

  /** Used for reflection. */
  type ResultType = Boolean

  /** Used for reflection. */
  def create(args: List[Element[_]]) = apply(args(0).asInstanceOf[Element[Double]])
}
