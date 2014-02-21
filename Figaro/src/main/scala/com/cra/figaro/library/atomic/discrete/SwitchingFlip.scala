/*
 * SwitchingFlip.scala
 * Flips that switch randomness every time they are proposed.
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   May 9, 2011
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.library.atomic.discrete

import com.cra.figaro.algorithm._
import com.cra.figaro.algorithm.factored._
import com.cra.figaro.language._
import com.cra.figaro.util._

/**
 * A Flip that switches its randomness (true to false or false to true) every time it is proposed.
 */
trait SwitchingFlip extends Flip {
  /**
   * Propose the next randomness, switching the probability of the flip. 
   * Since both the proposal and reverse proposal have probability 1, 
   * the second return value is 1. The third return value is the
   * probability of the new value divided by the probability of the randomness.
   */
  override def nextRandomness(rand: Randomness) =
    if (rand < probValue) (uniform(probValue, 1.0), 1.0, (1.0 - probValue) / probValue)
    else (uniform(0.0, probValue), 1.0, probValue / (1.0 - probValue))

  private def uniform(lower: Double, upper: Double) = random.nextDouble * (upper - lower) + lower
}

/**
 * A Flip that switches its randomness (true to false or false to true) every time it is proposed.
 * The parameter is constant.
 */
class AtomicSwitchingFlip(name: Name[Boolean], prob: Double, collection: ElementCollection)
  extends AtomicFlip(name, prob, collection) with SwitchingFlip

/**
 * A Flip that switches its randomness (true to false or false to true) every time it is proposed.
 * The parameter is an element.
 */
class CompoundSwitchingFlip(name: Name[Boolean], prob: Element[Double], collection: ElementCollection)
  extends CompoundFlip(name, prob, collection) with SwitchingFlip

object SwitchingFlip extends Creatable {
  /**
   * Create a Flip that switches its randomness (true to false or false to true) every time it is proposed.
   * The parameter is constant.
   */
  def apply(prob: Double)(implicit name: Name[Boolean], collection: ElementCollection) =
    new AtomicSwitchingFlip(name, prob, collection)

  /**
   * Create a Flip that switches its randomness (true to false or false to true) every time it is proposed.
   * The parameter is an element.
   */
  def apply(prob: Element[Double])(implicit name: Name[Boolean], collection: ElementCollection) =
    new CompoundSwitchingFlip(name, prob, collection)

  type ResultType = Boolean

  def create(args: List[Element[_]]) = apply(args(0).asInstanceOf[Element[Double]])
}
