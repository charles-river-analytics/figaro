/*
 * Continuous.scala
 * Trait for continuous elements
 * 
 * Created By:      Synapski (no e-mail)
 * Creation Date:   Oct 6, 2014
 * 
 * Copyright 2014 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.language

/**
 * Trait of elements representing continuous probability distributions  
 */
trait Continuous[T] extends Element[T] {

  /**
   * Log-likelihood of a value.
   */
  def logp(value: T): Double

  //This constraint is not actually applied, but we have to define it so it can be set later.
  private var observationConstraint : T => Double = (t: T) => 1.0
  
    /**
   * Condition the element by observing a particular value.
   * Propagates the effect to dependent elements and ensures that no other value for the element can be generated.
   * For continuous elements, a constraint is added whose value is the log likelihood of the observation. 
   */
  override def observe(value: T) {
    //We have to remove old observation first, or repeatedly observing will add on lots of constraints
    //Should conditions be removed as well, as they are in regular element.observe?
    removeConditions()
    this.removeConstraint(observationConstraint)
    observationConstraint = (t: T) => logp(value)
    addLogConstraint( observationConstraint )
    set(value)
    this.observation = Some(value)
  }
  
  /**
   * Removes conditions on the element and allows different values of the element to be generated.
   */
  override def unobserve() {
    this.removeConstraint(observationConstraint)
    unset()
  }

}
