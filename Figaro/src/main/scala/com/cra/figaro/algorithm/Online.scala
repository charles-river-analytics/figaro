/*
 * Online.scala
 * Online algorithms.
 * 
 * Created By:      Michael Howard
 * Creation Date:   Mar 13, 2015
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.algorithm.online

import com.cra.figaro.algorithm.Algorithm
import com.cra.figaro.language.NamedEvidence
import com.cra.figaro.language.Universe


/**
 * The general class of online algorithms. An online algorithm is provided with an initial model,
 * represented by a universe encoding the probability distribution over the initial state,
 * and a transition model, which produces a universe encoding the probability distribution
 * over the new state. An implementation of Online must implement the update method.
 * 
 * Querying and asserting evidence to a online algorithm are done using references. This is because references are stable over time,
 * while the particular elements they refer to are not.
 * 
 */
abstract trait Online extends Algorithm {

  /**
   * Update the algorithm, conditioning on the new evidence
   */
  def update(evidence: Seq[NamedEvidence[_]]=Seq()): Unit
  val initial: Universe
  val transition: Function0[Universe]

}