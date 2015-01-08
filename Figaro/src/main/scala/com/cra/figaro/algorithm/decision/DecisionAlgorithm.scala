/*
 * DecisionAlgorithm.scala
 * A generic decision algorithm trait
 * 
 * Created By:      Brian Ruttenberg (bruttenberg@cra.com)
 * Creation Date:   Oct 1, 2012
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.algorithm.decision


import com.cra.figaro.algorithm._
import com.cra.figaro.library.decision._
import com.cra.figaro.util._
import scala.collection.immutable.Map

/**
 * Trait that defines some common interface functions for decision algorithms.
 * Every decision algorithm must define the function computeUtility().
 */

trait DecisionAlgorithm[T, U] extends Algorithm {
  /**
   * Compute the utility of each parent/decision tuple and return a DecisionSample.
   * Each decision algorithm must define how this is done since it is used to set
   * the policy for a decision. For sampling algorithms, this will me a map of 
   * parent/decision tuples to a utility and a weight for that combination. For factored
   * algorithms, the DecisionSample will contain the exact expected utility with a weight of 1.0.
   */
  def computeUtility(): Map[(T, U), DecisionSample]
  
  // Memoize the utility in a lazy value
  private lazy val util = computeUtility()
  
  /** 
   *  Get the total utility and weight for all sampled values of the parent and decision.
   */
  def getUtility() = util
  
  /** 
   *  Get the total utility and weight for a specific value of a parent and decision.
   */
  def getUtility(p: T, d: U) = util((p, d))

  /** 
   *  Sets the policy for the given decision. This will get the computed utility of the algorithm
   *  and call setPolicy on the decision. Note there is no error checking here, so the decision in
   *  the argument must match the target decision in the algorithm.
   */
  def setPolicy(e: Decision[T, U]): Unit = e.setPolicy(getUtility())
}

/**
 * Trait for one time Decision Algorithms.
 */
trait OneTimeProbQueryDecision[T, U] extends OneTimeProbQuery with DecisionAlgorithm[T, U]
