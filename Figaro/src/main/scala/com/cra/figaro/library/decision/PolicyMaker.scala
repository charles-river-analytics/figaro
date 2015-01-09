/*
 * PolicyMaker.scala
 * Trait that defines how to make a strategy for a particular decision. All decision elements must implement
 * 
 * Created By:      Brian Ruttenberg (bruttenberg@cra.com)
 * Creation Date:   Oct 1, 2012
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.library.decision

import com.cra.figaro.algorithm.decision._
import com.cra.figaro.algorithm.decision.index._
import scala.collection.immutable.Map

/**
 * Trait that defines how to make a policy for a particular decision. All decision elements must implement
 * this trait by defining makePolicy. The default usage of caching and noncaching decisions
 * provide implementations of the makePolicy function. If you want a different policy, you must
 * define makePolicy.
 */

trait PolicyMaker[T, U] {
  /**
   * Returns a DecisionPolicy from a map of parent/decision tuple values to DecisionSamples.
   */
  def makePolicy(policyMap: Map[(T, U), DecisionSample]): DecisionPolicy[T, U]
  /**
   * Returns a DecisionPolicy from the given decision algorithm .
   */
  def makePolicy(alg: DecisionAlgorithm[T, U]): DecisionPolicy[T, U] = makePolicy(alg.getUtility())
}

/**
 * Trait that implements an exact policy.
 */
trait ExactPolicyMaker[T, U] extends PolicyMaker[T, U] {
  def makePolicy(policyMap: Map[(T, U), DecisionSample]) = DecisionPolicyExact(policyMap)
}
