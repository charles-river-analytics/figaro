/*
 * AtomicComponent.scala
 * Problem components for atomic elements.
 *
 * Created By:      William Kretschmer (kretsch@mit.edu)
 * Creation Date:   Apr 14, 2017
 *
 * Copyright 2017 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.algorithm.structured

import com.cra.figaro.algorithm.factored.factors.Variable
import com.cra.figaro.algorithm.lazyfactored._
import com.cra.figaro.algorithm.structured.strategy.range.AtomicRanger
import com.cra.figaro.language._

/**
 * Problem components for atomic elements.
 */
class AtomicComponent[Value](problem: Problem, val atomic: Atomic[Value], val ranger: AtomicRanger[Value])
  extends ProblemComponent(problem, atomic) {
  /**
   * Probabilities associated with values in the range of this component. This is used for factor creation. It is
   * required that (1) the key set of this map equals the range of the component, and (2) the values in this map sum to
   * 1.0.
   */
  var probs: Map[Extended[Value], Double] = Map(range.xvalues.head -> 1.0) // Initialize to {* -> 1.0}

  override def generateRange(): Unit = {
    // Generate extended values and probabilities with the discretizer
    probs = ranger.discretize()
    // Assign the range in the normal way using the extended values
    val newRange = new ValueSet(probs.keySet)
    if ((newRange.hasStar ^ range.hasStar) || (newRange.regularValues != range.regularValues)) {
      range = newRange
      setVariable(new Variable(range))
    }
  }
}
