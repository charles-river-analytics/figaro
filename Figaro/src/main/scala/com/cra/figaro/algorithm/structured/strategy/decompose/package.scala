/*
 * package.scala
 * Definitions of expansion and solution strategies.
 *
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   March 1, 2015
 *
 * Copyright 2015 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */
package com.cra.figaro.algorithm.structured.strategy

import com.cra.figaro.algorithm.structured.strategy.decompose.StructuredStrategy
import com.cra.figaro.algorithm.structured.Problem
import com.cra.figaro.algorithm.structured.ProblemComponent
import com.cra.figaro.algorithm.factored.ParticleGenerator


package object decompose {
  /**
   * A strategy takes a problem and does something useful, such as computing a solution.
   * Type-wise, a strategy simply does something with the problem.
   */
  type Strategy = Problem => Unit

  /**
   * A range sizer chooses a size of range for components corresponding to atomic elements.
   */
  type RangeSizer = ProblemComponent[_] => Int

  def defaultRangeSizer(pc: ProblemComponent[_]) = ParticleGenerator.defaultMaxNumSamplesAtChain
}
