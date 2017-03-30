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

import com.cra.figaro.algorithm.factored.ParticleGenerator
import com.cra.figaro.algorithm.structured.ProblemComponent


package object refine {

  /**
   * A range sizer chooses a size of range for components corresponding to atomic elements.
   */
  type RangeSizer = ProblemComponent[_] => Int

  /**
   * The default range sizer uses a fixed number of samples for each component. It takes
   * `ParticleGenerator.defaultTotalSamples` samples for any component given.
   */
  def defaultRangeSizer(pc: ProblemComponent[_]) = ParticleGenerator.defaultMaxNumSamplesAtChain

  /**
   * A range sizer that takes an additional `ParticleGenerator.defaultTotalSamples` samples each time it is called.
   */
  def increasingRangeSizer(pc: ProblemComponent[_]) = {
    val universe = pc.element.universe
    if(ParticleGenerator.exists(universe)) {
      ParticleGenerator(universe).samplesTaken(pc.element) + 1
    }
    else 1
  }
}
