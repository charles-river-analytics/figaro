/*
 * FromRange.scala
 * Elements representing uniform distributions over integers in a range.
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Jan 1, 2009
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.library.atomic.discrete

import com.cra.figaro.language._

/**
 * A uniform distribution over integers in range from lower (inclusive) to upper (exclusive).
 */
class FromRange(name: Name[Int], lower: Int, upper: Int, collection: ElementCollection)
  extends AtomicUniform(name, lower until upper, collection)

object FromRange {
  /**
   * Create a uniform distribution over integers in range from lower (inclusive) to upper (exclusive).
   */
  def apply(lower: Int, upper: Int)(implicit name: Name[Int], collection: ElementCollection) =
    new FromRange(name, lower, upper, collection)
}
