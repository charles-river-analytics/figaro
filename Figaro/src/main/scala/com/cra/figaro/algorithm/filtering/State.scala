/*
 * State.scala
 * Stores dynamic and static snapshots of a universe
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Apr 1, 2013
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.algorithm.filtering

import com.cra.figaro.language._

/**
 * A state contains snapshots of the static universe and the dynamic universe, 
 * representing a configuration of the static and dynamic variables in the system. 
 */
class State(val dynamic: Snapshot, val static: Snapshot) {
  /**
   * Get the value of the element associated with the given reference on the dynamic snapshot, or the
   * static snapshot if the referred element cannot be found in the dynamic snapshot.
   */
  def get[T](reference: Reference[T]): T = {
    try dynamic.get(reference)
    catch { case _: NoSuchElementException => static.get(reference) }
  }
}
