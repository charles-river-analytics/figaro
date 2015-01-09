/*
 * IncrementalProcess.scala
 * Trait for an incremental process
 *
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Oct 14, 2014
 *
 * Copyright 2014 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.library.collection

import com.cra.figaro.language._

/**
 * A process in which you can incrementally get the elements at a set of indices, while producing a new process.
 * The new process should take into account any dependencies with elements that have already been generated.
 */
trait IncrementalProcess[Index, Value] extends Process[Index, Value] {
  /**
   * Produce the elements representing the value of the process at the given indices.
   * Ensures that any dependencies between the elements are represented.
   * Also return a new container such that when elements for future indices are produced,
   * any dependencies between those elements and the ones for these indices are represented.
   *
   * The first return value maps each provided index to the corresponding element.
   * The second return values is the new container.
   *
   * This method does not assume that the indices have already been range checked.
   */
  protected def generateIncremental(indices: List[Index]): (Map[Index, Element[Value]], Process[Index, Value])
}
