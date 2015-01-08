/*
 * IncrementalProcess.scala
 * Trait for an independent process
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
 * A process in which all elements are independent.
 */
trait IndependentProcess[Index, Value] extends Process[Index, Value] {
  val generator: Index => Element[Value]

  def generate(index: Index) = generator(index)

  def generate(indices: List[Index]) = {
    Map(indices.map(index => (index, generator(index))):_*)
  }
}
