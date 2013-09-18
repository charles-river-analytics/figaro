/*
 * Sequence.scala
 * A sequence.
 * 
 * Created By:      Brian Ruttenberg (bruttenberg@cra.com)
 * Creation Date:   Oct 1, 2012
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 */

package com.cra.figaro.example.dosage

import com.cra.figaro.algorithm.decision.index._
import com.cra.figaro.example.dosage._

/**
 * A sequence.
 */
abstract class Sequence(val seq: String) {
  override def equals(o: Any) = o match {
    case that: Sequence => that.seq == seq
    case _ => false
  }
  override def hashCode = seq.hashCode

  override def toString() = seq.toString()
}
