/*
 * NucleotideSequence.scala
 * Represents a nucleotide sequence.
 * 
 * Created By:      Brian Ruttenberg (bruttenberg@cra.com)
 * Creation Date:   Oct 1, 2012
 * 
 * Copyright 2017 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.example.dosage

import com.cra.figaro.algorithm.decision.index._
import com.cra.figaro.example.dosage._

/**
 * Represents a nucleotide sequence.
 */

class NucleotideSequence(seq: String) extends Sequence(seq) with Distance[NucleotideSequence] {
  override def equals(o: Any) = super.equals(o)
  override def hashCode = super.hashCode
  
  def +(that: NucleotideSequence) = new NucleotideSequence(this.seq+that.seq)
  
  def distance(that: NucleotideSequence) = 0.0
  
   
}

object NucleotideSequence {
  def apply(seq: String) = new NucleotideSequence(seq)
}
