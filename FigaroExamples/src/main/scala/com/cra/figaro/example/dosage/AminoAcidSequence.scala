/*
 * AminoAcidSequence.scala
 * An AminoAcidSequence, which implements the Distance trait so it can be used in the k-NN decision algorithm.
 * 
 * Created By:      Brian Ruttenberg (bruttenberg@cra.com)
 * Creation Date:   Oct 1, 2012
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.example.dosage

import com.cra.figaro.algorithm.decision.index._
import com.cra.figaro.example.dosage._

/**
 * An AminoAcidSequence, which implements the Distance trait so it can be used in the k-NN decision algorithm.
 */

class AminoAcidSequence(seq: String, distfcn: (Char, Char) => Int)
  extends Sequence(seq) with Distance[AminoAcidSequence] {
  override def equals(o: Any) = super.equals(o)
  override def hashCode = super.hashCode

  def +(that: AminoAcidSequence) = new AminoAcidSequence(this.seq + that.seq, distfcn)

  /* Distance between two AminoAcidSequences is the Needleman-Wunsh global
   * alignment distance between them. This is basically an edit distance
   * using the specified cost matrix.  */
  def distance(that: AminoAcidSequence) = {
    var F = Map[(Int, Int), Int]()

    def fillEntry(i: Int, j: Int): Int = {
      F.get((i, j)) match {
        case Some(e) => e
        case None => {
          val n = if (i == 0 && j == 0) 0
          else if (i == 0)
            j * distfcn('-', that.seq(j - 1))
          else if (j == 0)
            i * distfcn(seq.charAt(i - 1), '-')
          else {
            val compare = fillEntry(i - 1, j - 1) + distfcn(seq.charAt(i - 1), that.seq(j - 1))
            val delete = fillEntry(i - 1, j) + distfcn('-', that.seq.charAt(j - 1))
            val insert = fillEntry(i, j - 1) + distfcn(seq.charAt(i - 1), '-')
            math.min(math.min(compare, delete), insert)
          }
          F += (i, j) -> n
          n
        }
      }
    }
    val dist = fillEntry(seq.length, that.seq.length).toDouble
    dist
  }

}

object AminoAcidSequence {
  def apply(seq: String, distfcn: (Char, Char) => Int) = new AminoAcidSequence(seq, distfcn)
  def apply(seq: String) = new AminoAcidSequence(seq, (s1: Char, s2: Char) => ScoringMatrix.blosum62(s1)(s2))
}
