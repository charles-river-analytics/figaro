/*
 * Protein.scala
 * A protein class, which is an Element[AminoAcidSequence].
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

import com.cra.figaro.language._
import com.cra.figaro.library.atomic.discrete._
import com.cra.figaro.example.dosage.Conversion._
import com.cra.figaro.library.collection._

/**
 * A protein is an Element[AminoAcidSequence].
 * Given an input string, the protein represents a distribution over
 * the possible AminoAcidSequences that can be generated from the string
 */
object Protein {
  def genApplyFcn = (l: Container[Int, AminoAcidSequence]) => l.foldLeft(AminoAcidSequence(""))(_ + _)

  /* A '-' means any AA, so this is a uniform distribution over existing AA. A
   * named AA is represented as a constant */
  def genFcn(s: String): Container[Int, AminoAcidSequence] = {
    val elems = s.map { c =>
      c match {
        case '-' => Uniform(aaListAsSeq: _*)
        case _ => Constant(AminoAcidSequence(c.toString))
      }
    }
    Container(elems: _*)
  }

  def apply(arg: String) = genApplyFcn(genFcn(arg))

}
