/*
 * Protein.scala
 * A protein class, which is an Element[AminoAcidSequence].
 * 
 * Created By:      Brian Ruttenberg (bruttenberg@cra.com)
 * Creation Date:   Oct 1, 2012
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 */

package com.cra.figaro.example.dosage

import com.cra.figaro.language._
import com.cra.figaro.library.atomic.discrete._
import com.cra.figaro.example.dosage.Conversion._

/**
 * A protein class, which is an Element[AminoAcidSequence].
 * Given an input string, the protein represents a distribution over
 * the possible AminoAcidSequences that can be generated from the string
 */
class Protein(name: Name[AminoAcidSequence], arg1: String, collection: ElementCollection)
  extends Apply1[List[AminoAcidSequence], AminoAcidSequence](name, Protein.genInjectFcn(arg1), Protein.genApplyFcn, collection)

object Protein {

  def genInjectFcn = (s: String) => genFcn(s)
  def genApplyFcn = (l: List[AminoAcidSequence]) => (AminoAcidSequence("") /: l)(_ + _)

  /* A '-' means any AA, so this is a uniform distribution over existing AA. A
   * named AA is represented as a constant */
  def genFcn(s: String): Inject[AminoAcidSequence] = {
    val elems = s.map { c =>
      c match {
        case '-' => Uniform(aaListAsSeq: _*)
        case _ => Constant(AminoAcidSequence(c.toString))
      }
    }
    Inject(elems: _*)
  }

  def apply(arg: String)(implicit name: Name[AminoAcidSequence], collection: ElementCollection) =
    new Protein(name, arg, collection)

}