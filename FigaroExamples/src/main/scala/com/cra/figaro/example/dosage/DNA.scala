/*
 * DNA.scala
 * Represents DNA, which is an Element[Nucleotide].
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

/**
 * Represents DNA, which is an Element[Nucleotide]. Given an input AminoAcidSequence,
 * this represents the distribution over all possible Nucleotide sequences that
 * are possible (since an AA -> Nuclotide is a one to many mapping)
 */
class DNA(name: Name[NucleotideSequence], arg1: Element[AminoAcidSequence], collection: ElementCollection)
  extends NonCachingChain[AminoAcidSequence, NucleotideSequence](name, arg1, DNA.genFcn, collection)

object DNA {

  def genFcn = (n: AminoAcidSequence) => genDNA(n)

  def nucToElement(n: String) = if (symbolToN(n).length > 1) Uniform(symbolToN(n): _*) else Constant(symbolToN(n)(0))

  def codonToElement(c: List[String]): Element[String] = {
    val clist = c.map { t =>
      val listElem = t.map(s => nucToElement(s.toString()))
      Apply(Inject(listElem: _*), (s: List[String]) => s reduce (_ + _))
    }
    if (clist.size > 1) Uniform(clist: _*) else clist(0)
  }

  def genDNA(seq: AminoAcidSequence): Element[NucleotideSequence] = {
    val codons = seq.seq.map(c => codonToElement(aaToComp(c.toString).toList))
    val concat = Inject(codons: _*)
    Apply(concat, (s: List[String]) => (NucleotideSequence("") /: s)(_ + NucleotideSequence(_)))
  }

  def apply(arg: Element[AminoAcidSequence])(implicit name: Name[NucleotideSequence], collection: ElementCollection) =
    new DNA(name, arg, collection)
}
	
	
