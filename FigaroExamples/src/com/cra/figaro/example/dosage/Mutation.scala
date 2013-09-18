/*
 * Mutation.scala
 * Represents the mutation of a nucleotide sequence over a given time period using the input transition matrix.
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
import com.cra.figaro.example.dosage._

/**
 * Represents the mutation of a nucleotide sequence over a given time period using
 * the input transition matrix. This is a noncaching chain that generates an element
 * that is a random walk over the sequence
 */
class Mutation(name: Name[NucleotideSequence], seq: Element[NucleotideSequence],
  time: Element[Int], tranMtx: Map[String, List[(Double, String)]], collection: ElementCollection)
  extends NonCachingChain[NucleotideSequence, NucleotideSequence](name, seq, (t1: NucleotideSequence) =>
    new NonCachingChain("", time, (t2: Int) => Mutation.RandWalk(t1, t2, tranMtx), collection), collection) {

}

object Mutation {

  /* One step of the random walk. Each nucleotide in the sequence generates a Select element
    * using the transition matrix (a distribution over the possible changes each nucleotide can make)
    * The elements are then converted to an Element list, and concatenated together to form a 
    * new nucleotide sequence. */
  def step(curr: Element[NucleotideSequence], tranMtx: Map[String, List[(Double, String)]]): Element[NucleotideSequence] = {
    def fcn(n: NucleotideSequence) = {
      val trans = n.seq map (c => Select(tranMtx(c.toString): _*))
      Apply(Inject(trans: _*), (s: List[String]) => (NucleotideSequence("") /: s)(_ + NucleotideSequence(_)))
    }
    NonCachingChain(curr, fcn)
  }

  /* The random walk mutation of a nucleotide sequence. */
  def RandWalk(seq: NucleotideSequence, time: Int, tranMtx: Map[String, List[(Double, String)]]): Element[NucleotideSequence] = {
    if (time == 1) {
      step(Constant(seq), tranMtx)
    } else {
      val prev = RandWalk(seq, time - 1, tranMtx)
      step(prev, tranMtx)
    }
  }

  def apply(seq: Element[NucleotideSequence], time: Element[Int], tranMtx: Map[String, List[(Double, String)]])(implicit name: Name[NucleotideSequence], collection: ElementCollection) = new Mutation(name, seq, time, tranMtx, collection)

}