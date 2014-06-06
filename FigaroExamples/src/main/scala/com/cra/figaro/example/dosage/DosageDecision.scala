/*
 * DosageDecision.scala
 * The effectiveness of the drug is a function of the dosage.
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

import com.cra.figaro.algorithm._
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.algorithm.decision._
import com.cra.figaro.language._
import com.cra.figaro.library.decision._
import com.cra.figaro.library.compound._
import com.cra.figaro.library.atomic.discrete._
import com.cra.figaro.example.dosage._
import com.cra.figaro.example.dosage.Conversion._
import math.E
import math.pow

/**
 * The effectiveness of the drug is a function of the dosage. The base effectiveness of the drug is if the protein has mutated
 * less than the 10% of the self-protein distance (in the mutation matrices, the distance between the same Amino Acids is not zero)
 * Each dosage of the drug raises the effectiveness threshold
 */

object DosageDecision {

  /* An amino acid sequence. '-' represents any amino acid */
  var protString = "----"

  /* An Element[AminoAcidSequence] that represents a uniform distribution over all possible sequences
     * generated from the original AA string  */
  val protModel = Protein(protString)

  /* An Element[NucleotideSequence] that represents the distribution over the possible
     * nucleotide sequences that can arise from an input AA sequence
     * (there is a one to many mapping from Amino Acids to Nucleotides)  */
  val dnaModel = DNA(protModel)

  /* Base frequencies of each nucleotide */
  val baseFreq = Map[String, Double]("A" -> .38, "G" -> .12, "C" -> .33, "U" -> .17)

  /* Create a transition matrix based on the F81 mutation model with the given
     * base frequencies and overall mutation rate   */
  val f81 = TransitionMatrix.F81(baseFreq, 0.075)

  /* An Element[NucleotideSequence] that represents the random mutation process
     * of a nucleotide sequence, over the specified time steps and using the 
     * specified transition matrix. This is basically a random walk per nucleotide  */
  val dnaMutation = Mutation(dnaModel, Constant(1), f81)

  /* Convert the mutated nucleotide sequence back to an AA sequence */
  val dnaToProtein = Apply(dnaMutation, (n: NucleotideSequence) => toAminoAcidSequence(n))

  /* Boolean Decision that decides whether to give a patient a drug only on the observation
     * of an unmutated AA sequence.
     * 
     * We will use an approximate decision strategy using K as 1% of the collected samples. For this to work, we need to define a 
     * distance between amino acid sequences. This is defined in the AminoAcidSequence class as the alignment distance between the
     *  two sequences, using the Needleman-Wunsh algorithm (edit distance essentially) and the blosum62 distance matrix. */
  val decDefault = Uniform(0, 1, 2)
  val dec = new NonCachingDecision("", protModel, (p: AminoAcidSequence) => decDefault, Universe.universe) with PolicyMaker[AminoAcidSequence, Int] {
    def makePolicy(stratMap: Map[(AminoAcidSequence, Int), DecisionSample]) = DecisionPolicyNN(stratMap, 0.01)
  }

  /* The effectiveness of the drug is a function of the dosage. The base effectiveness of the drug is if the protein has mutated
     * less than the 10% of the self-protein distance (in the mutation matrices, the distance between the same Amino Acids is not zero)
     * Each dosage of the drug raises the effectiveness threshold
     */
  // Compute distance between the original protein and itself (will not be zero)
  val selfDistance = Apply(protModel, (p: AminoAcidSequence) => p.distance(p))
  def effThreshFcn(i: Int) = {
    i match {
      case 0 => 0
      case 1 => .12
      case 2 => .2
    }
  }
  val effThresh = Apply(dec, selfDistance, (i: Int, d: Double) => (1.1 + effThreshFcn(i)) * d)

  // Drug is effective if the distance between the original protein and mutated is less than the threshold
  def effectiveFcn(source: AminoAcidSequence, mut: AminoAcidSequence, thresh: Double) = source.distance(mut) < thresh
  val effective = Apply(protModel, dnaToProtein, effThresh, effectiveFcn)

  /* There is a fixed 10% chance of side effects */
  val sideEffects = Flip(0.1)

  /* Side effects function, which applies a negative utility as the dosage increases */
  val sideEffectsUtil = Apply(sideEffects, dec, (b: Boolean, i: Int) =>
    (b, i) match {
      case (true, 0) => -10.0
      case (true, 1) => -27.0
      case (true, 2) => -39.0
      case _ => 0.0
    })

  /* The overall utility of the decision network.
     *  If the drug is effective, there is a positive utility, and zero otherwise
     *  We add that to the utility of the side effects
      */
  val effUtil = Apply(effective, (b: Boolean) => if (b) 15.0 else 0.0)
  val util = Apply(effUtil, sideEffectsUtil, (d1: Double, d2: Double) => d1 + d2)

  def main(args: Array[String]): Unit = {

    /* Create custom proposal scheme that always chooses a new value for the decision, then 
       * randomly selects another element */
    val Scheme = UntypedScheme(() => dec, Some(ProposalScheme.default))

    val decmh = DecisionMetropolisHastings(100000, Scheme, 5000, List(util), dec)

    var protString = "P---"

    decmh.start()
    decmh.setPolicy(dec)

    val policy = dec.getPolicy(AminoAcidSequence("PLAY"))
    println(policy)

    decmh.kill
  }

}







