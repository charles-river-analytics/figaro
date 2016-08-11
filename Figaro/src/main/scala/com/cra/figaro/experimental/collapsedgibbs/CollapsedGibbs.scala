/*
 * CollapsedGibbs.scala 
 * An implementation of collapsed Gibbs sampling (on factor graphs) using the algorithm in 
 * "Dynamic Blocking and Collapsing for Gibbs Sampling" Gogate and Venugopal 2013
 * 
 * Created By:    Cory Scott (cscott@cra.com)
 * Creation Date:   June 27, 2016
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */
package com.cra.figaro.experimental.collapsedgibbs

import com.cra.figaro.algorithm.lazyfactored.{ LazyValues, ValueSet }
import scala.annotation.tailrec
import com.cra.figaro.language._
import com.cra.figaro.language.Universe._
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.util._
import com.cra.figaro.algorithm.factored.VariableElimination
import scala.collection.mutable.{ Map => MutableMap, Set => MutableSet}
import com.cra.figaro.algorithm.factored.factors.{ Variable, ElementVariable, InternalChainVariable, Factor}
import com.cra.figaro.algorithm.structured.Problem
import com.cra.figaro.algorithm.factored.factors.factory.Factory
import com.cra.figaro.algorithm.factored.gibbs._
import com.cra.figaro.algorithm.factored.VEGraph
import com.cra.figaro.algorithm.{ AlgorithmException, UnsupportedAlgorithmException }
import com.cra.figaro.library.compound.^^


object CollapsedGibbs {

  // A block is just a list of variables
  type Block = List[Variable[_]]

  // Information passed to BlockSampler constructor
  type BlockInfo = (Block, List[Factor[Double]])

  // Constructor for BlockSampler
  type BlockSamplerCreator = BlockInfo => BlockSampler

  /**
   * Create a one-time collapsed Gibbs sampler using the given number of samples and target elements.
   * This sampler will use the default collapsing strategy (Heuristic) and default params for that strategy.
   */
  def apply(mySamples: Int, targets: Element[_]*)(implicit universe: Universe):CollapsedProbQueryGibbs =
  	this.apply("", List[(Universe, List[NamedEvidence[_]])](),
      (u: Universe, e: List[NamedEvidence[_]]) => () => ProbEvidenceSampler.computeProbEvidence(10000, e)(u),
    mySamples, 0, 1, BlockSampler.default, targets: _*)

  /**
   * Create a one-time collapsed Gibbs sampler using the given strategy for collapsing, number of samples,
   * and target elements.
   * Uses the default params for the strategy.
   */
  def apply(strategy:String, mySamples: Int, targets: Element[_]*)(implicit universe: Universe):CollapsedProbQueryGibbs =
  	this.apply(strategy, List[(Universe, List[NamedEvidence[_]])](),
      (u: Universe, e: List[NamedEvidence[_]]) => () => ProbEvidenceSampler.computeProbEvidence(10000, e)(u),
    mySamples, 0, 1, BlockSampler.default, targets: _*)

  /**
   * Create a one-time collapsed Gibbs sampler using the specified strategy and parameters, and the
   * given number of samples and target elements.
   */
  def apply(strategy:String, collapseParameters:Seq[Int], mySamples: Int, 
  	targets: Element[_]*)(implicit universe: Universe):CollapsedProbQueryGibbs =
  	this.apply(strategy, collapseParameters, List[(Universe, List[NamedEvidence[_]])](),
      (u: Universe, e: List[NamedEvidence[_]]) => () => ProbEvidenceSampler.computeProbEvidence(10000, e)(u),
    mySamples, 0, 1, BlockSampler.default, targets: _*)

  /**
   * Create a one-time collapsed Gibbs sampler using the given number of samples, the number of samples to burn in,
   * the sampling interval, the BlockSampler generator, and target elements.
   */
  def apply(mySamples: Int, burnIn: Int, interval: Int, blockToSampler: BlockSamplerCreator,
  	targets: Element[_]*)(implicit universe: Universe):CollapsedProbQueryGibbs =
  	this.apply("", List[(Universe, List[NamedEvidence[_]])](),
      (u: Universe, e: List[NamedEvidence[_]]) => () => ProbEvidenceSampler.computeProbEvidence(10000, e)(u),
    mySamples, burnIn, interval, blockToSampler, targets: _*)

  /**
   * Create a one-time collapsed Gibbs sampler using the given strategy, number of samples, the number of samples to burn in,
   * the sampling interval, the BlockSampler generator, and target elements.
   * Uses the default params for the strategy.
   */
  def apply(strategy: String, mySamples: Int, burnIn: Int, interval: Int, blockToSampler: BlockSamplerCreator,
  	targets: Element[_]*)(implicit universe: Universe):CollapsedProbQueryGibbs =
  	this.apply(strategy, List[(Universe, List[NamedEvidence[_]])](),
      (u: Universe, e: List[NamedEvidence[_]]) => () => ProbEvidenceSampler.computeProbEvidence(10000, e)(u),
    mySamples, burnIn, interval, blockToSampler, targets: _*)

  /**
   * Create a one-time collapsed Gibbs sampler using the specified strategy and parameters,
   * number of samples, the number of samples to burn in,
   * the sampling interval, the BlockSampler generator, and target elements.
   */
  def apply(strategy: String, collapseParameters:Seq[Int], mySamples: Int, burnIn: Int,
    interval: Int, blockToSampler: BlockSamplerCreator,
  	targets: Element[_]*)(implicit universe: Universe):CollapsedProbQueryGibbs =
  	this.apply(strategy, List[(Universe, List[NamedEvidence[_]])](),
      (u: Universe, e: List[NamedEvidence[_]]) => () => ProbEvidenceSampler.computeProbEvidence(10000, e)(u),
    mySamples, burnIn, interval, blockToSampler, targets: _*)

  /**
   * Create a one-time collapsed Gibbs sampler using the 
   * default strategy with default parameters, given dependent universes and algorithm,
   * the number of samples, the number of samples to burn in,
   * the sampling interval, the BlockSampler generator, and target elements.
   */
  def apply(dependentUniverses: List[(Universe, List[NamedEvidence[_]])],
    dependentAlgorithm: (Universe, List[NamedEvidence[_]]) => () => Double,
    mySamples: Int, burnIn: Int, interval: Int, blockToSampler: BlockSamplerCreator,
    targets: Element[_]*)(implicit universe: Universe):CollapsedProbQueryGibbs =
    this.apply("",
      dependentUniverses,
      dependentAlgorithm,
      mySamples, burnIn, interval, blockToSampler, targets: _*)

  /**
   * Create a one-time collapsed Gibbs sampler using the given strategy, 
   * dependent universes and algorithm,
   * the number of samples, the number of samples to burn in,
   * the sampling interval, the BlockSampler generator, and target elements.
   */
  def apply(strategy:String, dependentUniverses: List[(Universe, List[NamedEvidence[_]])],
    dependentAlgorithm: (Universe, List[NamedEvidence[_]]) => () => Double,
    mySamples: Int, burnIn: Int, interval: Int, blockToSampler: BlockSamplerCreator, targets: Element[_]*)(implicit universe: Universe) =
    strategy match {
    	case "SIMPLE" =>  new CollapsedProbQueryGibbs(universe, targets: _*)(
	      dependentUniverses,
	      dependentAlgorithm,
	      burnIn, interval, blockToSampler) with OneTimeProbQuerySampler with ChainApplyBlockingGibbs {
	      val numSamples = mySamples }
    	case "FACTOR" => new CollapsedProbQueryGibbs(universe, targets: _*)(
	      dependentUniverses,
	      dependentAlgorithm,
	      burnIn, interval, blockToSampler) with OneTimeProbQuerySampler with ChainApplyBlockingGibbs
	    	with FactorSizeCollapseStrategy { val numSamples = mySamples }
    	case "DETERM" => new CollapsedProbQueryGibbs(universe, targets: _*)(
	      dependentUniverses,
	      dependentAlgorithm,
	      burnIn, interval, blockToSampler) with OneTimeProbQuerySampler with ChainApplyBlockingGibbs
	    	with DeterministicCollapseStrategy { val numSamples = mySamples }
    	case "RECURR" => new CollapsedProbQueryGibbs(universe, targets: _*)(
	      dependentUniverses,
	      dependentAlgorithm,
	      burnIn, interval, blockToSampler) with OneTimeProbQuerySampler with ChainApplyBlockingGibbs
	    	with RecurringCollapseStrategy { val numSamples = mySamples }
	    case _ => new CollapsedProbQueryGibbs(universe, targets: _*)(
	      dependentUniverses,
	      dependentAlgorithm,
	      burnIn, interval, blockToSampler) with OneTimeProbQuerySampler with ChainApplyBlockingGibbs
	    	with HeuristicCollapseStrategy { val numSamples = mySamples }
	  }
  /**
   * Create a one-time collapsed Gibbs sampler using the given strategy and parameters, 
   * dependent universes and algorithm,
   * the number of samples, the number of samples to burn in,
   * the sampling interval, the BlockSampler generator, and target elements.
   */
  def apply(strategy:String, collapseParameters:Seq[Int], dependentUniverses: List[(Universe, List[NamedEvidence[_]])],
    dependentAlgorithm: (Universe, List[NamedEvidence[_]]) => () => Double,
    mySamples: Int, burnIn: Int, interval: Int, blockToSampler: BlockSamplerCreator, targets: Element[_]*)(implicit universe: Universe) =
    strategy match {
        /* 
         * In all the constructors below:
         * alpha is the maximum degree of any variable to eliminate.
         * gamma is the maximum number of edges we are allowed to add while eliminating variavles
         */ 
    	case "SIMPLE" =>  {
    		val Seq(alpha, gamma) = collapseParameters
    		new CollapsedProbQueryGibbs(universe, targets: _*)(
	      dependentUniverses,
	      dependentAlgorithm,
	      burnIn, interval, blockToSampler, alpha, gamma) with OneTimeProbQuerySampler with ChainApplyBlockingGibbs {
	      val numSamples = mySamples }
	    }
    	case "FACTOR" => {
        //factorThresh is the maximum total cost of all variables eliminated.
        //when we exceed this value, collapsing stops, even if there are still candidates.
    		val Seq(alpha, gamma, factorThresh) = collapseParameters
    		new CollapsedProbQueryGibbs(universe, targets: _*)(
	      dependentUniverses,
	      dependentAlgorithm,
	      burnIn, interval, blockToSampler, alpha, gamma) with OneTimeProbQuerySampler with ChainApplyBlockingGibbs
	      with FactorSizeCollapseStrategy {
		      val numSamples = mySamples
		      override val factorThreshold = factorThresh
	    	}
	    }
    	case "DETERM" => {
    		val Seq(alpha, gamma) = collapseParameters
    		new CollapsedProbQueryGibbs(universe, targets: _*)(
	      dependentUniverses,
	      dependentAlgorithm,
	      burnIn, interval, blockToSampler, alpha, gamma) with OneTimeProbQuerySampler with ChainApplyBlockingGibbs
	      with DeterministicCollapseStrategy {
	      val numSamples = mySamples }
	    }
    	case "RECURR" => {
        //sampleResetFrequency is the frequency with which we reset and re-collapse the model.
        //sampleSaveFrequency is the frequency with which we store a sample to use in our marginal estimates.
    		val Seq(alpha, gamma, sampleResetFrequency, sampleSaveFrequency) = collapseParameters
    		new CollapsedProbQueryGibbs(universe, targets: _*)(
	      dependentUniverses,
	      dependentAlgorithm,
	      burnIn, interval, blockToSampler, alpha, gamma) with OneTimeProbQuerySampler with ChainApplyBlockingGibbs
	      with RecurringCollapseStrategy { 
	      	val numSamples = mySamples
	      	override val sampleReset = sampleResetFrequency
	      	override val sampleRecurrence = sampleSaveFrequency
	      }
	    }
	    case _ => {
    		val Seq(alpha, gamma) = collapseParameters
    		new CollapsedProbQueryGibbs(universe, targets: _*)(
	      dependentUniverses,
	      dependentAlgorithm,
	      burnIn, interval, blockToSampler, alpha, gamma) with OneTimeProbQuerySampler with ChainApplyBlockingGibbs
	      with HeuristicCollapseStrategy {val numSamples = mySamples }
	    }
	  }

  /**
   * Create an anytime collapsed Gibbs sampler using the given target elements.
   */
  def apply(targets: Element[_]*)(implicit universe: Universe):CollapsedProbQueryGibbs =
  	this.apply("", List[(Universe, List[NamedEvidence[_]])](),
    (u: Universe, e: List[NamedEvidence[_]]) => () => ProbEvidenceSampler.computeProbEvidence(10000, e)(u),
    0, 1, BlockSampler.default, targets: _*)

  /**
   * Create an anytime collapsed Gibbs sampler using the given strategy and target elements.
   */
  def apply(strategy:String, targets: Element[_]*)(implicit universe: Universe):CollapsedProbQueryGibbs =
  	this.apply(strategy, List[(Universe, List[NamedEvidence[_]])](),
    (u: Universe, e: List[NamedEvidence[_]]) => () => ProbEvidenceSampler.computeProbEvidence(10000, e)(u),
    0, 1, BlockSampler.default, targets: _*)

  /**
   * Create an anytime collapsed Gibbs sampler using the given strategy, parameters, and target elements.
   */
  def apply(strategy: String, collapseParameters:Seq[Int], targets: Element[_]*)(implicit universe: Universe):CollapsedProbQueryGibbs =
  	this.apply(strategy, collapseParameters, List[(Universe, List[NamedEvidence[_]])](),
    (u: Universe, e: List[NamedEvidence[_]]) => () => ProbEvidenceSampler.computeProbEvidence(10000, e)(u),
    0, 1, BlockSampler.default, targets: _*)

  /**
   * Create an anytime collapsed Gibbs sampler using the default strategy with default parameters,
   * given number of samples to burn in,
   * the sampling interval, the BlockSampler generator, and target elements.
   */
  def apply(burnIn: Int, interval: Int, blockToSampler: BlockSamplerCreator,
  	targets: Element[_]*)(implicit universe: Universe):CollapsedProbQueryGibbs =
  	this.apply("", List[(Universe, List[NamedEvidence[_]])](),
    (u: Universe, e: List[NamedEvidence[_]]) => () => ProbEvidenceSampler.computeProbEvidence(10000, e)(u),
    burnIn, interval, blockToSampler, targets: _*)

  /**
   * Create an anytime collapsed Gibbs sampler using the given strategy, number of samples to burn in,
   * the sampling interval, the BlockSampler generator, and target elements.
   */
  def apply(strategy:String, burnIn: Int, interval: Int, blockToSampler: BlockSamplerCreator,
  	targets: Element[_]*)(implicit universe: Universe):CollapsedProbQueryGibbs =
  	this.apply(strategy, List[(Universe, List[NamedEvidence[_]])](),
    (u: Universe, e: List[NamedEvidence[_]]) => () => ProbEvidenceSampler.computeProbEvidence(10000, e)(u),
    burnIn, interval, blockToSampler, targets: _*)

  /**
   * Create an anytime collapsed Gibbs sampler using the given number of samples to burn in,
   * the sampling interval, the BlockSampler generator, and target elements.
   */
  def apply(strategy:String, collapseParameters:Seq[Int], burnIn: Int, interval: Int, blockToSampler: BlockSamplerCreator,
  	targets: Element[_]*)(implicit universe: Universe):CollapsedProbQueryGibbs =
  	this.apply(strategy, collapseParameters, List[(Universe, List[NamedEvidence[_]])](),
    (u: Universe, e: List[NamedEvidence[_]]) => () => ProbEvidenceSampler.computeProbEvidence(10000, e)(u),
    burnIn, interval, blockToSampler, targets: _*)

  /**
   * Create an anytime collapsed Gibbs sampler using the default strategy with default paramters,
   * given dependent universes and algorithm,
   * the number of samples to burn in, the sampling interval,
   * the BlockSampler generator, and target elements.
   */
  def apply(dependentUniverses: List[(Universe, List[NamedEvidence[_]])],
    dependentAlgorithm: (Universe, List[NamedEvidence[_]]) => () => Double,
    burnIn: Int, interval: Int, blockToSampler: BlockSamplerCreator,
    targets: Element[_]*)(implicit universe: Universe):CollapsedProbQueryGibbs =
    this.apply("",
      dependentUniverses,
      dependentAlgorithm,
      burnIn, interval, blockToSampler, targets: _*)

   /**
   * Create an anytime collapsed Gibbs sampler using the given strategy (with default parameters), 
   * given dependent universes and algorithm,
   * the number of samples to burn in, the sampling interval,
   * the BlockSampler generator, and target elements.
   */
  def apply(strategy: String, dependentUniverses: List[(Universe, List[NamedEvidence[_]])],
    dependentAlgorithm: (Universe, List[NamedEvidence[_]]) => () => Double,
    burnIn: Int, interval: Int, blockToSampler: BlockSamplerCreator, targets: Element[_]*)(implicit universe: Universe) =
    strategy match {
    	case "SIMPLE" =>  new CollapsedProbQueryGibbs(universe, targets: _*)(
	      dependentUniverses,
	      dependentAlgorithm,
	      burnIn, interval, blockToSampler) with AnytimeProbQuerySampler with ChainApplyBlockingGibbs
    	case "FACTOR" => new CollapsedProbQueryGibbs(universe, targets: _*)(
	      dependentUniverses,
	      dependentAlgorithm,
	      burnIn, interval, blockToSampler) with AnytimeProbQuerySampler with ChainApplyBlockingGibbs
	    	with FactorSizeCollapseStrategy
    	case "DETERM" => new CollapsedProbQueryGibbs(universe, targets: _*)(
	      dependentUniverses,
	      dependentAlgorithm,
	      burnIn, interval, blockToSampler) with AnytimeProbQuerySampler with ChainApplyBlockingGibbs
	    	with DeterministicCollapseStrategy
    	case "RECURR" => new CollapsedProbQueryGibbs(universe, targets: _*)(
	      dependentUniverses,
	      dependentAlgorithm,
	      burnIn, interval, blockToSampler) with AnytimeProbQuerySampler with ChainApplyBlockingGibbs
	    	with RecurringCollapseStrategy
	    case _ => new CollapsedProbQueryGibbs(universe, targets: _*)(
	      dependentUniverses,
	      dependentAlgorithm,
	      burnIn, interval, blockToSampler) with AnytimeProbQuerySampler with ChainApplyBlockingGibbs
	    	with HeuristicCollapseStrategy
	  }

  /**
   * Create an anytime collapsed Gibbs sampler using the given strategy, parameters, 
   * dependent universes and algorithm,
   * the number of samples to burn in, the sampling interval,
   * the BlockSampler generator, and target elements.
   */
  def apply(strategy: String, collapseParameters:Seq[Int], dependentUniverses: List[(Universe, List[NamedEvidence[_]])],
    dependentAlgorithm: (Universe, List[NamedEvidence[_]]) => () => Double,
    burnIn: Int, interval: Int, blockToSampler: BlockSamplerCreator, targets: Element[_]*)(implicit universe: Universe) =
    strategy match {
        /* 
         * In all the constructors below:
         * alpha is the maximum degree of any variable to eliminate.
         * gamma is the maximum number of edges we are allowed to add while eliminating variavles
         */ 
    	case "SIMPLE" =>  {
    		val Seq(alpha, gamma) = collapseParameters
    		new CollapsedProbQueryGibbs(universe, targets: _*)(
	      dependentUniverses,
	      dependentAlgorithm,
	      burnIn, interval, blockToSampler, alpha, gamma) with AnytimeProbQuerySampler with ChainApplyBlockingGibbs
	    }
      //factorThresh is the maximum total cost of all variables eliminated.
      //when we exceed this value, collapsing stops, even if there are still candidates.
    	case "FACTOR" => {
    		val Seq(alpha, gamma, factorThresh) = collapseParameters
    		new CollapsedProbQueryGibbs(universe, targets: _*)(
	      dependentUniverses,
	      dependentAlgorithm,
	      burnIn, interval, blockToSampler, alpha, gamma) with AnytimeProbQuerySampler with ChainApplyBlockingGibbs
	      with FactorSizeCollapseStrategy {
		      override val factorThreshold = factorThresh
	    	}
	    }
    	case "DETERM" => {
    		val Seq(alpha, gamma) = collapseParameters
    		new CollapsedProbQueryGibbs(universe, targets: _*)(
	      dependentUniverses,
	      dependentAlgorithm,
	      burnIn, interval, blockToSampler, alpha, gamma) with AnytimeProbQuerySampler with ChainApplyBlockingGibbs
	      with DeterministicCollapseStrategy
	    }
    	case "RECURR" => {
        //sampleResetFrequency is the frequency with which we reset and re-collapse the model.
        //sampleSaveFrequency is the frequency with which we store a sample to use in our marginal estimates.
    		val Seq(alpha, gamma, sampleResetFrequency, sampleSaveFrequency) = collapseParameters
    		new CollapsedProbQueryGibbs(universe, targets: _*)(
	      dependentUniverses,
	      dependentAlgorithm,
	      burnIn, interval, blockToSampler, alpha, gamma) with AnytimeProbQuerySampler with ChainApplyBlockingGibbs
	      with RecurringCollapseStrategy { 
	      	override val sampleReset = sampleResetFrequency
	      	override val sampleRecurrence = sampleSaveFrequency
	      }
	    }
	    case _ => {
    		val Seq(alpha, gamma, trackSamples) = collapseParameters
    		new CollapsedProbQueryGibbs(universe, targets: _*)(
	      dependentUniverses,
	      dependentAlgorithm,
	      burnIn, interval, blockToSampler, alpha, gamma) with AnytimeProbQuerySampler with ChainApplyBlockingGibbs
	      with HeuristicCollapseStrategy {
	      	override val trackingSamples = trackSamples
	      }
	    }
	  }


  /**
   * Use Gibbs sampling to compute the probability that the given element satisfies the given predicate.
   */
  def probability[T](target: Element[T], predicate: T => Boolean): Double = {
    val alg = CollapsedGibbs(10000, target)
    alg.start()
    val result = alg.probability(target, predicate)
    alg.kill()
    result
  }

  /**
   * Use Gibbs sampling to compute the probability that the given element has the given value.
   */
  def probability[T](target: Element[T], value: T): Double =
    probability(target, (t: T) => t == value)
}

