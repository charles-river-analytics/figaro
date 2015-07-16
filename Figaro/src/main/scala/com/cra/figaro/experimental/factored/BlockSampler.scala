/*
 * BlockSampler.scala
 * Classes for sampling blocks as part of Gibbs sampling.
 *
 * Created By:      William Kretschmer (kretsch@mit.edu)
 * Creation Date:   July 14, 2015
 *
 * Copyright 2015 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.experimental.factored

import com.cra.figaro.algorithm.factored.factors._
import com.cra.figaro.algorithm.lazyfactored.{Extended, Regular}
import com.cra.figaro.util._
import scala.annotation.tailrec
import scala.collection.mutable.{LinkedHashMap, Map}

object BlockSampler {
  /**
   * The default block sampler, which caches an unlimited number of factors
   */
  def default = (blockInfo: Gibbs.BlockInfo) => new SimpleBlockSampler(blockInfo) with Cached

  /**
   * The continuous block sampler, which uses a mixture of Gaussians approach when there exist continuous elements that are results of Chains
   */
  def continuous = (blockInfo: Gibbs.BlockInfo) => new SimpleBlockSampler(blockInfo) with Cached with GaussianProb

  /**
   * The continuous block sampler, which replaces zero probability states by a constant in continuous Chain factors
   */
  def constant(d: Double) = (blockInfo: Gibbs.BlockInfo) => new SimpleBlockSampler(blockInfo) with Cached with ConstantProb {val constant = d}
}

/**
 *  Class for handling sampling on a block
 */
abstract class BlockSampler(blockInfo: Gibbs.BlockInfo) {
  val (block, adjacentFactors, variableMap) = blockInfo

  // Indices over possible assignments to variables in the block
  val indices = new Indices(block)
  val allIndices = indices.allIndices

  // Maps a variable to its index in the block
  val indexMap = block.zipWithIndex.toMap

  /**
   * Sample this block once
   */
  def sample(currentSamples: Map[Variable[_], Int]): Unit

  /**
   * Get the factor from which to sample this block
   */
  def getSamplingFactor(currentSamples: Map[Variable[_], Int]): Factor[Double]

  /**
   * Select a set of indices in the factor according to the weights in the factor
   */
  def sampleFactor(factor: Factor[Double]): List[Int]

  /**
   * Normalize a factor so its weights sum to 1
   */
  def normalizeFactor(factor: Factor[Double]): Unit

  /**
   * Compute the probability that the specified index in this block appears in the factor given the currentSamples of the Markov blanket
   * Overwrite e.g. for continuous sampled factors
   */
  def probabilityWithinFactor(factor: Factor[Double], index: List[Int], currentSamples: Map[Variable[_], Int]): Double

  /**
   * Produce a sample when the Markov blanket indicates zero probability of all states in the block
   */
  def sampleZeroProbability(): List[Int]
}

class SimpleBlockSampler(blockInfo: Gibbs.BlockInfo)
  extends BlockSampler(blockInfo) {
  def sampleFactor(factor: Factor[Double]): List[Int] = {
    // This implementation closely mirrors the sampleMultinomial method in figaro.util
    val iterator = factor.getIndices.iterator
    @tailrec
    def helper(index: Double): List[Int] = {
      val next = iterator.next
      val prob = factor.get(next)
      if(index < prob) next
      else helper(index - prob)
    }
    helper(random.nextDouble)
  }

  def normalizeFactor(factor: Factor[Double]): Unit = {
    // Sum all of the entries and divide
    val normalizingConstant = factor.foldLeft(0.0, _ + _)
    factor.getIndices.foreach(index => factor.set(index, factor.get(index) / normalizingConstant))
  }

  def sample(currentSamples: Map[Variable[_], Int]): Unit = {
    // Get the joint factor over variables in this block conditioned on the samples
    val samplingFactor = getSamplingFactor(currentSamples)
    // Sample from the factor
    val sample =
      if(samplingFactor.isEmpty) sampleZeroProbability()
      else sampleFactor(samplingFactor)
    // Update currentSamples
    block.zip(sample).foreach(varAndSample => currentSamples(varAndSample._1) = varAndSample._2)
  }

  def getSamplingFactor(currentSamples: Map[Variable[_], Int]): Factor[Double] = {
    // Sparse to make sampling faster when variables in the block are correlated
    val result = new SparseFactor[Double](block, List())
    // Loop through all possible assignments of variables in this block
    allIndices.foreach(index => {
      // An index corresponds to an assignment of values to each variable
      // The probability of selecting this index is the product of the probabilities of selecting this index in each adjacent factor
      val prob = adjacentFactors.map(factor => probabilityWithinFactor(factor, index, currentSamples)).product
      if(prob > 0.0) result.set(index, prob)
    })
    // Ensure the entries sum to 1 for sampling
    normalizeFactor(result)
    result
  }

  def probabilityWithinFactor(factor: Factor[Double], index: List[Int], currentSamples: Map[Variable[_], Int]): Double = {
    // Get the weight in the factor without performing any modifications
    val factorIndex = variableMap(factor).map(v => if(block.contains(v)) index(indexMap(v)) else currentSamples(v))
    factor.get(factorIndex)
  }

  // When given a zero probability table, this implementation samples uniformly from the range of each variable in the block
  def sampleZeroProbability(): List[Int] = block.map(v => random.nextInt(v.size))
}

/**
 * Caches factors according to assignments of values in the Markov blanket, which avoids recomputing the same factors repeatedly
 */
trait Cached extends SimpleBlockSampler {
  val markovBlanket = (adjacentFactors.flatMap(variableMap(_)).toSet -- block.toSet).toList

  // Maps an assignment of values to the Markov blanket to a distribution over variables in the block
  // Implemented as a LinkedHashMap so calling cache.head returns the oldest entry
  val cache: Map[List[Int], Factor[Double]] = LinkedHashMap()

  // Override to limit the cache size
  val maxSize = Int.MaxValue

  override def getSamplingFactor(currentSamples: Map[Variable[_], Int]): Factor[Double] = {
    val key = markovBlanket.map(currentSamples(_))
    cache.get(key) match {
      // Return the cached factor
      case Some(factor) => factor
      // Otherwise compute the factor and store it, resizing the cache as necessary
      case None => {
        val factor = super.getSamplingFactor(currentSamples)
        if(cache.size == maxSize) cache -= cache.head._1
        cache += key -> factor
        factor
      }
    }
  }
}

/**
 * Specialized sampling for continuous (i.e. of type Double) elements in Chains
 */
trait ContinuousVariables extends SimpleBlockSampler {
  override def probabilityWithinFactor(factor: Factor[Double], index: List[Int], currentSamples: Map[Variable[_], Int]): Double = {
    val factorIndex = variableMap(factor).map(v => if(block.contains(v)) index(indexMap(v)) else currentSamples(v))
    val factorProb = factor.get(factorIndex)

    factor match {
      /*
       * The only difference here from the above implementation is that this changes the zero probability states in ConditionalSelector factors
       * i.e. we only make a special case for continuous elements in Chains
       * We technically allow states that are inconsistent according to the factor graphs
       * This is usually not a problem because the variables are continuous, so the states can still be consistent with the model
       * Ultimately this is just an approximation and is not always guaranteed to work
       */
      case cs: ConditionalSelector[_] => {
        //TODO: this can be optimized by storing the factors and relevant variable information below instead of recomputing every time
        val List((selector: InternalChainVariable[_, _], selectorIndex), (result, resultIndex)) = variableMap(factor).zip(factorIndex)
        val selectorExtended = selector.range(selectorIndex)
        val resultExtended = result.range(resultIndex)

        (selectorExtended, resultExtended) match {
          // This pattern matching basically just ensures that both the Chain and result are Doubles
          case (Regular((_, Regular(chainValue: Double))), Regular(resultValue: Double)) =>
            /*
             * That we only call probability when factorProb == 0.0 means there are two cases where it is not called:
             * -> When chainValue == resultValue, in which case it must hold that factorProb == 1.0 ("cares")
             * -> When chainValue comes from a different result element, in which case it also holds that factorProb == 1.0 ("don't cares")
             */
            if(factorProb == 0.0) probability(chainValue, resultValue, selector.chain.asInstanceOf[Variable[Double]])
            else factorProb
          case _ => factorProb
        }
      }
      case _ => factorProb
    }
  }

  // Function with which to assign probabilities in place of zeros
  // It is assumed that if chainValue == resultValue, the result is 1.0
  def probability(chainValue: Double, resultValue: Double, chain: Variable[Double]): Double
}

trait ConstantProb extends ContinuousVariables {
  val constant: Double

  def probability(resultValue: Double, chainValue: Double, chain: Variable[Double]): Double = constant
}

/**
 * Assigns probabilities to continuous variables based on a Gaussian PDF
 */
trait GaussianProb extends ContinuousVariables {
  val varianceMap: Map[Variable[_], Double] = Map()

  /*
   * The standard deviation is taken to be the difference between the Chain's minimum and maximum values, divided by the size of the Chain's support
   * This means that, for example, if the possible values of the variable are uniformly distributed accorss its support,
   * then given a value from the variable's support, the next higher or lower value will be approximately 1 standard deviation away
   * This enables good mixing while also making it unlikely to sample when the resultValue and chainValue are far apart
   */
  def probability(resultValue: Double, chainValue: Double, chain: Variable[Double]): Double = {
    // Cache the variance because iterating through the range every time is slow and unnecessary
    val variance = varianceMap.get(chain) match {
      case Some(variance) => variance
      case None => {
        val chainMin = chain.range.foldLeft(Double.PositiveInfinity)((min, ext) => if(ext.isRegular) min.min(ext.value) else min)
        val chainMax = chain.range.foldLeft(Double.NegativeInfinity)((max, ext) => if(ext.isRegular) max.max(ext.value) else max)
        val sdEstimate = (chainMax - chainMin) / chain.size
        val varianceEstimate = sdEstimate * sdEstimate
        varianceMap += chain -> varianceEstimate
        varianceEstimate
      }
    }
    val diff = resultValue - chainValue
    // No need to multiply by a constant at the front because we normalize later anyways
    // Moreover, we want this to return 1.0 when diff == 0.0
    math.exp(- (diff * diff) / (2.0 * variance))
  }
}