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
  def continuous = (blockInfo: Gibbs.BlockInfo) => new SimpleBlockSampler(blockInfo) with Cached with GaussianWeight

  /**
   * The factor product block sampler, which uses a VE-like procedure to compute joint factors
   * Works well on large but sparse blocks
   */
  def factorProduct = (blockInfo: Gibbs.BlockInfo) => new SimpleBlockSampler(blockInfo) with Cached with FactorProduct

  /**
   * A combination of continuous and factorProduct
   */
  def continuousFactorProduct = (blockInfo: Gibbs.BlockInfo) => new SimpleBlockSampler(blockInfo) with Cached with GaussianWeight with FactorProduct
}

/**
 *  Class for handling sampling on a block
 */
abstract class BlockSampler(val blockInfo: Gibbs.BlockInfo) {
  val (block, adjacentFactors) = blockInfo

  // Maps a variable to its index in the block
  val indexMap = block.zipWithIndex.toMap

  /**
   * Get the factor from which to sample this block
   */
  def getSamplingFactor(currentSamples: Map[Variable[_], Int]): Factor[Double]

  /**
   * Sample this block once
   */
  def sample(currentSamples: Map[Variable[_], Int]): Unit = {
    // Get the joint factor over variables in this block conditioned on the samples
    val samplingFactor = getSamplingFactor(currentSamples)
    // Ensure the entries sum to 1 for sampling
    normalizeFactor(samplingFactor)
    // Sample from the factor
    val sample =
      if(samplingFactor.isEmpty) sampleZeroProbability()
      else sampleFactor(samplingFactor)
    // Update currentSamples
    samplingFactor.variables.zip(sample).foreach(varAndSample => currentSamples(varAndSample._1) = varAndSample._2)
  }

  /**
   * Select a set of indices in the factor according to the weights in the factor
   */
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

  /**
   * Normalize a factor so its weights sum to 1
   */
  def normalizeFactor(factor: Factor[Double]): Unit = {
    // Sum all of the entries and divide
    val normalizingConstant = factor.foldLeft(0.0, _ + _)
    factor.getIndices.foreach(index => factor.set(index, factor.get(index) / normalizingConstant))
  }

  /**
   * Produce a sample when the Markov blanket indicates zero probability of all states in the block
   * This implementation samples uniformly from the range of each variable in the block
   */
  def sampleZeroProbability(): List[Int] = block.map(v => random.nextInt(v.size))
}

class SimpleBlockSampler(blockInfo: Gibbs.BlockInfo)
  extends BlockSampler(blockInfo) {

  // Indices over possible assignments to variables in the block
  val indices = new Indices(block)

  def getSamplingFactor(currentSamples: Map[Variable[_], Int]): Factor[Double] = computeSamplingFactor(currentSamples)

  def computeSamplingFactor(currentSamples: Map[Variable[_], Int]): Factor[Double] = {
    // Sparse to make sampling faster when variables in the block are correlated
    val result = new SparseFactor[Double](block, List())
    // Loop through all possible assignments of variables in this block
    indices.iterator.foreach(blockIndex => {
      // An index corresponds to an assignment of values to each variable
      // The probability of selecting this index is the product of the probabilities of selecting this index in each adjacent factor
      val prob = adjacentFactors.map(factor => {
        // Have to convert block index into an index in the factor
        val factorIndex = factor.variables.map(v => if(block.contains(v)) blockIndex(indexMap(v)) else currentSamples(v))
        factor.get(factorIndex)
      }).product
      if(prob > 0.0) result.set(blockIndex, prob)
    })
    result
  }
}

/**
 * A VE-like procedure that may work well on large but highly sparse blocks.
 */
trait FactorProduct extends SimpleBlockSampler {
  lazy val subFactors = adjacentFactors.map(factor => {
    // Separate block and Markov blanket variables
    // Markov blanket consists of variables in this factor, but not in the block
    val (blockVars, mbVars) = factor.variables.partition(block.contains(_))
    val blockVarIndices = blockVars.map(factor.variables.indexOf)
    val mbVarIndices = mbVars.map(factor.variables.indexOf)

    // Collect the rows of the factor and group by Markov blanket assignments
    val mbMap = factor.getIndices.map(index => {
      val blockIndex = blockVarIndices.map(index(_))
      val mbIndex = mbVarIndices.map(index(_))
      (mbIndex, blockIndex, factor.get(index))
    }).groupBy(_._1)

    // The result maps a Markov blanket assignment to a sub-factor containing the rows consistent with this assignment
    val result = new SparseFactor[SparseFactor[Double]](mbVars, List())

    mbMap.foreach(pair => {
      val (mbIndex, rows) = pair
      val subResult = new SparseFactor[Double](blockVars, List())
      // Fill the sub-factor with the possible rows in this block
      rows.foreach(row => {
        val (_, blockIndex, prob) = row
        subResult.set(blockIndex, prob)
      })
      result.set(mbIndex, subResult)
    })
    result
  })

  override def computeSamplingFactor(currentSamples: Map[Variable[_], Int]): Factor[Double] = {
    // Extract the rows from each factor and take their product
    val toMultiply: List[Factor[Double]] = subFactors.map(factor => factor.get(factor.variables.map(currentSamples(_))))
    toMultiply.reduce(_.product(_))
  }
}

/**
 * Caches factors according to assignments of values in the Markov blanket, which avoids recomputing the same factors repeatedly
 */
trait Cached extends SimpleBlockSampler {
  lazy val markovBlanket = (adjacentFactors.flatMap(_.variables).toSet -- block.toSet).toList

  // Maps an assignment of values to the Markov blanket to a distribution over variables in the block
  // Implemented as a LinkedHashMap so calling cache.head returns the oldest entry
  lazy val cache: Map[List[Int], Factor[Double]] = LinkedHashMap()

  // Override to limit the cache size
  lazy val maxSize = Int.MaxValue

  override def getSamplingFactor(currentSamples: Map[Variable[_], Int]): Factor[Double] = {
    val key = markovBlanket.map(currentSamples(_))
    cache.get(key) match {
      // Return the cached factor
      case Some(factor) => factor
      // Otherwise compute the factor and store it, resizing the cache as necessary
      case None => {
        val factor = computeSamplingFactor(currentSamples)
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
trait DoubleWeight extends SimpleBlockSampler {
  override val adjacentFactors = blockInfo._2.map(factor => {
    factor match {
      /*
       * The only difference here from the above implementation is that this changes the zero probability states in ConditionalSelector factors
       * i.e. we only make a special case for continuous elements in Chains
       * We technically allow states that are inconsistent according to the factor graphs
       * This is usually not a problem because the variables are continuous, so the states can still be consistent with the model
       * Ultimately this is just an approximation and is not always guaranteed to work
       */
      case _: ConditionalSelector[_] => {
        val List(selector: InternalChainVariable[_, _], result) = factor.variables

        val newFactor = factor.createFactor(List(selector), List(result))
        factor.getIndices.foreach(index => {
          val factorProb = factor.get(index)
          if(factorProb != 0.0) newFactor.set(index, factorProb)
          else {
            val List(selectorIndex, resultIndex) = index
            val selectorExtended = selector.range(selectorIndex)
            val resultExtended = result.range(resultIndex)

            (selectorExtended, resultExtended) match {
              // This pattern matching basically just ensures that both the Chain and result are Doubles
              case (Regular((_, Regular(chainValue: Double))), Regular(resultValue: Double)) =>
                newFactor.set(index, weight(chainValue, resultValue, selector.chain.asInstanceOf[Variable[Double]]))
              case _ => newFactor.set(index, 0.0)
            }
          }
        })
        newFactor
      }
      case _ => factor
    }
  })

  // Function with which to assign probabilities in place of zeros
  // It is assumed that if chainValue == resultValue, the result is 1.0
  def weight(chainValue: Double, resultValue: Double, chain: Variable[Double]): Double
}

/**
 * Assigns probabilities to continuous variables based on a Gaussian PDF
 */
trait GaussianWeight extends DoubleWeight {
  lazy val varianceMap: Map[Variable[_], Double] = Map()

  /*
   * The standard deviation is taken to be the difference between the Chain's minimum and maximum values, divided by the size of the Chain's support
   * This means that, for example, if the possible values of the variable are uniformly distributed accorss its support,
   * then given a value from the variable's support, the next higher or lower value will be approximately 1 standard deviation away
   * This enables good mixing while also making it unlikely to sample when the resultValue and chainValue are far apart
   */
  def weight(resultValue: Double, chainValue: Double, chain: Variable[Double]): Double = {
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