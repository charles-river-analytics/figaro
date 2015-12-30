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

package com.cra.figaro.algorithm.factored.gibbs

import com.cra.figaro.algorithm.factored.factors._
import com.cra.figaro.algorithm.lazyfactored.Regular
import com.cra.figaro.util._
import scala.annotation.tailrec
import scala.collection.mutable.{LinkedHashMap, Map => MutableMap, PriorityQueue}
import com.cra.figaro.algorithm.factored.factors._
import scala.collection.mutable.{Map => MutableMap}

object BlockSampler {
  /**
   * Caches 1000 factors and uses a VE-like procedure to compute joint factors
   */
  def default = (blockInfo: Gibbs.BlockInfo) =>
    new SimpleBlockSampler(blockInfo) with Cached with FactorProduct

  /**
   * Like BlockSampler.default, but also uses Gaussian weighting scheme when there exist Element[Double]s that are results of Chains
   */
  def continuous(_variance: Double) = (blockInfo: Gibbs.BlockInfo) =>
    new SimpleBlockSampler(blockInfo) with Cached with GaussianWeight with FactorProduct { val variance = _variance }

  /**
   * Computes the joint distribution over the block by iterating through every combination of assignments of variables
   * Takes time exponential in the size of the block, and is only recommended for blocks with no determinism
   */
  def joint = (blockInfo: Gibbs.BlockInfo) =>
    new SimpleBlockSampler(blockInfo) with Cached
}

/**
 *  Class for handling sampling on a block
 *  @param blockInfo The pair containing the variables to sample and adjacent factors.
 */
abstract class BlockSampler(val blockInfo: Gibbs.BlockInfo) {
  val (block, adjacentFactors) = blockInfo

  /**
   * Get the factor from which to sample this block
   * Returns a non-logarithmic factor
   */
  def getSamplingFactor(currentSamples: MutableMap[Variable[_], Int]): Factor[Double]

  /**
   * Sample this block once
   */
  def sample(currentSamples: MutableMap[Variable[_], Int]): Unit = {
    // Get the joint factor over variables in this block conditioned on the samples
    val samplingFactor = getSamplingFactor(currentSamples)
    // Sample from the factor
    val sample = sampleFactor(samplingFactor)
    // Update currentSamples
    samplingFactor.variables.zip(sample).foreach(varAndSample => currentSamples(varAndSample._1) = varAndSample._2)
  }

  /**
   * Select a set of indices in the factor according to the weights in the factor
   * Works on a non-logarithmic factor
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
}

class SimpleBlockSampler(blockInfo: Gibbs.BlockInfo)
  extends BlockSampler(blockInfo) {

  // Work in log space to prevent underflow
  val semiring = LogSumProductSemiring()

  // Indices over possible assignments to variables in the block
  val indices = new Indices(block)

  // Maps a variable to its index in the block
  val indexMap = block.zipWithIndex.toMap

  // Override e.g. for caching
  def getSamplingFactor(currentSamples: MutableMap[Variable[_], Int]): Factor[Double] = computeSamplingFactor(currentSamples)

  def computeSamplingFactor(currentSamples: MutableMap[Variable[_], Int]): Factor[Double] = {
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
      }).foldLeft(semiring.one)(semiring.product)
      if(prob != semiring.zero) result.set(blockIndex, prob)
    })
    // Ensure the entries sum to 1 for sampling
    normalizeFactor(result)
  }

  /**
   * Normalize a factor so its weights sum to 1
   * Takes a logarithmic factor and returns a non-logarithmic factor
   */
  def normalizeFactor(factor: Factor[Double]): Factor[Double] = {
    val normalizingConstant = semiring.sumMany(factor.contents.values)
    if(normalizingConstant == semiring.zero) throw new ZeroTotalUnnormalizedProbabilityException
    factor.mapTo((d: Double) => math.exp(d - normalizingConstant), SumProductSemiring())
  }
}

/**
 * A VE-like procedure that works well on large but highly sparse blocks. For each adjacent factor, this
 * groups the rows of the factor into sub-factors according to possible Markov blanket assignments. Each
 * of these sub-factors is accumulated into a factor over factors, which essentially maps the Markov
 * blanket of the original factor to a conditional distribution over the block. This does not store any
 * new information, but rather takes the information in each factor and stores it in an easier to use
 * format. This trait is the reason why we currently choose ''not'' to place a Chain's parent and the
 * Chain itself in the same block, since we cannot efficiently compute the product of the sub-factors of
 * the adjacent ConditionalSelector factors. In other cases we can make use of a priority queue to
 * compute the product in an efficient order. For ConditionalSelectors there is no way to keep the
 * intermediate factors sparse, and computing this product takes exponential time.
 */
trait FactorProduct extends SimpleBlockSampler {
  lazy val mbLookupFactors = adjacentFactors.map(factor => {
    // Separate block and Markov blanket variables
    // Markov blanket consists of variables in this factor, but not in the block
    // Note how the code below allows duplicate variables in factors
    val (blockVarsAndIndices, mbVarsAndIndices) = factor.variables.zipWithIndex.partition(p => block.contains(p._1))
    val (blockVars, blockVarIndices) = blockVarsAndIndices.unzip
    val (mbVars, mbVarIndices) = mbVarsAndIndices.unzip

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
      val subResult = new SparseFactor[Double](blockVars, List(), semiring)
      // Fill the sub-factor with the possible rows in this block
      rows.foreach(row => {
        val (_, blockIndex, prob) = row
        // No reason to keep the zero probability rows when factors are sparse
        if(prob != semiring.zero) subResult.set(blockIndex, prob)
      })
      result.set(mbIndex, subResult)
    })
    result
  })

  lazy val ord = new Ordering[Factor[Double]] {
    def compare(x: Factor[Double], y: Factor[Double]): Int = {
      // Prioritize the factor with the fewest average entries per variable, i.e. most variables per entry
      val xSize = math.pow(x.contents.size, 1.0 / x.numVars.toDouble)
      val ySize = math.pow(y.contents.size, 1.0 / y.numVars.toDouble)
      math.signum(ySize - xSize).toInt
    }
  }

  override def computeSamplingFactor(currentSamples: MutableMap[Variable[_], Int]): Factor[Double] = {
    // Extract the rows from each factor and take their product
    val toMultiply: List[Factor[Double]] = mbLookupFactors.map(factor => factor.get(factor.variables.map(currentSamples(_))))
    /*
     * This priority queue helps ensure that all intermediate products remain sparse. Even though we know
     * the final product should be sparse, it's possible for intermediate factors to become exponentially
     * large in the size of the block if multiplied in the wrong order.
     */
    val queue = PriorityQueue(toMultiply:_*)(ord)
    while(queue.length > 1) {
      val product = queue.dequeue().product(queue.dequeue())
      queue.enqueue(product)
    }
    normalizeFactor(queue.dequeue())
  }
}

/**
 * Caches factors according to assignments of values in the Markov blanket, which avoids recomputing the same factors repeatedly
 * Takes advantage of the fact in Gibbs sampling, nearby samples tend to be highly correlated
 */
trait Cached extends SimpleBlockSampler {
  lazy val markovBlanket = (adjacentFactors.flatMap(_.variables).toSet -- block.toSet).toList

  // Maps an assignment of values to the Markov blanket to a distribution over variables in the block
  // Implemented as a LinkedHashMap so calling cache.head returns the oldest entry
  lazy val cache: MutableMap[List[Int], Factor[Double]] = LinkedHashMap()

  // Override to limit the cache size
  lazy val maxSize = 1000

  override def getSamplingFactor(currentSamples: MutableMap[Variable[_], Int]): Factor[Double] = {
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
 * Specialized sampling for continuous (i.e. of type Double) elements in Chains. This differs from the
 * default sampler in that it can override the zero probability states in ConditionalSelector factors.
 * The idea here is that we allow a technically inconsistent state according to the factors when the
 * value of the Chain and result element are close. This is needed for compound continuous elements
 * because sampling will always produce disjoint ranges for each result element of the Chain, which
 * creates determinism issues with the way we currently block variables. This solution has not been
 * fully tested for accuracy of results.
 */
trait DoubleWeight extends SimpleBlockSampler {
  override val adjacentFactors = blockInfo._2.map(factor => {
    factor match {
      case _: ConditionalSelector[_] => {
        val List(selector, result) = factor.variables

        val newFactor = factor.createFactor(List(selector), List(result))
        factor.getIndices.foreach(index => {
          val factorProb = factor.get(index)
          if(factorProb != semiring.zero) newFactor.set(index, factorProb)
          else {
            val List(selectorIndex, resultIndex) = index
            val selectorExtended = selector.range(selectorIndex)
            val resultExtended = result.range(resultIndex)

            (selectorExtended, resultExtended) match {
              // This pattern matching basically just ensures that both the Chain and result are Doubles
              case (Regular(List(_, Regular(chainValue: Double))), Regular(resultValue: Double)) =>
                newFactor.set(index, logWeight(chainValue, resultValue))
              case _ => newFactor.set(index, semiring.zero)
            }
          }
        })
        newFactor
      }
      case _ => factor
    }
  })

  /**
   * Function with which to assign weights in place of -Infinity. It is assumed that if
   * chainValue == resultValue, the result is 0.0. Observe that setting this function to:
   * {{{if(chainValue == resultValue) 0.0 else Double.NegativeInfinity}}}
   * has the same effect as not using the DoubleWeight trait at all.
   */
  def logWeight(chainValue: Double, resultValue: Double): Double
}

/**
 * Assigns weights to continuous variables based on a Gaussian PDF with a static variance
 */
trait GaussianWeight extends DoubleWeight {
  /**
   * The static variance used to compute the compatibility function
   */
  val variance: Double

  def logWeight(resultValue: Double, chainValue: Double): Double = {
    val diff = resultValue - chainValue
    // No need to multiply by a constant at the front because we normalize later anyways
    // Moreover, we want this to return 0.0 when diff == 0.0
    - (diff * diff) / (2.0 * variance)
  }
}