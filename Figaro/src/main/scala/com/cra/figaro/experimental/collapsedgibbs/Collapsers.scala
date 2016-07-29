/*
 * Collapsers.scala 
 * Three alternative collapsing strategies for collapsed Gibbs sampling. 
 * Each of these is a trait that must be mixed in to a CollapsedProbQueryGibbs.
 * 
 * Created By:    Cory Scott (cscott@cra.com)
 * Creation Date:   July 21, 2016
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


/**
 * HeuristicCollapsedGibbs adds the Hellinger-distance-based term to the elimination heuristic.
 * So the heuristic is now based on the marginal probabilities and pairwise marginals.
 * These have to be estimated by some number of saved samples.
 */
trait HeuristicCollapseStrategy extends CollapsedProbabilisticGibbs {
  var numSamplesSeenSoFar: Int = 0
  var totalSamples: Int = 0
  var marginals: MutableMap[Int,MutableMap[Int,Double]] = MutableMap()
  var pairwiseMarignals: MutableMap[(Int, Int), MutableMap[(Int, Int), Double]] = MutableMap()
  var hellingerDistances: MutableMap[(Int, Int), Double] = MutableMap()
  // trackingSamples is the number of initial samples we take to estimate correlation bewteen variables. 
  val trackingSamples = 200

  override def initialize() = {
    super.initialize()
    totalSamples = 0
    val neededElements = getNeededElements(targetElements, Int.MaxValue)._1
    factors = getFactors(neededElements, targetElements, upperB)
    variables = factors.flatMap(_.variables).toSet
    varsInOrder = variables.toList
    targetVariables = targs.toList.map(x => Variable(x))
    globalGraph = new VEGraph(factors)
    //Initialize the maps that keep track of marginals and pairwise marginals.
    resetMarginals()
    //Get the initial samples.
    for {_ <- 1 to trackingSamples} sampleAllBlocksWithTracking
    //Compute the Hellinger Distances, then use them to collapse some variables.
    updateDistances()
    collapseVariables()
    //Perform update the original blocks to reflect eliminated variables.
    val blocks = correctBlocks(originalBlocks)
    blockSamplers = blocks.map(block => blockSamplerCreate((block,
    	factors.filter(_.variables.exists(block.contains(_))))))
    val initialSample = WalkSAT(factors, variables, semiring, 
    	(chain: Chain[_,_]) => LazyValues(chain.universe).getMap(chain).values.map(Variable(_)).toSet)
		variables.foreach(v => currentSamples(v) = initialSample(v))
    for (_ <- 1 to burnIn) sampleAllBlocks()
  }

  /**
   * Sample all blocks, then store that sample in the marginal and p.m. maps.
   */
  def sampleAllBlocksWithTracking() = {
    super.sampleAllBlocks()
    numSamplesSeenSoFar += 1
    for {testVar <- 0 until varsInOrder.length} {
      val samp1 = currentSamples.getOrElse(varsInOrder(testVar), -1)
      marginals(testVar)(samp1) = marginals(testVar).getOrElse(samp1, 0.0) + 1.0
      // only need to go up to testVar, since we can fill upper and lower triangles of matrix simultaneously.
      for {otherVar <- 0 until testVar} {
        val samp2 = currentSamples.getOrElse(varsInOrder(otherVar),-1)
        pairwiseMarignals((testVar, otherVar))((samp1, samp2)) = pairwiseMarignals((testVar, otherVar)).getOrElse(((samp1, samp2)) ,0.0) + 1.0
        pairwiseMarignals((otherVar, testVar))((samp2, samp1)) = pairwiseMarignals((otherVar, testVar)).getOrElse(((samp2, samp1)) ,0.0) + 1.0
      }
    }
  }

  /**
   * Reset all the marginal and p.m. maps to empty maps.
   */
  def resetMarginals() = {
    numSamplesSeenSoFar = 0
    for {testVar <- 0 until varsInOrder.length} {
      marginals(testVar) = MutableMap()
      for {otherVar <- 0 until testVar} {
        pairwiseMarignals((testVar, otherVar)) = MutableMap()
        pairwiseMarignals((otherVar, testVar)) = MutableMap()
      }
    }
  }

  /**
   * Use the marginal maps to compute Hellinger maps.
   */
  def updateDistances() = {
    for {testVar <- 0 until varsInOrder.length} {
      for {otherVar <- 0 until testVar} {
        val dist = distributionDistance(varsInOrder(testVar), varsInOrder(otherVar))
        hellingerDistances((testVar, otherVar)) = dist
        hellingerDistances((otherVar, testVar)) = dist
      }
    }

  }

  /**
   * Hellinger distance is defined in the source paper (amongst other places).
   * It's the sum over all values of X1 and X2 of (sqrt(P(X1,X2)) - sqrt(P(X1)*P(X2)))^2
   */
  def distributionDistance[T,U](var1: Variable[T], var2: Variable[U]) = {
    var dist = 0.0
    var index1 = varsInOrder.indexOf(var1)
    var index2 = varsInOrder.indexOf(var2)
    for {a <- 0 to var1.range.length} {
      for {b <- 0 to var2.range.length} {
        dist += (math.pow(math.sqrt(pairwiseMarignals.getOrElse((index1, index2), Map[(Int, Int), Double]()).getOrElse((a, b), 0.0)/numSamplesSeenSoFar)
         - math.sqrt(marginals.getOrElse(index1,Map[Int,Double]()).getOrElse(a,0.0)*marginals.getOrElse(index2,Map[Int,Double]()).getOrElse(b,0.0))/numSamplesSeenSoFar, 2.0))
      }
    }
    dist
  }

  /**
   * Compute the score of a given variable.
   */
  override def graphHeuristicFunction[T](var1: Variable[T]) = {
    //get number of edges left to add and distance from this variable to others
    var index = varsInOrder.indexOf(var1)
    var otherIndices = for {v <- 0 until varsInOrder.length if v != index} yield v
    //get the graph-based score from superclass, and add the score from Hellinger Distances
    super.graphHeuristicFunction(var1) + otherIndices.map(x => hellingerDistances.getOrElse((index, x), 0.0)).sum
  }

  /**
   * Perform the collapsing step. 
   */
  override def collapseVariables() = {
    //store the heuristic for every variable, so we don't have to calculate it as often.
    var graphHeuristic:MutableMap[Variable[_], Double] = MutableMap() ++ variables.map(v => v-> graphHeuristicFunction(v)).toMap
    //sort the variables using stored values
    var sortedVars = sortByHeuristic(variables.toList, graphHeuristic)
    var edgesAdded:Int = 0
    //map and tempFactors are to help with variable elimination
    var map = MutableMap[Variable[_], MultiSet[Factor[Double]]]()
    var tempFactors = HashMultiSet[Factor[Double]]()
    factors foreach (x => tempFactors.addOne(x))
    for {fact <- tempFactors} {
      fact.variables foreach (v => map += v -> (map.getOrElse(v, HashMultiSet()).addOne(fact)))
    }
    //we collapse variables until either we are out of candidates or we've added too many edges
    while (sortedVars.length > 0 && edgesAdded < gamma) {
      //eliminate the variable with highest heuristic.
      var toRemove:Variable[_] = sortedVars(0)
      eliminate(toRemove, tempFactors, map)
      variables = variables.filter(_ != toRemove)
      var oldNeighbors = globalGraph.info(toRemove).neighbors.filter(_ != toRemove)
      globalGraph = new VEGraph(tempFactors)
      //update all of the neighbors of the variable we just eliminated, since their scores will have changed.
      for {x <- oldNeighbors} graphHeuristic(x) = graphHeuristicFunction(x)
      /** In this version of collapseVariables, we also have to subtract the Hellinger Distance 
       * between our removed variable and all other variables from each of their scores.
       */
      for {v <- variables.filter(!oldNeighbors.contains(_))} graphHeuristic(v) -= hellingerDistances.getOrElse((varsInOrder.indexOf(toRemove), varsInOrder.indexOf(v)), 0.0)
      sortedVars = sortByHeuristic(variables.toList, graphHeuristic)
    }
    //update the list of factors to reflect the changes we've made.
    //factors = marginalize(makeResultFactor(tempFactors))
    factors = tempFactors.elements
  }

}


/**
 * This trait causes variables to collapsed until the total summed size of all of the factors collapsed
 * thus far exceeds a threshold. 
 */
trait FactorSizeCollapseStrategy extends CollapsedProbabilisticGibbs {

  val factorThreshold:Int = 1000

  override def makeResultFactor(factorsAfterElimination: MultiSet[Factor[Double]]): Factor[Double] = {
  	//println(factorsAfterElimination.map((x:Factor[Double]) => x.size))
  	super.makeResultFactor(factorsAfterElimination)
	}

  /**
   * Collapse variables 
   */
  override def collapseVariables() = {
    //store the heuristic for every variable, so we don't have to calculate it as often.
    var graphHeuristic:MutableMap[Variable[_], Double] = MutableMap() ++ variables.map(v => v-> graphHeuristicFunction(v)).toMap
    //sort the variables using stored values
    var sortedVars = sortByHeuristic(variables.toList, graphHeuristic)
    var edgesAdded:Int = 0
    //map and tempFactors are to help with variable elimination
    val map = MutableMap[Variable[_], MultiSet[Factor[Double]]]()
    var tempFactors = HashMultiSet[Factor[Double]]()
    factors foreach (x => tempFactors.addOne(x))
    for {fact <- tempFactors} {
      fact.variables foreach (v => map += v -> (map.getOrElse(v, HashMultiSet()).addOne(fact)))
    }
    var costSum = 0
    //we collapse variables until either we are out of candidates or we've added too many edges
    while (costSum < factorThreshold && sortedVars.length > 0) {
      //eliminate the variable with highest heuristic.
      //println(tempFactors.map((x:Factor[Double]) => x.variables.map(y => varsInOrder.indexOf(y))))
      var toRemove:Variable[_] = sortedVars(0)
      val cost = map(toRemove).map((x:Factor[Double]) => x.size).product
      //check to see if adding this cost will bring us above threshold. If so, move on to another variable.
      if (cost + costSum < factorThreshold) {
	      eliminate(toRemove, tempFactors, map)
	      var oldNeighbors = globalGraph.info(toRemove).neighbors.filter(_ != toRemove)
	      globalGraph = new VEGraph(tempFactors)
	      variables = variables.filter(_ != toRemove)
		    //update all of the neighbors of the variable we just eliminated, since their scores will have changed.
		    for {x <- oldNeighbors} graphHeuristic(x) = graphHeuristicFunction(x)
	      sortedVars = sortByHeuristic(variables.toList, graphHeuristic)
	      costSum += cost
	    }
	    else {
	      sortedVars = sortByHeuristic(sortedVars.filter(_ != toRemove), graphHeuristic)
    	}
    }
    factors = tempFactors.elements
  }

}



/**
 * Experimental collapser that doesn't need to calculate marginal probabilities.
 * The original paper doesn't distinguish between model variables, or use any meta-information
 * about the variables.
 * Since Figaro knows which variables are deterministic, we can use this as a proxy for the 
 * correlation heuristic.
 */
trait DeterministicCollapseStrategy extends CollapsedProbabilisticGibbs {

  /**
   * For this strategy we need access to the distance maps as well as the blocks.
   */
  var hellingerDistances: MutableMap[(Int, Int), Double] = MutableMap()
  var blocks: List[Gibbs.Block] = _

  /**
   * Unlike other 
   */
  override def initialize() = {
    super.initialize()
    updateDistances()
    collapseVariables()
    //Perform update the original blocks to reflect eliminated variables.
    val blocks = correctBlocks(originalBlocks)
    blockSamplers = blocks.map(block => blockSamplerCreate((block,
    	factors.filter(_.variables.exists(block.contains(_))))))
    val initialSample = WalkSAT(factors, variables, semiring, 
    	(chain: Chain[_,_]) => LazyValues(chain.universe).getMap(chain).values.map(Variable(_)).toSet)
		variables.foreach(v => currentSamples(v) = initialSample(v))
    for (_ <- 1 to burnIn) sampleAllBlocks()
  }

  /**
   * Override the distance function.
   */
  def updateDistances() = {
    blocks = createBlocks()
    for {testVar <- 0 until varsInOrder.length} {
      for {otherVar <- 0 until testVar} {
        var dist = 0.0
        /* Instead of the given definition of Hellinger Distance, use the following distance function:
         * For every block that contains both variables:
         * sum up the difference in index between the first variable and the second variable,
         * normalized by block size.
         * Since blocks are added by starting with stochastic variables and recursively adding
         * children, this should be a rough measure of how much otherVar depends on testVar
         */
        for {b <- blocks} {
          if (b.contains(testVar) && b.contains(otherVar)) {
            dist += (b.indexOf(otherVar) - b.indexOf(testVar))/b.size
          }
        }
        hellingerDistances((testVar, otherVar)) = dist
        hellingerDistances((otherVar, testVar)) = -1.0*dist
      }
    }

  }

  /**
   * Override the heuristic function to use the new measure of distance.
   */
  override def graphHeuristicFunction[T](var1: Variable[T]) = {
    //get number of edges left to add and distance from this variable to others
    var index = varsInOrder.indexOf(var1)
    var otherIndices = for {v <- 0 until varsInOrder.length if v != index} yield v
    super.graphHeuristicFunction(var1) + otherIndices.map(x => hellingerDistances.getOrElse((index, x), 0.0)).sum
  }

}


/**
 * In the paper, the authors recommend updating the marginals every N samples and re-collapsing every few iterations.
 * In practice, this is pretty slow. 
 * This trait will keep a running tally of samples of each of the used variables and re-collapse the factor graph
 * (starting from the initial graph) periodically. 
 */
trait RecurringCollapseStrategy extends HeuristicCollapseStrategy {
  /**
   *  How often we want to re-collapse.
   */
  val sampleReset:Int = 2000

  /**
   * How often we want to save one of our samples for marginal estimation.
   */
  val sampleRecurrence: Int = 50

  /**
   * We override resetMarginals() to do nothing so that we don't get rid of our saved marginal data every time we re-initialize.
   * If we haven't built the marginal maps yes, build them, else do nothing. 
   */
  override def resetMarginals() = {
    if (marginals.isEmpty) {
      super.resetMarginals()
    }
  }

  /**
   * Every sampleRecurrence many samples, we alter the marginals and PairwiseMarginal Maps to record the current sample.
   * Every sampleReset many samples, we re-initialize. 
   */
  override def sampleAllBlocks() = {
    totalSamples += 1
    if ((totalSamples % sampleRecurrence) == 0) {
      sampleAllBlocksWithTracking()
    }
    else if (totalSamples == sampleReset) {
      initialize()
    }
    else {
      super.sampleAllBlocks()
    }
  }
}