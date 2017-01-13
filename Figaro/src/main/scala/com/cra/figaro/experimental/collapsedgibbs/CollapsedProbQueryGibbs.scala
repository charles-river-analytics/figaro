/*
 * CollapsedProbQueryGibbs.scala 
 * Core class for the collapsed Gibbs sampler. All other Collapsed Gibbs samplers extend this class,
 * with traits mixed in to specify strategy.
 * 
 * Created By:    Cory Scott (cscott@cra.com)
 * Creation Date:   July 25, 2016
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
 * CollapsedProbQueryGibbs only uses graph information and the list of targets to collapse some variables.
 * extend with HeuristicCollapser or RecurringCollapser to implement other features described in Gogate et. al. 
 */
abstract class CollapsedProbQueryGibbs(override val universe: Universe, targets: Element[_]*)(
override val dependentUniverses: List[(Universe, List[NamedEvidence[_]])],
override val dependentAlgorithm: (Universe, List[NamedEvidence[_]]) => () => Double,
override val burnIn: Int, override val interval: Int,
override val blockToSampler: Gibbs.BlockSamplerCreator, alphaIn: Int = 10, gammaIn:Int = 1000, 
	upperBounds: Boolean = false)
extends ProbQueryGibbs(universe, targets: _*)(dependentUniverses, dependentAlgorithm, 0, interval, 
		blockToSampler, upperBounds)
with CollapsedProbabilisticGibbs {
  override def initialize() = {
    super.initialize()
    //need to create the original blocks before we eliminate any variables
    originalBlocks = createBlocks()
    alpha = alphaIn
    gamma = gammaIn
    targs = targets
    alphaChoose2 = math.max((alpha*(alpha-1.0))/2.0, 1.0)
    varsInOrder = variables.toList
    targetVariables = targets.toList.map(x => Variable(x))
    globalGraph = new VEGraph(factors)
    collapseVariables()
    //create the correct blocks by filtering out eliminated variables
    val blocks = correctBlocks(originalBlocks)
    //this is all the same as in ProbQueryGibbs
    blockSamplerCreate = blockToSampler
    blockSamplers = blocks.map(block => 
    	blockToSampler((block, factors.filter(_.variables.exists(block.contains(_))))))
    val initialSample = WalkSAT(factors, variables, semiring, chainMapper)
		variables.foreach(v => currentSamples(v) = initialSample(v))
    for (_ <- 1 to burnIn) sampleAllBlocks()
  }
}

trait CollapsedProbabilisticGibbs extends ProbabilisticGibbs {
  /**
   * We need a list of variables in order so we can access them by index.
   */
  var varsInOrder: List[Variable[_]] = List()


  var originalBlocks:List[Gibbs.Block] = List()
  /**
   * List of variables corresponding to target elements.
   * Creating these is memoized, so we don't need to worry about duplicates.
   */
  var targetVariables: List[Variable[_]] = List()
  
  /**
   * Only variables with alpha or fewer neighbors in the primal graph are candidates for collapsing.
   */
  var alpha:Int = _
  /*
  gamma controls the trade-off between blocking and collapsing. See collapseVariables.
  */
  var gamma:Int = _


  /**
   * We use ( alpha C 2 ) often, may as well store it.
   */
  var alphaChoose2:Double = _


  /**
   * globalGraph lets us traverse the primal graph.
   */
  var globalGraph: VEGraph = _


  /**
   * Store which elements are our target variables so that subclasses can make use of them. 
   */
  var targs:Seq[Element[_]] = _

  /**
   * Store which elements are our target variables so that subclasses can make use of them. 
   */
  var upperB:Boolean = _

  /*
   * Keep the BlockSamplerCreator for later use.
   */
  var blockSamplerCreate: Gibbs.BlockSamplerCreator = _

  /**
   * Returns how many edges would be added to the primal graph by removing var1. 
   * Note: this is number of edges added, NOT net edges added and removed.
   * Source paper is somewhat ambiguous on whether this should be added or net. 
   */  
  def graphTerm[T](var1: Variable[T]):Double = {
    //val (updatedGraph, trash) = globalGraph.eliminate(var1)
    val updatedGraph = globalGraph.eliminate(var1)._1
    val graphTerm = (variables.filter(x => x != var1).toList.map(varT => updatedGraph.info(varT).neighbors.toList.length - 1 ).sum/2  
      - variables.filter(x => x != var1).toList.map(varT => (globalGraph.info(varT).neighbors.filter(x => x != var1).toList.length - 1) ).sum/2)
    graphTerm
  }


  /**
	 * We want to alter the original blocks so that we filter out any variables which have
	 * been eliminated. If the original blocks overlapped a lot, then there'll be a lot of 
	 * redundancy in the filtered blocks, so we take a further step of eliminating any block
	 * xs which is fully contained in another block ys.
   */
  def correctBlocks(originalBlocks:List[Gibbs.Block]):List[Gibbs.Block] = {
  	val initial = MutableSet[Set[Variable[_]]]()
  	for {x <- originalBlocks.map(bl => bl.filter(y => variables.contains(y)).toSet).distinct} {
  		initial add x
  	}
  	for {xs <- initial} {
  		for {ys <- initial}{
  			//println("&" + xs + " " + ys)
  			if ((xs subsetOf ys) && (xs != ys)) {
  				initial remove xs
  			}
  		}
  	}
  	initial.map(x => x.toList).toList
  }

  /**
   * The heuristic of a node is how many edges would be added to the primal graph by removing that variable.
   * Because we make a clique over the variable's neighbors.
   * Since we only eliminate variables with alpha or fewer neighbors, this is capped at (alpha C 2).
   * So we return the number of edges as a percentage of (alpha C 2).
   */
  def graphHeuristicFunction[T](var1: Variable[T]):Double = {
    ((alphaChoose2 - graphTerm(var1))/alphaChoose2)
  }

  /**
   * Eliminate a variable. This follows the same approach as in VariableElimination.scala.
  }*/
	def eliminate(
    variable: Variable[_],
    factors: MultiSet[Factor[Double]],
    map: MutableMap[Variable[_], MultiSet[Factor[Double]]]): Unit = {
    val varFactors = map(variable)
    if (varFactors nonEmpty) {
    	//flatten all of varFactors into one factor
      val productFactor = varFactors reduceLeft (_.product(_))
      //marginalize that factor to all variables other than variable
      val resultFactor = productFactor.marginalizeTo( 
      	productFactor.variables.filter(_ != variable): _*)
      //update our multiset of factors, and our map variables ->: factors
      varFactors.foreach(factors.removeOne(_))
      factors.addOne(resultFactor)
      varFactors.foreach(removeFactor(_, map))
      map -= variable
      addFactor(resultFactor, map)
    }
  }


  /**
   * Marginalize a factor to a particular variable. 
   */
  def marginalizeToTarget(factor: Factor[Double], target: Variable[_]) = {
    val unnormalizedTargetFactor = factor.marginalizeTo(target)
    val z = unnormalizedTargetFactor.foldLeft(semiring.zero, _ + _)
    //val targetFactor = Factory.make[Double](unnormalizedTargetFactor.variables)
    val targetFactor = unnormalizedTargetFactor.mapTo((d: Double) => d )
    targetFactor
  }

  /**
   * Marginalize all factors to their component variables. 
   */
  def marginalize(resultFactor: Factor[Double]) =
    variables.map(marginalizeToTarget(resultFactor, _)).toList

  /**
   * Combine all the remaining factors into one 'result factor', as in VE.
   */
  def makeResultFactor(factorsAfterElimination: MultiSet[Factor[Double]]): Factor[Double] = {
    // It is possible that there are no factors (this will happen if there are  no queries or evidence).
    // Therefore, we start with the unit factor and use foldLeft, instead of simply reducing the factorsAfterElimination.
    factorsAfterElimination.foldLeft(Factory.unit(semiring))(_.product(_))
  }

  /**
   * add a factor to the list
   */
  def addFactor[T](factor: Factor[T], map: MutableMap[Variable[_], MultiSet[Factor[T]]]): Unit =
    factor.variables foreach (v => map += v -> (map.getOrElse(v, HashMultiSet()).addOne(factor)))

  /**
   * remove a factor from the list
   */
  def removeFactor[T](factor: Factor[T], map: MutableMap[Variable[_], MultiSet[Factor[T]]]): Unit =
    factor.variables foreach (v => map += v -> (map.getOrElse(v, HashMultiSet()).removeOne(factor)))


  /**
   * Sort variables by the target heuristic, if they have fewer than alpha neighbors and are not targets.
   */
  def sortByHeuristic(varList:List[Variable[_]], HeuristicMap:MutableMap[Variable[_], Double]) = {
    varList.filter(x => !targetVariables.contains(x) && 
    	(globalGraph.info(x).neighbors.toList.length - 1) <= alpha).sortWith(HeuristicMap(_) > HeuristicMap(_))
  }
  /**
   * Perform the collapsing step. 
   */
  def collapseVariables() = {
    //store the heuristic for every variable, so we don't have to calculate it as often.
    val graphHeuristic:MutableMap[Variable[_], Double] = MutableMap() ++ variables.map(v => 
    	v-> graphHeuristicFunction(v)).toMap
    //sort the variables using stored values
    var sortedVars = sortByHeuristic(variables.toList, graphHeuristic)
    var edgesAdded:Int = 0
    //map and tempFactors are to help with variable elimination.
    val map = MutableMap[Variable[_], MultiSet[Factor[Double]]]()
    val tempFactors = HashMultiSet[Factor[Double]]()
    factors foreach (x => tempFactors.addOne(x))
    for {fact <- tempFactors} {
      fact.variables foreach (v => map += v -> (map.getOrElse(v, HashMultiSet()).addOne(fact)))
    }
    //we collapse variables until either we are out of candidates or we've added too many edges.
    while (sortedVars.length > 0 && edgesAdded < gamma) {
      //eliminate the variable with highest heuristic.
      var toRemove:Variable[_] = sortedVars(0)
      eliminate(toRemove, tempFactors, map)
      variables = variables.filter(_ != toRemove)
      var oldNeighbors = globalGraph.info(toRemove).neighbors.filter(_ != toRemove)
      globalGraph = new VEGraph(tempFactors)
      //update all of the neighbors of the variable we just eliminated, since their scores will have changed.
      for {x <- oldNeighbors} graphHeuristic(x) = graphHeuristicFunction(x)
      sortedVars = sortByHeuristic(variables.toList, graphHeuristic)
    }
    //update the list of factors to reflect the changes we've made.
    factors = tempFactors.elements
  }
}



