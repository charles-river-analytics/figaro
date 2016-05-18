/*
 * GibbsSolver.scala
 * A Gibbs sampling solver.
 *
 * Created By:      William Kretschmer (kretsch@mit.edu)
 * Creation Date:   Aug 6, 2015
 *
 * Copyright 2015 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.algorithm.structured.solver

import scala.annotation.tailrec
import com.cra.figaro.algorithm.OneTime
import com.cra.figaro.algorithm.factored.factors._
import com.cra.figaro.algorithm.sampling.BaseUnweightedSampler
import com.cra.figaro.algorithm.factored._
import com.cra.figaro.algorithm.structured.Problem
import com.cra.figaro.language.Chain
import com.cra.figaro.algorithm.factored.gibbs.ProbabilisticGibbs
import com.cra.figaro.algorithm.factored.gibbs.Gibbs
import com.cra.figaro.algorithm.factored.gibbs.WalkSAT

class GibbsSolver(problem: Problem, toEliminate: Set[Variable[_]], toPreserve: Set[Variable[_]], _factors: List[Factor[Double]],
   _numSamples: Int,  _burnIn: Int,  _interval: Int, val blockToSampler: Gibbs.BlockSamplerCreator)
  extends BaseUnweightedSampler(null) with ProbabilisticGibbs with OneTime {

  def numSamples() = _numSamples
  def burnIn() = _burnIn
  def interval() = _interval 
  
  def initializeBlocks() = {
    factors = _factors.map(_.mapTo(math.log, semiring))
    variables = factors.flatMap(_.variables).toSet
    val blocks = createBlocks()
    // Create block samplers
    blockSamplers = blocks.map(block => blockToSampler((block, factors.filter(_.variables.exists(block.contains(_))))))
  }

  override def initialize() = {
    super.initialize
    if (blockSamplers == null) initializeBlocks()
    // Initialize the samples to a valid state and take the burn-in samples
    val initialSample = WalkSAT(factors, variables, semiring, chainMapper)
    variables.foreach(v => currentSamples(v) = initialSample(v))
    for (_ <- 1 to burnIn) sampleAllBlocks()
  }

  def chainMapper(chain: Chain[_, _]): Set[Variable[_]] = problem.collection(chain).actualSubproblemVariables.values.toSet

  def run = {}

  def go(): List[Factor[Double]] = {
    initialize()
    val targetVars = toPreserve.toList
    val result = new SparseFactor[Double](targetVars, List())
    for (_ <- 0 until numSamples) {
      for (_ <- 0 until interval) {
        sampleAllBlocks()
      }
      val factorIndex = targetVars.map(currentSamples(_))
      result.set(factorIndex, result.get(factorIndex) + 1)
    }
    List(result.mapTo(_ / numSamples))
  }

  val dependentUniverses = null

  val dependentAlgorithm = null

  val targetElements = null

  def createBlocks(): List[Gibbs.Block] = {
    val variables = factors.flatMap(_.variables).toSet
    val variableParents = problem.collection.variableParents
    // Maps each variable to its deterministic children, i.e. variables that should be included in a block with this variable
    val variableChildren: Map[Variable[_], Set[Variable[_]]] =
      variables.map(v => v -> variables.filter(variableParents(_).contains(v))).toMap

    // Start with the purely stochastic variables with no parents
    val starterVariables = variables.filter(variableParents(_).isEmpty)

    @tailrec // Recursively add deterministic children to the block
    def expandBlock(expand: Set[Variable[_]], block: Set[Variable[_]] = Set()): Gibbs.Block = {
      if (expand.isEmpty) block.toList
      else {
        val expandNext = expand.flatMap(variableChildren(_))
        expandBlock(expandNext, block ++ expand)
      }
    }
    starterVariables.map(v => expandBlock(Set(v))).toList
  }
}
