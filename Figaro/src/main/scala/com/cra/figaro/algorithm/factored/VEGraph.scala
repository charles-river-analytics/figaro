/*
 * VEGraph.scala
 * Induced graph for variable elimination.
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Jan 1, 2009
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.algorithm.factored

import com.cra.figaro.algorithm._
import scala.collection.mutable.PriorityQueue

/**
 * Abstract factors with no rows associated with variables.
 */
case class AbstractFactor(variables: List[Variable[_]])

/**
 * Information associated with a variable during variable elimination, including the factors to which it
 * belongs and variables with which it shares a factor.
 * 
 * @param factors The abstract factors to which this variable belongs
 * @param neighbors The variables that share a factor in common with this variable.
 */
case class VariableInfo(factors: Set[AbstractFactor], neighbors: Set[Variable[_]])

/**
 * Induced graph for variable elimination.
 * 
 * @param info A map from variables to information about the variables describing the 
 * factors to which it belongs and its neighbors.
 */
class VEGraph private (val info: Map[Variable[_], VariableInfo]) {
  /**
   * Create the initial induced graph from a set of factors.
   */
  def this(factors: Traversable[Factor[_]]) = this(VEGraph.makeInfo(factors))

  /**
   * Returns the new graph after eliminating the given variable. This includes a factor involving all the
   * variables appearing in a factor with the eliminated variable, and excludes all factors in which the
   * eliminated variable appears.
   */
  def eliminate(variable: Variable[_]): VEGraph = {
    val VariableInfo(oldFactors, allVars) = info(variable)
    val newFactor = AbstractFactor((allVars - variable).toList)
    var newInfo = VEGraph.makeInfo(info, List(newFactor), oldFactors)
    def removeNeighbor(neighbor: Variable[_]) = {
      val VariableInfo(oldNeighborFactors, oldNeighborNeighbors) = newInfo(neighbor)
      newInfo += neighbor -> VariableInfo(oldNeighborFactors, oldNeighborNeighbors - variable)
    }
    newInfo(variable).neighbors foreach (removeNeighbor(_))
    (new VEGraph(newInfo))
  }

  /**
   * Returns the elimination score, which is the increase in cost between the new factor involving the
   * variable and the existing factors (we want to minimize score).
   */
  def score(variable: Variable[_]): Double = {
    val VariableInfo(oldFactors, allVars) = info(variable)
    val oldCost = VEGraph.cost(oldFactors)
    val newFactor = AbstractFactor((allVars - variable).toList)
    val newCost = VEGraph.cost(newFactor)
    newCost - oldCost
    //Experimental: what if we just consider the new cost?
    //newCost
  }
}

object VEGraph {
  /**
   * The cost of a factor is the number of entries in it, which is the product of the ranges of its variables.
   */
  def cost(factor: AbstractFactor): Double =
    (1.0 /: factor.variables)(_ * _.size.toDouble)

  /**
   * The cost of a set of factors is the sum of the costs of the individual factors.
   */
  def cost(factors: Traversable[AbstractFactor]): Double =
    (0.0 /: factors)(_ + cost(_))

  private def makeInfo(factors: Traversable[Factor[_]]): Map[Variable[_], VariableInfo] =
    makeInfo(Map(), factors map ((f: Factor[_]) => AbstractFactor(f.variables)), List())

  private def makeInfo(initialInfo: Map[Variable[_], VariableInfo],
    factorsToAdd: Traversable[AbstractFactor],
    factorsToRemove: Traversable[AbstractFactor]): Map[Variable[_], VariableInfo] = {
    var info: Map[Variable[_], VariableInfo] = initialInfo
    def addFactorToVariable(factor: AbstractFactor, variable: Variable[_]): Unit = {
      val oldInfo = info.getOrElse(variable, VariableInfo(Set(), Set()))
      val newFactors = oldInfo.factors + factor
      val newNeighbors = oldInfo.neighbors ++ factor.variables
      info += variable -> VariableInfo(newFactors, newNeighbors)
    }
    def removeFactorFromVariable(factor: AbstractFactor, variable: Variable[_]) = {
      val oldInfo = info.getOrElse(variable, VariableInfo(Set(), Set()))
      val newFactors = oldInfo.factors - factor
      info += variable -> VariableInfo(newFactors, oldInfo.neighbors)
    }
    def addFactor(factor: AbstractFactor) = factor.variables foreach (addFactorToVariable(factor, _))
    def removeFactor(factor: AbstractFactor) = factor.variables foreach (removeFactorFromVariable(factor, _))
    factorsToAdd foreach (addFactor(_))
    factorsToRemove foreach (removeFactor(_))
    info
  }
}
