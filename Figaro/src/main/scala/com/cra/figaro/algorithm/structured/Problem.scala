/*
 * Problem.scala
 * An inference problem to be solved as a single unit by a solver.
 *
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   March 1, 2015
 *
 * Copyright 2015 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.algorithm.structured

import com.cra.figaro.algorithm.factored.factors._
import com.cra.figaro.language._
import com.cra.figaro.algorithm.structured.strategy.solve.SolvingStrategy

/**
 * A Problem defines an inference problem to be solved.
 * It includes a set of components directly contained in the problem.
 * These components might have nested subproblems.
 * It also refers to global components that are outside of this problem.
 * The targets are elements that appear in this problem that are visible outside.
 * They might be newly defined in this problem or they might be defined previously, but either way, they should not be eliminated.
 */
class Problem(val collection: ComponentCollection, val targets: List[Element[_]] = List()) {
  /**
   *  Components outside of this problem that appear in the solution to this problem.
   */
  var globals: Set[ProblemComponent[_]] = Set()

  /**
   *  Components directly defined in this problem.
   */
  var components: List[ProblemComponent[_]] = List()

  /**
   *  Factors over globals produced by solving the problem.
   */
  var solution: List[Factor[Double]] = List()
  
  /**
   *  A map for each variable that indicates the value of the variable that is maximal for each possible value of the interface
   *  The support of each factor is over the product of the supports of the interface variables
   *  
   */
  var recordingFactors: Map[Variable[_], Factor[_]] = Map()
  
  /**
   * A flag indicating whether the problem has been solved.
   */
  var solved: Boolean = false
  
  val componentsToVisit: scala.collection.mutable.Set[ProblemComponent[_]] = scala.collection.mutable.Set()

  /**
   * Add a component for the given element to this problem.
   */
  def add[T](element: Element[T]): ProblemComponent[T] = collection.add(element, this)

  /**
   *  Determines if a variable is internal to this problem and should be eliminated
   */
  def internal(variable: Variable[_]): Boolean = {
    collection.intermediates.contains(variable) || {
      val component = collection.variableToComponent(variable)
      contains(component.problem) & !targets.contains(component.element)
    }
  }
  
  /**
   * Determines if a variable is in scope outside of this problem
   */
  def global(variable: Variable[_]): Boolean = {
    !collection.intermediates.contains(variable) && 
    !contains(collection.variableToComponent(variable).problem)
  }
  

  /**
   * Determines if this problem contains the given problem.
   * Any variables in the contained problem should also be eliminated when this problem is solved.
   */
  def contains(otherProblem: Problem): Boolean = {
    def componentContains(component: ProblemComponent[_]): Boolean = {
      component match {
        case c: ChainComponent[_,_] => c.subproblems.values.exists(_.contains(otherProblem))
        case _ => false
      }
    }
    otherProblem == this || components.exists(componentContains(_))
  }


  /**
   * Solve the problem defined by all the components' current factors.
   * This will also set the globals accordingly.
   * All components in this problem and contained subproblems should be eliminated in the solution.
   */
  def solve( strategy: SolvingStrategy, bounds: Bounds = Lower) {
    val targetComponents = targets.map(collection(_))    
    val allFactors = components.flatMap(c => c.nonConstraintFactors ::: c.constraintFactors(bounds))
    val allVariables = (Set[Variable[_]]() /: allFactors)(_ ++ _.variables)
    val (toEliminate, toPreserve) = allVariables.partition(internal(_))
    globals = toPreserve.map(collection.variableToComponent(_))
    val (solution, recordingFactors) = strategy.solve(this, toEliminate, toPreserve, allFactors)
    this.solution = solution
    this.recordingFactors = recordingFactors
    solved = true
    toEliminate.foreach((v: Variable[_]) => {
      if (collection.intermediates.contains(v)) collection.intermediates -= v
    })
  }

  /**
   * Produce a single weighted sample of all the elements in this problem.
   */
  def sample(): (Map[Element[_], _], Double) = {
    (Map(), 1.0)
  }

  targets.foreach(target => if (!collection.contains(target)) add(target))
}

class NestedProblem[T](collection: ComponentCollection, val target: Element[T]) extends Problem(collection, List(target))
