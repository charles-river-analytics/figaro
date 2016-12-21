/*
 * RecursiveStrategy.scala
 * A strategy that fully expands a problem and solves the subproblems in a structured manner.
 *
 * Created By:      Brian Ruttenberg (bruttenberg@cra.com)
 * Creation Date:   July 1, 2015
 *
 * Copyright 2015 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */
package com.cra.figaro.algorithm.structured.strategy.decompose

import com.cra.figaro.algorithm.structured.strategy.solve.SolvingStrategy
import com.cra.figaro.algorithm.structured._

/**
 * A recursive strategy that applies the structured strategy at every decomposition
 */
private[figaro] class RecursiveStructuredStrategy(problem: Problem, solvingStrategy: SolvingStrategy,
  rangeSizer: RangeSizer, bounds: Bounds, parameterized: Boolean)
  extends StructuredStrategy(problem, solvingStrategy, 
      (p: Problem) => new RecursiveStructuredStrategy(p, solvingStrategy, rangeSizer, bounds, parameterized), rangeSizer, bounds, parameterized) { 
  problem.collection.useSingleChainFactor = true
}

/**
 * A recursive strategy that applies the flatten strategy at every decomposition.
 */
private[figaro] class RecursiveFlattenStrategy(problem: Problem, solvingStrategy: SolvingStrategy,
  rangeSizer: RangeSizer, bounds: Bounds, parameterized: Boolean)
  extends FlattenStrategy(problem, solvingStrategy, 
      (p: Problem) => new RecursiveFlattenStrategy(p, solvingStrategy, rangeSizer, bounds, parameterized), rangeSizer, bounds, parameterized) { 
}

/**
 * A recursive strategy that applies the Raising strategy at every decomposition. 
 */
private[figaro] class RecursiveRaisingStrategy(problem: Problem, solvingStrategy: SolvingStrategy, raisingCriteria: (Problem, Problem) => Boolean,
  rangeSizer: RangeSizer, bounds: Bounds, parameterized: Boolean)
  extends RaisingStrategy(problem, solvingStrategy, 
      (p: Problem) => new RecursiveRaisingStrategy(p, solvingStrategy, raisingCriteria, rangeSizer, bounds, parameterized), raisingCriteria, rangeSizer, bounds, parameterized) { 
}