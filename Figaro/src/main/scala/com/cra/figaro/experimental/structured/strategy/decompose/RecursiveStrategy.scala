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
package com.cra.figaro.experimental.structured.strategy.decompose

import com.cra.figaro.experimental.structured._
import com.cra.figaro.experimental.structured.solver.Solver
import com.cra.figaro.language.Element
import com.cra.figaro.experimental.structured.strategy.solve.SolvingStrategy

/**
 * A recursive strategy that applies the same decomposition strategy at every decomposition
 */
private[figaro] class RecursiveStrategy(problem: Problem, solvingStrategy: SolvingStrategy,
  rangeSizer: RangeSizer, bounds: Bounds, parameterized: Boolean)
  extends StructuredStrategy(problem, solvingStrategy, 
      (p: Problem) => new RecursiveStrategy(p, solvingStrategy, rangeSizer, bounds, parameterized), rangeSizer, bounds, parameterized) {
  
}
