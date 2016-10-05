/*
 * FullDecompositionStrategy.scala
 * Strategies that fully decompose a problem.
 *
 * Created By:      William Kretschmer (kretsch@mit.edu)
 * Creation Date:   Sep 27, 2016
 *
 * Copyright 2016 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */
package com.cra.figaro.algorithm.structured.strategy.refine

import com.cra.figaro.algorithm.structured.{Bounds, NestedProblem, Problem, ProblemComponent}

import scala.collection.mutable

class FullDecompositionStrategy(problem: Problem, rangeSizer: RangeSizer, bounds: Bounds, parameterized: Boolean,
                                done: mutable.Set[ProblemComponent[_]] = mutable.Set())
  extends DecompositionStrategy(problem, rangeSizer, bounds, parameterized, done) {

  override def recurse(nestedProblem: NestedProblem[_]): Option[DecompositionStrategy] = {
    Some(new FullDecompositionStrategy(nestedProblem, rangeSizer, bounds, parameterized, done))
  }
}
