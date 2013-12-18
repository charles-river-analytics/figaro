/*
 * FactoredAlgorithm.scala
 * Trait for algorithms that use factors
 * 
 * Created By:      Michael Howard (mhoward@cra.com)
 * Creation Date:   Jul 1, 2013
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.algorithm.factored

import com.cra.figaro.algorithm._
import com.cra.figaro.language._
import com.cra.figaro.library.decision._
import com.cra.figaro.util._
import annotation.tailrec
import scala.collection._

/**
 * Trait for algorithms that use factors.
 */
trait FactoredAlgorithm[T] extends Algorithm {

  /**
   * All implementations of factored algorithms must specify a way to get the factors from the given universe and
   * dependent universes.
   */
  def getFactors(targetVariables: Seq[Variable[_]]): List[Factor[T]]
  
  /**
   * The universe on which this variable elimination algorithm should be applied.
   */
  val universe: Universe
  
  /**
   * A list of universes that depend on this universe such that evidence on those universes should be taken into
   * account in this universe.
   */
  val dependentUniverses: List[(Universe, List[NamedEvidence[_]])]

  /**
   * The algorithm to compute probability of specified evidence in a dependent universe.
   * We use () => Double to represent this algorithm instead of an instance of ProbEvidenceAlgorithm. 
   * Typical usage is to return the result of ProbEvidenceAlgorithm.computeProbEvidence when invoked.
   */
  val dependentAlgorithm: (Universe, List[NamedEvidence[_]]) => () => Double

  /**
   * The sum, product operations on the factor types and
   * appropriate values for zero and one must be defined.
   */
  val semiring: Semiring[T]
}





