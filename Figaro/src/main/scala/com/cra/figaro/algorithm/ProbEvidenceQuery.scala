/*
 * ProbEvidenceQuery.scala
 * Algorithms that provide a query for the probability of some named evidence.
 * 
 * Created By:      Lee Kellogg
 * Creation Date:   May 11, 2015
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.algorithm

import com.cra.figaro.language.NamedEvidence

/**
 * Algorithms that provide a query for the probability of some named evidence
 */
trait ProbEvidenceQuery {
  
    /**
      * Compute the probability of the given named evidence.
      * Takes the conditions and constraints in the model as part of the model definition.
      * This method takes care of creating and running the necessary algorithms.
      */
  def probabilityOfEvidence(evidence: List[NamedEvidence[_]]): Double
}