/*
 * Decomposable.scala
 * A trait for indicating decomposable classes
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

import com.cra.figaro.algorithm.structured.ProblemComponent

/**
 * Decomposable trait. This trait can be added to elements or other decomposable classes that indicates to a 
 * decomposing strategy how to process this ProblemComponent
 */
trait Decomposable {
	def process(first: ProblemComponent[_], rest: List[ProblemComponent[_]], done: Set[ProblemComponent[_]]): Set[ProblemComponent[_]]
}