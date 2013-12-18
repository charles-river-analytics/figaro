/*
 * OpenUniverse.scala
 * An example of open universe reasoning.
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Jan 1, 2009
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.example

import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.language._
import com.cra.figaro.library.atomic.continuous.{ Uniform, Normal }
import com.cra.figaro.library.atomic.discrete.{ Geometric, FromRange }
import com.cra.figaro.library.compound._
import com.cra.figaro.language.Universe._
import com.cra.figaro.util._

/**
 * An example of open universe reasoning.
 */
object OpenUniverse {
  createNew()

  private def source(): Element[Double] = Uniform(0.0, 1.0)

  private val numSources = Geometric(0.9)

  private val sources = MakeList(numSources, source _)

  private class Sample {
    val sourceNum = IntSelector(numSources)
    val source = Apply(sources, sourceNum, (s: Seq[Double], i: Int) => s(i))
    val position = NonCachingChain(source, (x: Double) => Normal(x, 1.0))
  }

  private val sample1 = new Sample
  private val sample2 = new Sample
  private val samples = List(sample1, sample2)
  private val numSamples = samples.length

  private val equal = sample1.source === sample2.source

  def chooseScheme(): ProposalScheme =
    DisjointScheme(
      (0.25, () => ProposalScheme(numSources)),
      (0.25, () => ProposalScheme(sources.items(random.nextInt(numSources.value)))),
      (0.25, () => ProposalScheme(samples(random.nextInt(numSamples)).sourceNum)),
      (0.25, () => ProposalScheme(samples(random.nextInt(numSamples)).position.resultElement)))

  def main(args: Array[String]) = {
    sample1.position.addCondition((y: Double) => y >= 0.7 && y < 0.8)
    sample2.position.addCondition((y: Double) => y >= 0.7 && y < 0.8)
    val alg = MetropolisHastings(100000, chooseScheme, 5000, equal)
    alg.start()
    println(alg.probability(equal, true))
    alg.kill
  }
}
