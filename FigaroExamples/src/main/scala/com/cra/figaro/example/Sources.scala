/*
 * Sources.scala
 * An example of dependent universe reasoning.
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

import com.cra.figaro.algorithm._
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.algorithm.factored._
import com.cra.figaro.library.compound.{ If, ^^ }
import com.cra.figaro.library.atomic.continuous._
import com.cra.figaro.language._
import com.cra.figaro.util._

/**
 * An example of dependent universe reasoning.
 */
object Sources {
  private class Source(val name: String) {
    override val toString = name
  }

  private abstract class Sample(val name: String) {
    val fromSource: Element[Source]

    override val toString = name
  }

  private class Pair(val source: Source, val sample: Sample) {
    val universe = new Universe(List(sample.fromSource))
    val isTheRightSource = Apply(sample.fromSource, (s: Source) => s == source)("isTheRightSource", universe)
    val rightSourceDistance = Normal(0.0, 1.0)("rightSourceDistance", universe)
    val wrongSourceDistance = Uniform(0.0, 10.0)("wrongSourceDistance", universe)
    val distance = If(isTheRightSource, rightSourceDistance, wrongSourceDistance)("distance", universe)
  }

  private val source1 = new Source("Source 1")
  private val source2 = new Source("Source 2")
  private val source3 = new Source("Source 3")
  private val sample1 = new Sample("Sample 1") { val fromSource = Select(0.5 -> source1, 0.5 -> source2) }
  private val sample2 = new Sample("Sample 2") { val fromSource = Select(0.9 -> source1, 0.1 -> source3) }
  private val pair1 = new Pair(source1, sample1)
  private val pair2 = new Pair(source2, sample1)
  private val pair3 = new Pair(source1, sample2)
  private val pair4 = new Pair(source3, sample2)

  private val values = Values()
  private val samples = List(sample1, sample2)
  for {
    (firstSample, secondSample) <- upperTriangle(samples)
    sources1 = values(firstSample.fromSource)
    sources2 = values(secondSample.fromSource)
    if sources1.intersect(sources2).nonEmpty
  } {
    println("First sample: " + firstSample + ", Second sample: " + secondSample)
    ^^(firstSample.fromSource, secondSample.fromSource).addCondition((p: (Source, Source)) => p._1 != p._2)
  }

  def main(args: Array[String]) {
    val evidence1 = NamedEvidence("distance", Condition((d: Double) => d > 0.5 && d < 0.6))
    val evidence2 = NamedEvidence("distance", Condition((d: Double) => d > 1.5 && d < 1.6))
    val evidence3 = NamedEvidence("distance", Condition((d: Double) => d > 2.5 && d < 2.6))
    val evidence4 = NamedEvidence("distance", Condition((d: Double) => d > 0.5 && d < 0.6))
    val ue1 = (pair1.universe, List(evidence1))
    val ue2 = (pair2.universe, List(evidence2))
    val ue3 = (pair3.universe, List(evidence3))
    val ue4 = (pair4.universe, List(evidence4))
    def peAlg(universe: Universe, evidence: List[NamedEvidence[_]]) = () => ProbEvidenceSampler.computeProbEvidence(100000, evidence)(universe)
    val alg = VariableElimination(List(ue1, ue2, ue3, ue4), peAlg _, sample1.fromSource)
    alg.start()
    val result = alg.probability(sample1.fromSource)(_ == source1)
    println("Probability of Source 1: " + result)
    alg.kill()

  }
}
