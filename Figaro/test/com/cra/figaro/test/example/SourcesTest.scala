/*
 * SourcesTest.scala  
 * Sources example tests.
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Jan 1, 2009
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.test.example

import org.scalatest.Matchers
import org.scalatest.WordSpec
import com.cra.figaro.algorithm._
import com.cra.figaro.algorithm.factored._
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.language._
import com.cra.figaro.library.compound._
import com.cra.figaro.library.atomic.continuous._
import com.cra.figaro.util._
import com.cra.figaro.test._

class SourcesTest extends WordSpec with Matchers {
  "The sources example" should {
    "produce the correct answer under variable elimination with dependent universe reasoning" taggedAs (ExampleTest) in {
      def peAlg(universe: Universe, evidence: List[NamedEvidence[_]]) = () => ProbEvidenceSampler.computeProbEvidence(1000000, evidence)(universe)
      test((dependentUniverses: List[(Universe, List[NamedEvidence[_]])], element: Element[Source]) =>
        VariableElimination(dependentUniverses, peAlg _, element))

    }
  }

  class Source(val name: String) {
    override val toString = name
  }

  abstract class Sample(val name: String) {
    val fromSource: Element[Source]

    override val toString = name
  }

  class Pair(val source: Source, val sample: Sample) {
    val universe = new Universe(List(sample.fromSource))
    val isTheRightSource = Apply(sample.fromSource, (s: Source) => s == source)("", universe)
    val rightSourceDistance = Normal(0.0, 1.0)("", universe)
    val wrongSourceDistance = Uniform(0.0, 10.0)("", universe)
    val distance = If(isTheRightSource, rightSourceDistance, wrongSourceDistance)("distance", universe)
  }

  def test(algorithmCreator: (List[(Universe, List[NamedEvidence[_]])], Element[Source]) => ProbQueryAlgorithm) {
    Universe.createNew()
    val source1 = new Source("Source 1")
    val source2 = new Source("Source 2")
    val source3 = new Source("Source 3")
    val sample1 = new Sample("Sample 1") { val fromSource = Select(0.5 -> source1, 0.5 -> source2) }
    val sample2 = new Sample("Sample 2") { val fromSource = Select(0.9 -> source1, 0.1 -> source3) }
    val pair1 = new Pair(source1, sample1)
    val pair2 = new Pair(source2, sample1)
    val pair3 = new Pair(source1, sample2)
    val pair4 = new Pair(source3, sample2)

    val values = Values()
    val samples = List(sample1, sample2)
    for {
      (firstSample, secondSample) <- upperTriangle(samples)
      sources1 = values(firstSample.fromSource)
      sources2 = values(secondSample.fromSource)
      if sources1.intersect(sources2).nonEmpty
    } {
      ^^(firstSample.fromSource, secondSample.fromSource).addCondition((p: (Source, Source)) => p._1 != p._2)
    }

    val condition1 = (d: Double) => d > 0.5 && d < 0.6
    val condition2 = (d: Double) => d > 1.5 && d < 1.6
    val condition3 = (d: Double) => d > 2.5 && d < 2.6
    val condition4 = (d: Double) => d > 0.5 && d < 0.6
    val evidence1 = List(NamedEvidence("distance", Condition(condition1)))
    val evidence2 = List(NamedEvidence("distance", Condition(condition2)))
    val evidence3 = List(NamedEvidence("distance", Condition(condition3)))
    val evidence4 = List(NamedEvidence("distance", Condition(condition4)))
    val dependent1 = (pair1.universe, evidence1)
    val dependent2 = (pair2.universe, evidence2)
    val dependent3 = (pair3.universe, evidence3)
    val dependent4 = (pair4.universe, evidence4)

    // Uniform probability of each range = 0.1 / 10.0 = 0.01
    // Normal cdf:
    // 0.5: 0.6915
    // 0.6: 0.7257
    // 1.5: 0.9332
    // 1.6: 0.9452
    // 2.5: 0.9938
    // 2.6: 0.9953
    // Normal probability of (0.5, 0.6) = 0.7257 - 0.6915 = 0.0342 
    // Normal probability of (1.5, 1.6) = 0.9452 - 0.9332 = 0.012 
    // Normal probability of (2.5, 2.6) = 0.9953 - 0.9938 = 0.0015 
    // Code: pxy = Probability sample 1 is from source x and sample 2 is from source y
    val p13 = 0.5 * 0.1 * 0.0342 * 0.0342
    val p21 = 0.5 * 0.9 * 0.012 * 0.0015
    val p23 = 0.5 * 0.1 * 0.012 * 0.0342
    val answer = p13 / (p13 + p21 + p23)
    val alg = algorithmCreator(List(dependent1, dependent2, dependent3, dependent4), sample1.fromSource)
    alg.start()
    alg.probability(sample1.fromSource, source1) should be(answer +- 0.01)
    alg.kill
  }

}
