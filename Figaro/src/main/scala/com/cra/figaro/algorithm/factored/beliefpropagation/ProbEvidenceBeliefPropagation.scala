/*
 * ProbEvidenceBeliefPropagation.scala  
 * A belief propagation algorithm.
 * 
 * Created By:      Brian Ruttenberg (bruttenberg@cra.com)
 * Creation Date:   Jan 15, 2015
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.algorithm.factored.beliefpropagation

import scala.Option.option2Iterable
import com.cra.figaro.algorithm._
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.language._
import com.cra.figaro.util._
import annotation.tailrec
import com.cra.figaro.algorithm.OneTimeProbQuery
import com.cra.figaro.algorithm.ProbQueryAlgorithm
import com.cra.figaro.algorithm.factored.factors._
import com.cra.figaro.algorithm.factored.factors.factory._
import com.cra.figaro.algorithm.factored._
import com.cra.figaro.algorithm.sampling.ProbEvidenceSampler
import com.cra.figaro.language.Element
import com.cra.figaro.language.Universe
import com.cra.figaro.algorithm.lazyfactored.LazyValues
import com.cra.figaro.algorithm.lazyfactored.BoundedProbFactor
import scala.collection.mutable.Map

trait ProbEvidenceBeliefPropagation extends ProbabilisticBeliefPropagation with ProbEvidenceAlgorithm  {
    
  private def logFcn: (Double => Double) = (logSpaceSemiring(): DivideableSemiRing[Double]) match {
    case LogSumProductSemiring() => (d: Double) => d
    case SumProductSemiring() => (d: Double) => if (d == semiring.zero) Double.NegativeInfinity else math.log(d)
  }
  
  private def probFcn: (Double => Double) = (logSpaceSemiring(): DivideableSemiRing[Double]) match {
    case LogSumProductSemiring() => (d: Double) => if (d == logSpaceSemiring().zero) 0 else math.exp(d)
    case SumProductSemiring() => (d: Double) => d
  }

  private def entropy(probFactor: Factor[Double], logFactor: Factor[Double]): Double = {
    // Even though the variables in each factor are the same, the order of the vars might be different
    val logFactorMapping = probFactor.variables.map(v => logFactor.variables.indexOf(v))
    def remap(l: List[Int]) = l.zipWithIndex.map(s => (s._1, logFactorMapping(s._2))).sortBy(_._2).unzip._1

    val e = (0.0 /: probFactor.getIndices)((c: Double, i: List[Int]) => {
      val p = probFcn(probFactor.get(i))
      if (p == 0) c else c + p * logFcn(logFactor.get(remap(i)))
    })
    e
  }

  /**
   * Compute the evidence of the model. Returns the probability of evidence on the model. This assumes that BP
   * has already been run on this algorithm instance.
   */
  def computedResult(): Double = {

    val factorNodes = factorGraph.getNodes.filter(_.isInstanceOf[FactorNode]).toList
    val varNodes = factorGraph.getNodes.filter(_.isInstanceOf[VariableNode]).toList

    val nonZeroEvidence = factorNodes.exists(p => beliefMap(p).contents.exists(_._2 != Double.NegativeInfinity))

    if (nonZeroEvidence) {
      val betheEnergy = -1 * factorNodes.map(f => {
        entropy(normalize(beliefMap(f)), factorGraph.getFactorForNode(f.asInstanceOf[FactorNode]))
      }).sum
      val betheEntropy = {
        val factorEntropy = -1 * factorNodes.map(f => {
          entropy(normalize(beliefMap(f)), normalize(beliefMap(f)))
        }).sum
        val varEntropy = varNodes.map(v => {
          (factorGraph.getNeighbors(v).size - 1) * entropy(normalize(beliefMap(v)), normalize(beliefMap(v)))
        }).sum
        factorEntropy + varEntropy
      }
      math.exp(-1 * (betheEnergy - betheEntropy)) / denominator
    } else {
      0.0
    }
  }
}

/**
 * Trait for One Time BP evidence algorithms.
 */
trait OneTimeProbEvidenceBeliefPropagation extends OneTimeProbabilisticBeliefPropagation with OneTimeProbEvidence with ProbEvidenceBeliefPropagation {
  def additionalEvidenceAlgorithm(evidence: List[NamedEvidence[_]]): ProbEvidenceAlgorithm = {
    val myIterations = this.iterations
    val myResult = computedResult
    val myEvidence = evidence
    new ProbQueryBeliefPropagation(universe, universe.activeElements: _*)(
      List(),
      (u: Universe, e: List[NamedEvidence[_]]) => () => ProbEvidenceSampler.computeProbEvidence(10000, e)(u)) with OneTimeProbEvidenceBeliefPropagation with OneTimeProbQuery {
      val iterations = myIterations
      override val denominator = myResult
      override val evidence = myEvidence
    }
  }
}


object ProbEvidenceBeliefPropagation {
  /**
   * Creates a One Time belief propagation computer in the current default universe.
   */
def apply(myIterations: Int, evidence: List[NamedEvidence[_]])(implicit universe: Universe) = {
    val baseline = new ProbQueryBeliefPropagation(universe, universe.activeElements:_*)(
      List(),
      (u: Universe, e: List[NamedEvidence[_]]) => () => ProbEvidenceSampler.computeProbEvidence(10000, e)(u)) 
      with OneTimeProbabilisticBeliefPropagation with OneTimeProbQuery with OneTimeProbEvidenceBeliefPropagation { val iterations = myIterations }
    baseline.start
    baseline.probAdditionalEvidence(evidence)
  }

  /**
   * Use one-time sampling to compute the probability of the given named evidence.
   * Takes the conditions and constraints in the model as part of the model definition.
   * This method takes care of creating and running the necessary algorithms.
   */
  def computeProbEvidence(myIterations: Int, evidence: List[NamedEvidence[_]])(implicit universe: Universe): Double = {
    val alg1 = apply(myIterations, List())
    alg1.start()
    val alg2 = alg1.probAdditionalEvidence(evidence)
    alg1.kill()
    alg2.start()
    val result = alg2.probEvidence
    alg2.kill()
    result
  }
 
}
