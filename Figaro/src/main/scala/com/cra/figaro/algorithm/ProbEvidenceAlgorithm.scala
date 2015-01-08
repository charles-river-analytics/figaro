/*
 * ProbEvidenceAlgorithm.scala
 * Algorithms that compute probability of evidence.
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Jan 1, 2009
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.algorithm

import com.cra.figaro.language._
import scala.language.existentials

/**
 * Algorithms that compute probability of evidence.
 * The evidence is specified as a list of named Evidence items.
 * In addition to the universe, the ProbEvidenceAlgorithm takes two optional arguments. The evidence, which defaults to empty, and the denominator, which defaults to 1.
 * The evidence is a list of specific evidence items associated with references.
 * When started, the algorithm computes the probability of the named evidence, in addition to the conditions and constraints in the model, divided by the denominator (partition function).
 * In a typical use case, one might want to compute the probability of the named evidence, taking the conditions and constraints as a given part of the model.
 * To achieve this, you would create a ProbEvidenceAlgorithm with no evidence to compute the probability of the conditions and constraints. This probability becomes the
 * denominator in a subsequent algorithm that takes the named evidence whose probability you want to compute.
 * Several shortcut ways of achieving this are provided.
 */
trait ProbEvidenceAlgorithm extends Algorithm {
  val universe: Universe
  val evidence: List[NamedEvidence[_]] = List[NamedEvidence[_]]()
  val denominator: Double = 1.0

  /* Particular implementations of probability of evidence algorithms must define the following method. */
  protected def computedResult: Double

  /**
   * The algorithm used to compute the probability of additional evidence, as created by probAdditionalEvidence.
   * This algorithm can be different to the one defined in this class. (For example, a one-time algorithm can use an anytime algorithm for additional evidence.)
   */
  def additionalEvidenceAlgorithm(evidence: List[NamedEvidence[_]]): ProbEvidenceAlgorithm

  /**
   * Returns an algorithm to compute the probability of the additional evidence provided.
   */
  def probAdditionalEvidence(evidence: List[NamedEvidence[_]]): ProbEvidenceAlgorithm = {
    if (!active) throw new AlgorithmInactiveException
    additionalEvidenceAlgorithm(evidence)
  }

  /**
   * The computed probability of evidence.
   */
  def probEvidence: Double = {
    if (!active) throw new AlgorithmInactiveException
    computedResult
  }

   /**
   * The computed log probability of evidence.
   */
  def logProbEvidence: Double = {
    if (!active) throw new AlgorithmInactiveException
    Math.log(probEvidence)
  }
  
  private var savedConditions: List[List[(Element[_]#Condition, Element[_]#Contingency)]] = List()
  private var savedConstraints: List[List[(Element[_]#Constraint, Element[_]#Contingency)]] = List()

  /**
   * Since probability of evidence algorithms introduce additional evidence (namely, their evidence argument), into an existing universe,
   * a mechanism must be provided for introducing the evidence when the algorithm begins and cleaning it up at the end. This is achieved
   * with the initialize method, called when the algorithm starts, and the cleanUp method, called when the algorithm is killed.
   */
  override def initialize(): Unit = {
    super.initialize()
    savedConditions = for { NamedEvidence(ref, _, _) <- evidence } yield universe.get(ref).allConditions
    savedConstraints = for { NamedEvidence(ref, _, _) <- evidence } yield universe.get(ref).allConstraints
    universe.assertEvidence(evidence)
  }

  /**
   * Removes the evidence provided in the constructor from the universe.
   */
  override def cleanUp(): Unit = {
    for { ((NamedEvidence(ref, _, contingency), conditions), constraints) <- evidence.zip(savedConditions).zip(savedConstraints) } {
      universe.removeEvidence(ref, contingency)
      val elem = universe.get(ref)
      // The following should use asInstanceOf[Element.Contingency], which is the same as elem.Contingency. There is no need for the Element class to contain a public type which is the
      // same as its companion object. However, the correct implementation results in the Scala compiler crashing.
      for { (_, contingency) <- conditions } { elem.removeConditions(contingency.asInstanceOf[elem.Contingency]) }
      for { (_, contingency) <- constraints } { elem.removeConstraints(contingency.asInstanceOf[elem.Contingency]) }
      for { (condition, contingency) <- conditions } { elem.addCondition(condition.asInstanceOf[elem.Condition], contingency.asInstanceOf[elem.Contingency]) }
      for { (constraint, contingency) <- constraints } { elem.addConstraint(constraint.asInstanceOf[elem.Constraint], contingency.asInstanceOf[elem.Contingency]) }
    }
    super.cleanUp()
  }

  universe.registerAlgorithm(this)
}
