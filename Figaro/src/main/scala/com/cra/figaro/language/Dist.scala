/*
 * Dist.scala
 * Distributions in which outcomes are specified by elements
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Jan 1, 2009
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.language

import com.cra.figaro.library.atomic.continuous._
import com.cra.figaro.util._
import com.sun.org.apache.bcel.internal.classfile.Unknown

/**
 * Distributions with randomly chosen outcomes that are themselves specified by Elements. The probabilities can
 * either be simple (Doubles) or complex (Elements).
 * 
 * @param clauses The list of pairs of probability specifications and elements to generate the value.
 * @tparam P The type of the probability specification.
 * @tparam T The type of values of this element.
 */
abstract class Dist[P, T](name: Name[T], val clauses: List[(P, Element[T])], collection: ElementCollection)
  extends Element[T](name, collection) with IfArgsCacheable[T] {
  type Randomness = Double

  def generateRandomness() = random.nextDouble()

  private[figaro] lazy val (probs, outcomes) = clauses.unzip // lazy to avoid uninitialized val bug

  private[figaro] lazy val outcomeArray = outcomes.toArray

  /**
   * Select which outcome clause will be used, based on the randomness.
   */
  def selectIndex(rand: Randomness): Int

  /**
   * Finish the generation of the value of this element, given the index of the outcome clause used.
   */
  def finishGeneration(index: Int): T = {
    val outcome = outcomeArray(index)
    if (outcome.value == null) outcome.generate()
    outcome.value
  }

  def generateValue(rand: Randomness) = finishGeneration(selectIndex(rand))
}

/**
 * A distribution in which the probabilities are constants and the outcomes are Elements.
 */
class AtomicDist[T](name: Name[T], clauses: List[(Double, Element[T])], collection: ElementCollection)
  extends Dist(name, clauses, collection) {
  def args: List[Element[_]] = outcomes

  private lazy val indexedProbs = normalize(probs).zipWithIndex

  /**
   * Select which outcome clause will be used, based on the randomness.
   */
  def selectIndex(rand: Randomness) = selectMultinomial(rand, indexedProbs)

  override def toString = {
    val clauseStrings = clauses map (clause => clause._1.toString + " -> " + clause._2)
    "Dist(" + clauseStrings.mkString(", ") + ")"
  }
}

/**
 * A distribution in which both the probabilities and outcomes are Elements.
 */
class CompoundDist[T](name: Name[T], clauses: List[(Element[Double], Element[T])], collection: ElementCollection)
  extends Dist(name, clauses, collection) {
  // args is a val rather than a def because some work needs to be done to produce it;
  // it is lazy to avoid uninitialized val bug
  lazy val args: List[Element[_]] = probs ::: outcomes

  /**
   * Select which outcome clause will be used, based on the randomness.
   */
  def selectIndex(rand: Randomness) = {
    probs.foreach(prob => if (prob.value.asInstanceOf[java.lang.Double] == null) prob.generate())
    val unnormalized = probs map (_.value)
    val normalized = normalize(unnormalized)
    selectMultinomial(rand, normalized.zipWithIndex)
  }

  override def toString = {
    val clauseStrings = clauses map (clause => clause._1.toString + " -> " + clause._2)
    "Dist(" + clauseStrings.mkString(", ") + ")"
  }
}

object Dist {
  /**
   * A distribution in which the probabilities are constants and the outcomes are Elements.
   */
  def apply[T](clauses: (Double, Element[T])*)(implicit name: Name[T], collection: ElementCollection) =
    new AtomicDist(name, clauses.toList, collection)

  /**
   * A distribution in which both the probabilities and outcomes are Elements.
   */
  def apply[T](clauses: (Element[Double], Element[T])*)(implicit name: Name[T], collection: ElementCollection) =
    new CompoundDist(name, clauses.toList, collection)

  /**
   * Constructor for distributions in which the probabilities and outcomes are provided as two lists of equal length.
   */
  def apply[T](probabilities: List[Double], outcomes: List[Element[T]])(implicit name: Name[T], collection: ElementCollection) =
    new AtomicDist(name, probabilities zip outcomes, collection)

}
