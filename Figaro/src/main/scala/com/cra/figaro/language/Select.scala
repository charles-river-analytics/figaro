/*
 * Select.scala
 * Distributions with randomly chosen outcomes.
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

/**
 * Distributions with randomly chosen outcomes. The probabilities can
 * either be simple (Doubles) or complex (Elements).
 * 
 * @param clauses The list of pairs of probability specifications and values.
 * @tparam P The type of the probability specification.
 * @tparam T The type of values of this element.
 */
abstract class Select[P, T](name: Name[T], val clauses: List[(P, T)], collection: ElementCollection)
  extends Element[T](name, collection) with Cacheable[T] {
  type Randomness = Double

  def generateRandomness() = random.nextDouble()

  private[figaro] lazy val (probs, outcomes) = clauses.unzip // lazy to avoid uninitialized val bug

  override def toString = {
    val clauseStrings = clauses map (clause => clause._1.toString + " -> " + clause._2)
    "Select(" + clauseStrings.mkString(", ") + ")"
  }
}

/**
 * A distribution in which both the probabilities and the outcomes are values. Each outcome is
 * chosen with the corresponding probability.
 */
class AtomicSelect[T](name: Name[T], clauses: List[(Double, T)], collection: ElementCollection)
  extends Select(name, clauses, collection) with Atomic[T] {
  private lazy val normalizedProbs = normalize(probs)

  private lazy val normalizedClauses = normalizedProbs zip outcomes

  def density(outcome: T) = (0.0 /: (normalizedClauses filter (_._2 == outcome)))(_ + _._1)

  def generateValue(rand: Randomness) = selectMultinomial(rand, normalizedClauses)
}

/**
 * A distribution in which the probabilities are Elements and the outcomes are values.
 */
class CompoundSelect[T](name: Name[T], clauses: List[(Element[Double], T)], collection: ElementCollection)
  extends Select(name, clauses, collection) {
  def args: List[Element[_]] = probs

  def generateValue(rand: Randomness) = {
    probs.foreach(prob => if (prob.value.asInstanceOf[java.lang.Double] == null) prob.generate())
    val unnormalized = probs map (_.value)
    val normalized = normalize(unnormalized)
    selectMultinomial(rand, normalized zip outcomes)
  }
}

/**
 * A distribution in which the probabilities are learnable parameters and the outcomes are values.
 */
class ParameterizedSelect[T](name: Name[T], override val parameter: AtomicDirichlet, outcomes: List[T], collection: ElementCollection)
  extends Select(name, parameter.alphas.toList zip outcomes, collection) with Parameterized[T] {

  private lazy val normalizedProbs = normalize(probs)
  def args: List[Element[_]] = List(parameter)
  private lazy val normalizedClauses = normalizedProbs zip outcomes

  def distributionToStatistics(distribution: Stream[(Double, T)]): Seq[Double] = {
    val distList = distribution.toList
    for { outcome <- outcomes } 
    yield {
      distList.find(_._2 == outcome) match {
        case Some((prob, _)) => prob
        case None => 0.0
      }
    }
  }
  
  def density(value: T): Double = {
    outcomes.indexOf(value) match {
      case -1 => 0.0
      case i => parameter.value(i)
    }
  }
  
  def generateValue(rand: Randomness) = selectMultinomial(rand, normalizedClauses)

}

object Select {

  private def makeParameterizedSelect[T](name: Name[T], parameter: AtomicDirichlet, outcomes: List[T], collection: ElementCollection): ParameterizedSelect[T] = {
    new ParameterizedSelect(name, parameter, outcomes, collection)
  }
  /**
   * A distribution in which both the probabilities and the outcomes are values. Each outcome is
   * chosen with the corresponding probability.
   */
  def apply[T](clauses: (Double, T)*)(implicit name: Name[T], collection: ElementCollection) =
    new AtomicSelect(name, clauses.toList, collection)
  /**
   * A distribution in which both the probabilities and the outcomes are values. Each outcome is
   * chosen with the corresponding probability.
   */
  def apply[T](probabilities: List[Double], outcomes: List[T])(implicit name: Name[T], collection: ElementCollection) =
    new AtomicSelect(name, probabilities zip outcomes, collection)
  /**
   * A distribution in which the probabilities are Elements and the outcomes are values.
   */
  def apply[T](clauses: (Element[Double], T)*)(implicit name: Name[T], collection: ElementCollection) =
    new CompoundSelect(name, clauses.toList, collection)

    /**
   * A distribution in which the probabilities are Elements and the outcomes are values.
   */
  def apply[T](probabilities: List[Element[Double]], outcomes: List[T])(implicit name: Name[T], collection: ElementCollection) =
    new CompoundSelect(name, probabilities zip outcomes, collection)

  /**
  * A distribution in which the probabilities are specified by a learnable parameter and the outcomes are values.
  */
  def apply[T](parameter: AtomicDirichlet, outcomes: T*)(implicit name: Name[T], collection: ElementCollection) =
    makeParameterizedSelect(name, parameter, outcomes.toList, collection)
        
}

