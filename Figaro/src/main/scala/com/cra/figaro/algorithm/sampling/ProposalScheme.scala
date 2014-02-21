/*
 * ProposalScheme.scala
 * Proposal schemes.
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Jan 1, 2009
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.algorithm.sampling

import com.cra.figaro.algorithm._
import com.cra.figaro.language._

/**
 * Exception thrown when attempting to create a proposal scheme with no steps.
 */
object EmptyProposalScheme extends RuntimeException

/**
 * Class that describes proposal schemes used in Metropolis-Hastings algorithms.
 */
sealed abstract class ProposalScheme

/**
 * A proposal scheme that proposes a first element and optionally continues with another proposal
 * scheme based on the value of the first element.
 * 
 * @param first The first element to propose.
 * @param rest An optional proposal scheme that is invoked conditioned on the value of the first element.
 */
case class TypedScheme[T](first: () => Element[T], rest: T => Option[ProposalScheme]) extends ProposalScheme

/**
 * A proposal scheme that proposes a first element and optionally continues with another proposal
 * scheme.
 * 
 * @param first The first element to propose.
 * @param rest An optional proposal scheme that is invoked after the first element is proposed.
 */
case class UntypedScheme(first: () => Element[_], rest: Option[ProposalScheme]) extends ProposalScheme

/**
 * A proposal scheme that consists of proposing a single element.
 * 
 * @param element The element to propose
 */
case class FinalScheme(element: () => Element[_]) extends ProposalScheme

/**
 * A proposal scheme that chooses between different proposal schemes, each with a probability.
 * 
 * @param choices A variable list of pairs of probabilities and proposal schemes.
 */
case class DisjointScheme(choices: (Double, () => ProposalScheme)*) extends ProposalScheme

/**
 * Proposes switching the randomness of two elements and optionally continues with another proposal
 * scheme.
 * 
 *  @param firstElems The two elements whose randomness is switched.
 *  @param rest The optional proposal scheme. 
 */
case class SwitchScheme[T, U](firstElems: () => (Element[T], Element[U]), rest: Option[ProposalScheme])
  extends ProposalScheme

object ProposalScheme {
  /**
   * The default proposal scheme that proposes a randomly chosen stochastic element in the universe.
   */
  def default(implicit universe: Universe) = FinalScheme(() => universe.randomStochasticElement())

  /**
   * Create a proposal scheme consisting of proposing each of the elements in sequence.
   */
  def apply(elems: Element[_]*): ProposalScheme = {
    elems.toList match {
      case Nil => throw EmptyProposalScheme
      case List(elem) => FinalScheme(() => elem)
      case elem :: rest => UntypedScheme(() => elem, Some(apply(rest: _*)))
    }
  }
}

