/*
 * CPD.scala
 * Conditional probability distributions.
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Mar 25, 2011
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.library.compound

import com.cra.figaro.language._

/**
 * A conditional probability distribution with one parent.
 */
class CPD1[T1, U](name: Name[U], arg1: Element[T1], clauses: Seq[(T1, Element[U])], collection: ElementCollection)
  extends CachingChain[T1, U](name, arg1, (t1: T1) => CPD.getMatch(clauses, t1), collection)

/**
 * A conditional probability distribution with two parents.
 */
class CPD2[T1, T2, U](name: Name[U], arg1: Element[T1], arg2: Element[T2],
  clauses: Seq[((T1, T2), Element[U])], collection: ElementCollection)
  extends CachingChain[(T1, T2), U](
    name,
    ^^(arg1, arg2)("", collection),
    (p: (T1, T2)) => CPD.getMatch(clauses, p),
    collection)

/**
 * A conditional probability distribution with three parents.
 */
class CPD3[T1, T2, T3, U](name: Name[U], arg1: Element[T1], arg2: Element[T2], arg3: Element[T3],
  clauses: Seq[((T1, T2, T3), Element[U])], collection: ElementCollection)
  extends CachingChain[(T1, T2, T3), U](
    name,
    ^^(arg1, arg2, arg3)("", collection),
    (p: (T1, T2, T3)) => CPD.getMatch(clauses, p),
    collection)

/**
 * A conditional probability distribution with four parents.
 */
class CPD4[T1, T2, T3, T4, U](name: Name[U], arg1: Element[T1], arg2: Element[T2], arg3: Element[T3], arg4: Element[T4],
  clauses: Seq[((T1, T2, T3, T4), Element[U])], collection: ElementCollection)
  extends CachingChain[(T1, T2, T3, T4), U](
    name,
    ^^(arg1, arg2, arg3, arg4)("", collection),
    (p: (T1, T2, T3, T4)) => CPD.getMatch(clauses, p),
    collection)

/**
 * A conditional probability distribution with five parents.
 */
class CPD5[T1, T2, T3, T4, T5, U](name: Name[U], arg1: Element[T1], arg2: Element[T2], arg3: Element[T3], arg4: Element[T4],
  arg5: Element[T5], clauses: Seq[((T1, T2, T3, T4, T5), Element[U])], collection: ElementCollection)
  extends CachingChain[(T1, T2, T3, T4, T5), U](
    name,
    ^^(arg1, arg2, arg3, arg4, arg5)("", collection),
    (p: (T1, T2, T3, T4, T5)) => CPD.getMatch(clauses, p),
    collection)

object CPD {
  /**
   * Create a CPD with one parent.
   * @param clauses A sequence of (condition, Element) pairs, where the condition specifies a value of the parent
   */
  def apply[T1, U](arg1: Element[T1], clauses: (T1, Element[U])*)(implicit name: Name[U], collection: ElementCollection) =
    new CPD1(name, arg1, clauses, collection)

  /**
   * Create a CPD with two parents.
   * @param clauses A sequence of (condition, Element) pairs, where the condition specifies a value of the parents
   */
  def apply[T1, T2, U](arg1: Element[T1], arg2: Element[T2], clauses: ((T1, T2), Element[U])*)(implicit name: Name[U], collection: ElementCollection) =
    new CPD2(name, arg1, arg2, clauses, collection)

  /**
   * Create a CPD with three parents.
   * @param clauses A sequence of (condition, Element) pairs, where the condition specifies a value of the parents
   */
  def apply[T1, T2, T3, U](arg1: Element[T1], arg2: Element[T2], arg3: Element[T3],
    clauses: ((T1, T2, T3), Element[U])*)(implicit name: Name[U], collection: ElementCollection) =
    new CPD3(name, arg1, arg2, arg3, clauses, collection)

  /**
   * Create a CPD with four parents.
   * @param clauses A sequence of (condition, Element) pairs, where the condition specifies a value of the parents
   */
  def apply[T1, T2, T3, T4, U](arg1: Element[T1], arg2: Element[T2], arg3: Element[T3], arg4: Element[T4],
    clauses: ((T1, T2, T3, T4), Element[U])*)(implicit name: Name[U], collection: ElementCollection) =
    new CPD4(name, arg1, arg2, arg3, arg4, clauses, collection)

  /**
   * Create a CPD with five parents.
   * @param clauses A sequence of (condition, Element) pairs, where the condition specifies a value of the parents
   */
  def apply[T1, T2, T3, T4, T5, U](arg1: Element[T1], arg2: Element[T2], arg3: Element[T3], arg4: Element[T4],
    arg5: Element[T5], clauses: ((T1, T2, T3, T4, T5), Element[U])*)(implicit name: Name[U], collection: ElementCollection) =
    new CPD5(name, arg1, arg2, arg3, arg4, arg5, clauses, collection)

  private[compound] def getMatch[T, U](clauses: Seq[(T, Element[U])], t: T): Element[U] =
    clauses.find(_._1 == t) match {
      case Some(clause) => clause._2
      case None => throw new MatchError(t)
    }
}
