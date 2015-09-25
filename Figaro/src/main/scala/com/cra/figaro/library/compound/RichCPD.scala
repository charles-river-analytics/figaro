/*
 * RichCPD.scala
 * Conditional probability distributions with rich cases.
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Apr 25, 2011
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.library.compound

import com.cra.figaro.language._

/**
 * A conditional probability distributions with rich cases with one parent.
 */
class RichCPD1[T1, U](
  name: Name[U],
  arg1: Element[T1],
  clauses: Seq[(CPDCase[T1], Element[U])],
  collection: ElementCollection) extends CachingChain[T1, U](
  name,
  arg1,
  (t1: T1) => RichCPD.getMatch(clauses, t1),
  collection)

/**
 * A conditional probability distributions with rich cases with two parents.
 */
class RichCPD2[T1, T2, U](
  name: Name[U],
  arg1: Element[T1],
  arg2: Element[T2],
  clauses: Seq[((CPDCase[T1], CPDCase[T2]), Element[U])],
  collection: ElementCollection) extends CachingChain[(T1, T2), U](
  name,
  ^^(arg1, arg2)("", collection),
  (p: (T1, T2)) => RichCPD.getMatch(clauses, p._1, p._2),
  collection)

/**
 * A conditional probability distributions with rich cases with three parents.
 */
class RichCPD3[T1, T2, T3, U](
  name: Name[U],
  arg1: Element[T1],
  arg2: Element[T2],
  arg3: Element[T3],
  clauses: Seq[((CPDCase[T1], CPDCase[T2], CPDCase[T3]), Element[U])],
  collection: ElementCollection) extends CachingChain[(T1, T2, T3), U](
  name,
  ^^(arg1, arg2, arg3)("", collection),
  (p: (T1, T2, T3)) => RichCPD.getMatch(clauses, p._1, p._2, p._3),
  collection)

/**
 * A conditional probability distributions with rich cases with four parents.
 */
class RichCPD4[T1, T2, T3, T4, U](
  name: Name[U],
  arg1: Element[T1],
  arg2: Element[T2],
  arg3: Element[T3],
  arg4: Element[T4],
  clauses: Seq[((CPDCase[T1], CPDCase[T2], CPDCase[T3], CPDCase[T4]), Element[U])],
  collection: ElementCollection) extends CachingChain[(T1, T2, T3, T4), U](
  name,
  ^^(arg1, arg2, arg3, arg4)("", collection),
  (p: (T1, T2, T3, T4)) => RichCPD.getMatch(clauses, p._1, p._2, p._3, p._4),
  collection)

/**
 * A conditional probability distributions with rich cases with five parents.
 */
class RichCPD5[T1, T2, T3, T4, T5, U](
  name: Name[U],
  arg1: Element[T1],
  arg2: Element[T2],
  arg3: Element[T3],
  arg4: Element[T4],
  arg5: Element[T5],
  clauses: Seq[((CPDCase[T1], CPDCase[T2], CPDCase[T3], CPDCase[T4], CPDCase[T5]), Element[U])],
  collection: ElementCollection) extends CachingChain[(T1, T2, T3, T4, T5), U](
  name,
  ^^(arg1, arg2, arg3, arg4, arg5)("", collection),
  (p: (T1, T2, T3, T4, T5)) => RichCPD.getMatch(clauses, p._1, p._2, p._3, p._4, p._5),
  collection)

object RichCPD {
  /**
   * Create a conditional probability distributions with rich cases with one parent.
   * @param clauses A sequence of (condition, Element) pairs, where the condition specifies a CPDCase of the parent
   */
  def apply[T1, U](arg1: Element[T1], clauses: (CPDCase[T1], Element[U])*)(implicit name: Name[U], collection: ElementCollection) =
    new RichCPD1(name, arg1, clauses, collection)

  /**
   * Create a conditional probability distributions with rich cases with two parents.
   * @param clauses A sequence of (condition, Element) pairs, where the condition specifies a CPDCase of each of the parents
   */
  def apply[T1, T2, U](arg1: Element[T1], arg2: Element[T2],
    clauses: ((CPDCase[T1], CPDCase[T2]), Element[U])*)(implicit name: Name[U], collection: ElementCollection) =
    new RichCPD2(name, arg1, arg2, clauses, collection)

  /**
   * Create a conditional probability distributions with rich cases with three parents.
   * @param clauses A sequence of (condition, Element) pairs, where the condition specifies a CPDCase of each of the parents
   */
  def apply[T1, T2, T3, U](arg1: Element[T1], arg2: Element[T2], arg3: Element[T3],
    clauses: ((CPDCase[T1], CPDCase[T2], CPDCase[T3]), Element[U])*)(implicit name: Name[U], collection: ElementCollection) =
    new RichCPD3(name, arg1, arg2, arg3, clauses, collection)

  /**
   * Create a conditional probability distributions with rich cases with four parents.
   * @param clauses A sequence of (condition, Element) pairs, where the condition specifies a CPDCase of each of the parents
   */
  def apply[T1, T2, T3, T4, U](arg1: Element[T1], arg2: Element[T2], arg3: Element[T3], arg4: Element[T4],
    clauses: ((CPDCase[T1], CPDCase[T2], CPDCase[T3], CPDCase[T4]), Element[U])*)(implicit name: Name[U], collection: ElementCollection) =
    new RichCPD4(name, arg1, arg2, arg3, arg4, clauses, collection)

  /**
   * Create a conditional probability distributions with rich cases with five parents.
   * @param clauses A sequence of (condition, Element) pairs, where the condition specifies a CPDCase of each of the parents
   */
  def apply[T1, T2, T3, T4, T5, U](arg1: Element[T1], arg2: Element[T2], arg3: Element[T3], arg4: Element[T4],
    arg5: Element[T5],
    clauses: ((CPDCase[T1], CPDCase[T2], CPDCase[T3], CPDCase[T4], CPDCase[T5]), Element[U])*)(implicit name: Name[U], collection: ElementCollection) =
    new RichCPD5(name, arg1, arg2, arg3, arg4, arg5, clauses, collection)

  private[compound] def getMatch[T1, U](clauses: Seq[(CPDCase[T1], Element[U])], t1: T1): Element[U] =
    clauses.find(_._1 contains t1) match {
      case Some(clause) => clause._2
      case None =>
        throw new MatchError(t1)
    }

  private[compound] def getMatch[T1, T2, U](clauses: Seq[((CPDCase[T1], CPDCase[T2]), Element[U])],
    t1: T1, t2: T2): Element[U] =
    clauses.find(clause => (clause._1._1 contains t1) && (clause._1._2 contains t2)) match {
      case Some(clause) => clause._2
      case None => throw new MatchError((t1, t2))
    }

  private[compound] def getMatch[T1, T2, T3, U](clauses: Seq[((CPDCase[T1], CPDCase[T2], CPDCase[T3]), Element[U])],
    t1: T1, t2: T2, t3: T3): Element[U] =
    clauses.find(clause => (clause._1._1 contains t1) &&
      (clause._1._2 contains t2) && (clause._1._3 contains t3)) match {
      case Some(clause) => clause._2
      case None => throw new MatchError((t1, t2, t3))
    }

  private[compound] def getMatch[T1, T2, T3, T4, U](
    clauses: Seq[((CPDCase[T1], CPDCase[T2], CPDCase[T3], CPDCase[T4]), Element[U])],
    t1: T1, t2: T2, t3: T3, t4: T4): Element[U] =
    clauses.find(clause => (clause._1._1 contains t1) &&
      (clause._1._2 contains t2) && (clause._1._3 contains t3) &&
      (clause._1._4 contains t4)) match {
      case Some(clause) => clause._2
      case None => throw new MatchError((t1, t2, t3, t4))
    }

  private[compound] def getMatch[T1, T2, T3, T4, T5, U](
    clauses: Seq[((CPDCase[T1], CPDCase[T2], CPDCase[T3], CPDCase[T4], CPDCase[T5]), Element[U])],
    t1: T1, t2: T2, t3: T3, t4: T4, t5: T5): Element[U] =
    clauses.find(clause => (clause._1._1 contains t1) &&
      (clause._1._2 contains t2) && (clause._1._3 contains t3) &&
      (clause._1._4 contains t4) && (clause._1._5 contains t5)) match {
      case Some(clause) => clause._2
      case None => throw new MatchError((t1, t2, t3, t4, t5))
    }
}
