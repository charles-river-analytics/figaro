/*
 * TupleElement.scala
 * Elements over tuples.
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Jan 1, 2009
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.library.compound

import com.cra.figaro.language._

/**
 * Elements over pairs on which element-level component extraction is defined.
 */
class DuoElement[T1, T2](elem: Element[(T1, T2)]) {
  /**
   * Element that extracts the first component.
   */
  def _1 = Apply(elem, (t: (T1, T2)) => t._1)

  /**
   * Element that extracts the second component.
   */
  def _2 = Apply(elem, (t: (T1, T2)) => t._2)
}

/**
 * Elements over triples on which element-level component extraction is defined.
 */
class TrioElement[T1, T2, T3](elem: Element[(T1, T2, T3)]) {
  /**
   * Element that extracts the first component.
   */
  def _1 = Apply(elem, (t: (T1, T2, T3)) => t._1)

  /**
   * Element that extracts the second component.
   */
  def _2 = Apply(elem, (t: (T1, T2, T3)) => t._2)

  /**
   * Element that extracts the third component.
   */
  def _3 = Apply(elem, (t: (T1, T2, T3)) => t._3)
}

/**
 * Elements over quadruples on which element-level component extraction is defined.
 */
class QuartetElement[T1, T2, T3, T4](elem: Element[(T1, T2, T3, T4)]) {
  /**
   * Element that extracts the first component.
   */
  def _1 = Apply(elem, (t: (T1, T2, T3, T4)) => t._1)

  /**
   * Element that extracts the second component.
   */
  def _2 = Apply(elem, (t: (T1, T2, T3, T4)) => t._2)

  /**
   * Element that extracts the third component.
   */
  def _3 = Apply(elem, (t: (T1, T2, T3, T4)) => t._3)

  /**
   * Element that extracts the fourth component.
   */
  def _4 = Apply(elem, (t: (T1, T2, T3, T4)) => t._4)
}

/**
 * Elements over quintuples on which element-level component extraction is defined.
 */
class QuintetElement[T1, T2, T3, T4, T5](elem: Element[(T1, T2, T3, T4, T5)]) {
  /**
   * Element that extracts the first component.
   */
  def _1 = Apply(elem, (t: (T1, T2, T3, T4, T5)) => t._1)

  /**
   * Element that extracts the second component.
   */
  def _2 = Apply(elem, (t: (T1, T2, T3, T4, T5)) => t._2)

  /**
   * Element that extracts the third component.
   */
  def _3 = Apply(elem, (t: (T1, T2, T3, T4, T5)) => t._3)

  /**
   * Element that extracts the fourth component.
   */
  def _4 = Apply(elem, (t: (T1, T2, T3, T4, T5)) => t._4)

  /**
   * Element that extracts the fifth component.
   */
  def _5 = Apply(elem, (t: (T1, T2, T3, T4, T5)) => t._5)
}
