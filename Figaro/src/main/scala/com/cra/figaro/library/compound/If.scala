/*
 * If.scala
 * Conditional elements.
 *
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Oct 10, 2010
 *
 * Copyright 2017 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */
///////////////////////////////////////////////////////////////////////////////

package com.cra.figaro.library.compound

import com.cra.figaro.language._

/**
 * Element that conditions on the test and results in either the then clause or else clause.
 * The then and else clauses are constants.
 */
class FastIf[T](name: Name[T], val test: Element[Boolean], val thn: T, val els: T, collection: ElementCollection)
  extends Apply1(name, test, (b: Boolean) => if (b) thn; else els, collection) {
  override def toString = "FastIf(" + test + ", " + thn + ", " + els + ")"
}

/**
 * Element that conditions on the test and results in either the then clause or else clause.
 * The then and else clauses are elements.
 */
class If[T](name: Name[T], val test: Element[Boolean], t: () => Element[T], e: () => Element[T], collection: ElementCollection)
  extends CachingChain(name, test, (b: Boolean) => if (b) t(); else e(), collection) {
  lazy val thn = t()
  lazy val els = e()
  override def toString = "If(" + test + ", " + thn + ", " + els + ")"
}

object If {
  /**
   * Create an element that conditions on the test and results in either the then clause or else clause.
   * The then and else clauses are constants.
   */
  def apply[T](test: Element[Boolean], thn: T, els: T)(implicit name: Name[T], collection: ElementCollection) =
    new FastIf(name, test, thn, els, collection)

  /**
   * Create an element that conditions on the test and results in either the then clause or else clause.
   * The then and else clauses are elements.
   */
  def apply[T](test: Element[Boolean], thn: => Element[T], els: => Element[T])(implicit name: Name[T], collection: ElementCollection) =
    new If(name, test, () => thn, () => els, collection)
}
