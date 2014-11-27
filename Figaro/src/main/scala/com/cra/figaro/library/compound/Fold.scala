package com.cra.figaro.library.compound

import com.cra.figaro.language._

object FoldLeft  {
  /*
   * FoldLeft has two alternative implementations.
   * For factored algorithms, we use a decomposition into a chain series.
   * For other algorithms, we use a simple generateValue.
   */
  def apply[T,U](start: U, function: (U, T) => U)(elements: Element[T]*)(implicit name: Name[U], collection: ElementCollection): Element[U] = {
    if (elements.isEmpty) Constant(start)
    else {
      val elem = elements.head
      if (!elem.active) elem.activate
      new Deterministic[U](name, collection) {
        def args = elements.toList
        override def generateValue = elements.map(_.value).foldLeft(start)(function)
      }
    }
  }
}

object FoldRight  {
  def apply[T,U](start: U, function: (T, U) => U)(elements: Element[T]*)(implicit name: Name[U], collection: ElementCollection): Element[U] = {
    FoldLeft(start, (u: U, t: T) => function(t, u))(elements.reverse:_*)(name, collection)
  }
}

object Reduce {
  def apply[T](function: (T, T) => T)(elements: Element[T]*)(implicit name: Name[T], collection: ElementCollection): Element[T] = {
    val elem = elements.head
    if (!elem.active) elem.activate
    Chain(elem, (t: T) => FoldLeft(t, function)(elements.tail:_*))(name, collection)
  }
}
