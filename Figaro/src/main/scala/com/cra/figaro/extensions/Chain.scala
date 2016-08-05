package com.cra.figaro.extensions

import com.cra.figaro.language._

object Chain {
  /**
    * Create a chain of 1 argument.
    * The Chain factory constructor chooses either a CachingChain or NonCachingChain depending on the cacheability of the parent.
    * The parent's cacheability is determined by the parent.isCachable field. This is normally determined by the type of the element.
    */
  def apply[T, U](parent: Element[T])(cpd: T => Element[U])(implicit name: Name[U], collection: ElementCollection): Chain[T, U] = {
    if (parent.isCachable) new CachingChain(name, parent, cpd, collection)
    else new NonCachingChain(name, parent, cpd, collection)
  }

  /**
    * Create a chain of 2 arguments. This is implemented as a chain on the single argument consisting of the pair of the two arguments.
    * If both arguments are cachable, a CachingChain will be produced, otherwise a NonCaching chain.
    */
  def apply[T1, T2, U](parent1: Element[T1], parent2: Element[T2])(cpd: (T1, T2) => Element[U])(implicit name: Name[U], collection: ElementCollection): Chain[(T1, T2), U] = {
    val parentTuple = new Apply2("", parent1, parent2, (t1: T1, t2: T2) => (t1, t2), collection)
    if (parentTuple.isCachable) new CachingChain(name, parentTuple, (p: (T1, T2)) => cpd(p._1, p._2), collection)
    else new NonCachingChain(name, parentTuple, (p: (T1, T2)) => cpd(p._1, p._2), collection)
  }

}

object CachingChain {

  /** Create a CachingChain of 1 argument. */
  def apply[T, U](parent: Element[T])(cpd: T => Element[U])(implicit name: Name[U], collection: ElementCollection): CachingChain[T, U] =
    new CachingChain(name, parent, cpd, collection)

  /**
    * Create a CachingChain of 2 arguments. This is implemented as a chain on the single argument consisting of the pair of the two arguments.
    */
  def apply[T1, T2, U](parent1: Element[T1], parent2: Element[T2])(cpd: (T1, T2) => Element[U])(implicit name: Name[U], collection: ElementCollection): CachingChain[(T1, T2), U] = {
    new CachingChain(
      name,
      new Apply2("", parent1, parent2, (t1: T1, t2: T2) => (t1, t2), collection),
      (pair: (T1, T2)) => cpd(pair._1, pair._2),
      collection)
  }
}

object NonCachingChain {
  /** Create a NonCaching chain of 1 argument. */
  def apply[T, U](parent: Element[T])(cpd: T => Element[U])(implicit name: Name[U], collection: ElementCollection): NonCachingChain[T, U] =
    new NonCachingChain(name, parent, cpd, collection)

  /** Create a NonCaching chain of 2 arguments. This is implemented as a chain on the single argument consisting of the pair of the two arguments. */
  def apply[T1, T2, U](parent1: Element[T1], parent2: Element[T2])(cpd: (T1, T2) => Element[U])(implicit name: Name[U], collection: ElementCollection): NonCachingChain[(T1, T2), U] =
    new NonCachingChain(
      name,
      new Apply2("", parent1, parent2, (t1: T1, t2: T2) => (t1, t2), collection),
      (pair: (T1, T2)) => cpd(pair._1, pair._2),
      collection)
}
