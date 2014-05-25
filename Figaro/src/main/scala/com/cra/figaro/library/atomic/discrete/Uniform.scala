/*
 * Uniform.scala
 * Elements representing discrete uniform distributions.
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Feb 25, 2011
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.library.atomic.discrete

import com.cra.figaro.algorithm._
import com.cra.figaro.algorithm.factored._
import com.cra.figaro.language._
import com.cra.figaro.util._

/**
 * A discrete uniform distribution over a sequence of options, each of which is a constant.
 */
class AtomicUniform[T](name: Name[T], options: Seq[T], collection: ElementCollection)
  extends AtomicSelect[T](name, options.toList map (1.0 -> _), collection) with Atomic[T] with Cacheable[T] {
  override def toString = "Uniform(" + options.mkString(", ") + ")"
}

/**
 * A discrete uniform distribution over a sequence of options, each of which
 * is an element over values.
 */
class CompoundUniform[T](name: Name[T], options: Seq[Element[T]], collection: ElementCollection)
  extends CachingChain[List[T], T](
    name,
    new Inject("", options, collection),
    (options: Seq[T]) => new AtomicUniform("", options, collection),
    collection) {
  override def toString = "Uniform(" + options.mkString(", ") + ")"
}

object Uniform {
  /**
   * Create a discrete uniform distribution over a sequence of options, each of
   * which is a constant.
   */
  def apply[T](options: T*)(implicit name: Name[T], collection: ElementCollection) =
    new AtomicUniform(name, options, collection)

  /**
   * Create a discrete uniform distribution over a sequence of options, each of
   * which is an element over values.
   */
  def apply[T](options: Element[T]*)(implicit name: Name[T], collection: ElementCollection) =
    new CompoundUniform(name, options, collection)
}
