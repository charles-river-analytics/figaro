/*
 * Values.scala
 * Compute the range of values of elements.
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Jan 1, 2009
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 */

package com.cra.figaro.algorithm

import com.cra.figaro.language._
import com.cra.figaro.library.compound._
import com.cra.figaro.util._
import scala.collection.mutable.Map

/**
 * Objects for computing the range of values of elements in a universe. If an element has an abstraction,
 * it computes the abstract values.
 */
class Values(universe: Universe = Universe.universe) {
  private def values[T](element: Element[T]): Set[T] =
    Abstraction.fromPragmas(element.pragmas) match {
      case None => concreteValues(element)
      case Some(abstraction) => abstractValues(element, abstraction)
    }

  private def concreteValues[T](element: Element[T]): Set[T] =
    element match {
      case c: Constant[_] => Set(c.constant)
      case f: Flip => Set(true, false)
      case d: Select[_, _] => Set(d.outcomes: _*)
      case d: Dist[_, _] => Set(d.outcomes: _*) flatMap (values(_))
      case i: FastIf[_] => Set(i.thn, i.els)
      case i: If[_] => this(i.thn) ++ this(i.els)
      case a: Apply1[_, _] => this(a.arg1) map (a.fn(_))
      case a: Apply2[_, _, _] =>
        for { List(arg1, arg2) <- cartesianProduct(this(a.arg1).toList, this(a.arg2).toList).toSet }
          yield a.fn(arg1.asInstanceOf[a.Arg1Type], arg2.asInstanceOf[a.Arg2Type])
      case a: Apply3[_, _, _, _] =>
        for {
          List(arg1, arg2, arg3) <- cartesianProduct(this(a.arg1).toList, this(a.arg2).toList, this(a.arg3).toList).toSet
        } yield a.fn(arg1.asInstanceOf[a.Arg1Type],
          arg2.asInstanceOf[a.Arg2Type],
          arg3.asInstanceOf[a.Arg3Type])
      case a: Apply4[_, _, _, _, _] =>
        for {
          List(arg1, arg2, arg3, arg4) <- cartesianProduct(this(a.arg1).toList, this(a.arg2).toList,
            this(a.arg3).toList, this(a.arg4).toList).toSet
        } yield a.fn(arg1.asInstanceOf[a.Arg1Type],
          arg2.asInstanceOf[a.Arg2Type],
          arg3.asInstanceOf[a.Arg3Type],
          arg4.asInstanceOf[a.Arg4Type])
      case a: Apply5[_, _, _, _, _, _] =>
        for {
          List(arg1, arg2, arg3, arg4, arg5) <- cartesianProduct(this(a.arg1).toList, this(a.arg2).toList, this(a.arg3).toList,
            this(a.arg4).toList, this(a.arg5).toList).toSet
        } yield a.fn(arg1.asInstanceOf[a.Arg1Type],
          arg2.asInstanceOf[a.Arg2Type],
          arg3.asInstanceOf[a.Arg3Type],
          arg4.asInstanceOf[a.Arg4Type],
          arg5.asInstanceOf[a.Arg5Type])
      case c: Chain[_, _] => this(c.parent) flatMap ((s: c.ParentType) => this(c.get(s)))
      case i: Inject[_] =>
        val inputValues = i.args map (this(_).toList)
        homogeneousCartesianProduct(inputValues: _*).toSet
      case v: ValuesMaker[_] => v.makeValues.toSet
      case _ => throw new UnsupportedAlgorithmException(element)
    }

  private def abstractValues[T](element: Element[T], abstraction: Abstraction[T]): Set[T] = {
    val inputs: Seq[T] = {
      element match {
        case _: Atomic[_] =>
          for { i <- 1 to abstraction.numAbstractPoints * abstraction.numConcretePointsPerAbstractPoint }
            yield element.generateValue(element.generateRandomness)
        case _ => concreteValues(element).toList
      }
    }
    abstraction.scheme.select(inputs, abstraction.numAbstractPoints)
  }

  // We can't use Util.memo because the type of the Map contains existential variables.
  // The problem is that we need to use asInstanceOf to convert the result to a Set of the correct type.
  private var memoValues: Map[Element[_], Set[_]] = Map()

  universe.register(memoValues)

  /**
   * Returns the range of values of an element. This method is memoized.
   */
  def apply[T](element: Element[T]): Set[T] =
    memoValues.get(element) match {
      case Some(set) => set.asInstanceOf[Set[T]]
      case None =>
        val set = values(element)
        memoValues += element -> set
        set
    }
}

object Values {
  private val expansions = scala.collection.mutable.Map[Universe, Values]()

  /**
   * Create an object for computing the range of values of elements in the universe. This object is only
   * created once for a universe.
   */
  def apply(universe: Universe = Universe.universe): Values = {
    expansions.get(universe) match {
      case Some(e) => e
      case None =>
        val expansion = new Values(universe)
        expansions += (universe -> expansion)
        universe.registerUniverse(expansions)
        expansion
    }
  }
}
