/*
 * Values.scala
 * Lazily compute the range of values of elements.
 *
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Dec 27, 2013
 *
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.algorithm.lazyfactored

import com.cra.figaro.language._
import com.cra.figaro.algorithm._
import com.cra.figaro.util._
import com.cra.figaro.library.compound._
import scala.collection.mutable.Map
import scala.collection.SortedSet
import ValueSet._
import com.cra.figaro.algorithm.factored.ParticleGenerator

/*
 * The Values class takes a universe and a depth and provides a
 * Computing values in a lazy way requires a depth to be specified. Every time a Chain is expanded, the depth is decreased by 1. If the depth is 0, the chain is
 * not expanded, and its value set contains only Star, which means unspecified values.
 *
 */

/**
 * Object for lazily computing the range of values of elements in a universe. Given a universe, you can compute the values
 * of elements in the universe to any desired depth.
 */
class LazyValues(universe: Universe) {
  private def values[T](element: Element[T], depth: Int, numArgSamples: Int, numTotalSamples: Int): ValueSet[T] = {
    // In some cases (e.g. CompoundFlip), we might know the value of an element without getting the values of its arguments.
    // However, future algorithms rely on the values of the arguments having been gotten already.
    // Therefore, we compute the values of the arguments (to a lesser depth) first,
    // and use the stored values of the arguments when we actually compute the values of the element.

    // Override the argument list for chains since the resultElement of the chain will be processed as the chain is processed
    val elementArgs = element match {
      case c: Chain[_,_] => List(c.parent)
      case _ => element.args
    }

    for { arg <- elementArgs } {
      LazyValues(arg.universe)(arg, depth - 1, numArgSamples, numTotalSamples)
      usedBy(arg) = usedBy.getOrElse(arg, Set()) + element
    }

    Abstraction.fromPragmas(element.pragmas) match {
      case None => concreteValues(element, depth, numArgSamples, numTotalSamples)
      case Some(abstraction) => abstractValues(element, abstraction, depth, numArgSamples, numTotalSamples)
    }

  }

  private def concreteValues[T](element: Element[T], depth: Int, numArgSamples: Int, numTotalSamples: Int): ValueSet[T] =
    element match {
      case c: Constant[_] => withoutStar(Set(c.constant))
      case f: Flip => withoutStar(Set(true, false))
      case d: Select[_, _] => withoutStar(Set(d.outcomes: _*))
      case d: Dist[_, _] =>
        val componentSets = d.outcomes.map(storedValues(_))
        componentSets.reduce(_ ++ _)
      //case i: FastIf[_] => withoutStar(Set(i.thn, i.els))
      case a: Apply1[_, _] =>
        val applyMap = getMap(a)
        val vs1 = LazyValues(a.arg1.universe).storedValues(a.arg1)
        val resultsSet =
          for {
            arg1Val <- vs1.regularValues
          } yield {
            getOrElseInsert(applyMap, arg1Val, a.fn(arg1Val))
          }
        if (vs1.hasStar) withStar(resultsSet); else withoutStar(resultsSet)
      case a: Apply2[_, _, _] =>
        val applyMap = getMap(a)
        val vs1 = LazyValues(a.arg1.universe).storedValues(a.arg1)
        val vs2 = LazyValues(a.arg2.universe).storedValues(a.arg2)
        val choices = cartesianProduct(vs1.xvalues.toList, vs2.xvalues.toList).asInstanceOf[List[List[Extended[_]]]]
        val resultsList =
          for {
            List(arg1, arg2) <- choices
            if (arg1.isRegular && arg2.isRegular)
          } yield {
            val arg1Val = arg1.value.asInstanceOf[a.Arg1Type]
            val arg2Val = arg2.value.asInstanceOf[a.Arg2Type]
            getOrElseInsert(applyMap, (arg1Val, arg2Val), a.fn(arg1Val, arg2Val))
          }
        if (vs1.hasStar || vs2.hasStar) withStar(resultsList.toSet); else withoutStar(resultsList.toSet)
      case a: Apply3[_, _, _, _] =>
        val applyMap = getMap(a)
        val vs1 = LazyValues(a.arg1.universe).storedValues(a.arg1)
        val vs2 = LazyValues(a.arg2.universe).storedValues(a.arg2)
        val vs3 = LazyValues(a.arg3.universe).storedValues(a.arg3)
        val choices = cartesianProduct(vs1.xvalues.toList, vs2.xvalues.toList, vs3.xvalues.toList).asInstanceOf[List[List[Extended[_]]]]
        val resultsList =
          for {
            List(arg1, arg2, arg3) <- choices
            if (arg1.isRegular && arg2.isRegular && arg3.isRegular)
          } yield {
            val arg1Val = arg1.value.asInstanceOf[a.Arg1Type]
            val arg2Val = arg2.value.asInstanceOf[a.Arg2Type]
            val arg3Val = arg3.value.asInstanceOf[a.Arg3Type]
            getOrElseInsert(applyMap, (arg1Val, arg2Val, arg3Val), a.fn(arg1Val, arg2Val, arg3Val))
          }
        if (vs1.hasStar || vs2.hasStar || vs3.hasStar) withStar(resultsList.toSet); else withoutStar(resultsList.toSet)
      case a: Apply4[_, _, _, _, _] =>
        val applyMap = getMap(a)
        val vs1 = LazyValues(a.arg1.universe).storedValues(a.arg1)
        val vs2 = LazyValues(a.arg2.universe).storedValues(a.arg2)
        val vs3 = LazyValues(a.arg3.universe).storedValues(a.arg3)
        val vs4 = LazyValues(a.arg4.universe).storedValues(a.arg4)
        val choices = cartesianProduct(vs1.xvalues.toList, vs2.xvalues.toList, vs3.xvalues.toList, vs4.xvalues.toList).asInstanceOf[List[List[Extended[_]]]]
        val resultsList =
          for {
            List(arg1, arg2, arg3, arg4) <- choices
            if (arg1.isRegular && arg2.isRegular && arg3.isRegular && arg4.isRegular)
          } yield {
            val arg1Val = arg1.value.asInstanceOf[a.Arg1Type]
            val arg2Val = arg2.value.asInstanceOf[a.Arg2Type]
            val arg3Val = arg3.value.asInstanceOf[a.Arg3Type]
            val arg4Val = arg4.value.asInstanceOf[a.Arg4Type]
            getOrElseInsert(applyMap, (arg1Val, arg2Val, arg3Val, arg4Val), a.fn(arg1Val, arg2Val, arg3Val, arg4Val))
          }
        if (vs1.hasStar || vs2.hasStar || vs3.hasStar || vs4.hasStar) withStar(resultsList.toSet); else withoutStar(resultsList.toSet)
      case a: Apply5[_, _, _, _, _, _] =>
        val applyMap = getMap(a)
        val vs1 = LazyValues(a.arg1.universe).storedValues(a.arg1)
        val vs2 = LazyValues(a.arg2.universe).storedValues(a.arg2)
        val vs3 = LazyValues(a.arg3.universe).storedValues(a.arg3)
        val vs4 = LazyValues(a.arg4.universe).storedValues(a.arg4)
        val vs5 = LazyValues(a.arg5.universe).storedValues(a.arg5)
        val choices = cartesianProduct(vs1.xvalues.toList, vs2.xvalues.toList, vs3.xvalues.toList, vs4.xvalues.toList, vs5.xvalues.toList).asInstanceOf[List[List[Extended[_]]]]
        val resultsList =
          for {
            List(arg1, arg2, arg3, arg4, arg5) <- choices
            if (arg1.isRegular && arg2.isRegular && arg3.isRegular && arg4.isRegular && arg5.isRegular)
          } yield {
            val arg1Val = arg1.value.asInstanceOf[a.Arg1Type]
            val arg2Val = arg2.value.asInstanceOf[a.Arg2Type]
            val arg3Val = arg3.value.asInstanceOf[a.Arg3Type]
            val arg4Val = arg4.value.asInstanceOf[a.Arg4Type]
            val arg5Val = arg5.value.asInstanceOf[a.Arg5Type]
            getOrElseInsert(applyMap, (arg1Val, arg2Val, arg3Val, arg4Val, arg5Val), a.fn(arg1Val, arg2Val, arg3Val, arg4Val, arg5Val))
          }
        if (vs1.hasStar || vs2.hasStar || vs3.hasStar || vs4.hasStar || vs5.hasStar) withStar(resultsList.toSet); else withoutStar(resultsList.toSet)
      case c: Chain[_, _] =>

        def findChainValues[T, U](chain: Chain[T, U], cmap: Map[T, Element[U]], pVals: ValueSet[T], samples: Int): Set[ValueSet[U]] = {
          val chainVals = pVals.regularValues.map { parentVal =>
            val resultElem = getOrElseInsert(cmap, parentVal, chain.getUncached(parentVal))
            val result = LazyValues(resultElem.universe)(resultElem, depth - 1, samples, samples)
            usedBy(resultElem) = usedBy.getOrElse(resultElem, Set()) + element
            result
          }
          val newParentVals = LazyValues(chain.parent.universe).storedValues(chain.parent)
          if (newParentVals == pVals) {
            chainVals
          } else {
            findChainValues(chain, cmap, newParentVals, samples)
          }
        }

        val chainMap = getMap(c)
        val parentVS = LazyValues(c.parent.universe).storedValues(c.parent)
        val samplesPerValue = math.max(1, (numTotalSamples.toDouble/parentVS.regularValues.size).toInt)

        val resultVSs = findChainValues(c, chainMap, parentVS, samplesPerValue)

        val startVS: ValueSet[c.Value] =
          if (parentVS.hasStar) withStar[c.Value](Set()); else withoutStar[c.Value](Set())
        resultVSs.foldLeft(startVS)(_ ++ _)
      case i: Inject[_] =>
        val elementVSs = i.args.map(arg => LazyValues(arg.universe).storedValues(arg))
        val incomplete = elementVSs.exists(_.hasStar)
        val elementValues = elementVSs.toList.map(_.regularValues.toList)
        val resultValues = homogeneousCartesianProduct(elementValues: _*).toSet.asInstanceOf[Set[i.Value]]
        if (incomplete) withStar(resultValues); else withoutStar(resultValues)
      case v: ValuesMaker[_] => {
        v.makeValues(depth)
      }
      case a: Atomic[_] => {
        if (!ParticleGenerator.exists(universe)) {
          println("Warning: Sampling element " + a + " even though no sampler defined for this universe")
        }
        val thisSampler = ParticleGenerator(universe)
    	  val samples = thisSampler(a, numArgSamples)
        withoutStar(samples.unzip._2.toSet)
      }
      case _ =>
        /* A new improvement - if we can't compute the values, we just make them *, so the rest of the computation can proceed */
        withStar(Set())
    }

  private def abstractValues[T](element: Element[T], abstraction: Abstraction[T], depth: Int,
      numArgSamples: Int, numTotalSamples: Int): ValueSet[T] = {
    val (inputs, hasStar): (List[T], Boolean) = {
      element match {
        case _: Atomic[_] =>
          val values =
            for { i <- 1 to abstraction.numAbstractPoints * abstraction.numConcretePointsPerAbstractPoint }
              yield element.generateValue(element.generateRandomness)
          (values.toList, false)
        case _ =>
          val values = concreteValues(element, depth, numArgSamples, numTotalSamples)
          (values.regularValues.toList, values.hasStar)
      }
    }
    val results = abstraction.scheme.select(inputs, abstraction.numAbstractPoints)
    if (hasStar) withStar(results); else withoutStar(results)
  }

  /*
   * The memoized values for an element contains the value set that was found so far for the element, together with
   * the depth of expansion that was performed.
   */
  private val memoValues: Map[Element[_], (ValueSet[_], Int)] = Map()
  universe.register(memoValues)

  private val requiredDepths: Map[Element[_], Int] = Map()
  universe.register(requiredDepths)

  /*
   * If we increase the depth at which an element was expanded, we need to make sure to recompute the values of elements that depend on them.
   * For this reason, we keep track of which elements a given element is used by.
   * This usedBy map is different from the one in Universe because it only contains usedBy elements in this values computation.
   */
  private val usedBy: Map[Element[_], Set[Element[_]]] = Map()
  universe.register(usedBy)

  /**
   * Returns the range of values of an element. This method is memoized.
   * If it has previously been called on the same element with a depth at least as great as this one,
   * or if a previous call has resulted in a result with no Star, the previous result is reused.
   */
  def apply[T](element: Element[T], depth: Int): ValueSet[T] = {
    val (numArgSamples, numTotalSamples) = if (ParticleGenerator.exists(universe)) {
      val pg = ParticleGenerator(universe)
      (pg.numArgSamples, pg.numTotalSamples )
    } else {
      (ParticleGenerator.defaultArgSamples, ParticleGenerator.defaultTotalSamples)
    }
    apply(element, depth, numArgSamples, numTotalSamples)
  }

  def apply[T](element: Element[T], depth: Int, numArgSamples: Int, numTotalSamples: Int): ValueSet[T] = {
    val myDepth = requiredDepths.getOrElse(element, -1).max(depth)
    if (LazyValues.debug) {
      println("Computing values for " + element.toNameString + "@" + element.hashCode + ", depth = " + myDepth)
    }
    memoValues.get(element) match {
      case Some((result, memoDepth)) if !result.hasStar || memoDepth >= myDepth =>
        if (LazyValues.debug) {
          println("Found values for " + element.toNameString + "@" + element.hashCode + " in cache: " + result)
        }
        result.asInstanceOf[ValueSet[T]]
      case _ =>
        val vs = if (myDepth >= 0) values(element, myDepth, numArgSamples, numTotalSamples); else withStar[T](Set())
        memoValues += element -> (vs, myDepth)
        if (LazyValues.debug) {
          println("Newly computed values for " + element.toNameString + ": " + vs)
        }
        // Propagate changes to any elements that this element is used by
        // We know the depth at which the other element was computed, according to memoValues.
        // We must make sure to clear the cache so we actually recompute the other element at its depth.
        // We must also make sure to clear the usedBy entry to make sure we don't get into a loop.
        for { otherElement <- usedBy.getOrElse(element, Set()) } {
          val values = LazyValues(otherElement.universe)
          values.memoValues.get(otherElement) match {
            case Some((_, otherDepth)) =>
              usedBy(element) -= otherElement
              values.memoValues -= otherElement
              values(otherElement, otherDepth)
            case None => () // we must be in the process of computing the other element, so we don't need to do it again
          }
        }
        vs

    }
  }

  /**
   * This code ensures that if there are multiple elements that need to be expanded to a certain depth, then if one uses another, the full value set
   * of the second is used in computing the value set of the first.
   */
  def expandAll(elementDepths: scala.collection.Set[(Element[_], Int)]) = {
    for { (element, depth) <- elementDepths } {
      requiredDepths += element -> requiredDepths.getOrElse(element, -1).max(depth)
    }
    for { (element, depth) <- elementDepths } {
      this(element, depth)
    }
  }

  /**
   * Returns the elements whose values have been computed.
   */
  def expandedElements: scala.collection.Set[Element[_]] = memoValues.keySet

  /**
   * Returns the previously computed values at maximum depth, if any.
   * If the element has not been expanded, we return the empty value set.
   */
  def storedValues[T](element: Element[T]): ValueSet[T] = {
    memoValues.get(element) match {
      case Some((result, _)) => result.asInstanceOf[ValueSet[T]]
      case None => new ValueSet[T](Set())
    }
  }

  /**
   * Returns the depth to which an element has been expanded, if any.
   */
  def expandedDepth[T](element: Element[T]): Option[Int] = {
    memoValues.get(element).map(_._2)
  }

  /*
   * For factored inference, we need to make sure that every time we use Chain to get a resulting element for a particular parent value,
   * we get the same result element, so that variables in different factors line up.
   * This is achieved by maintaining a cache in chainMaps. For a given element, chainMaps maintains a map from argument values (of type Any) to
   * result elements.
   * Note that this is different from the cache in a caching chain, and even applies to a non-caching chain. There are two reasons a cache is used
   * even for non-caching chains. First, it is necessary for correctness of factored inference. Second, if we are computing the range of values
   * of the chain, we are already computing all the values of the parent, so in most cases the number of parent values will not be so large as to
   * cause memory problems.
   *
   * New development: We have discovered a design pattern where the result of an Apply is a container of elements. For example, see the lazy list example
   * where the result of Apply is a Cons containing a head element and a tail element. In these cases, we also need to make sure that these elements are
   * the same. Therefore, we also have to maintain a map from Apply arguments to their resulting values. This cache is contained in applyMap.
   */
  private val chainMaps: Map[Element[_], Map[Any, Element[_]]] = Map()
  universe.register(chainMaps)

  /**
   * Gets the mapping from parent values to result elements associated with a chain.
   */
  def getMap[T, U](chain: Chain[T, U]): Map[T, Element[U]] = {
    getOrElseInsert(chainMaps, chain, Map[Any, Element[_]]()).asInstanceOf[Map[T, Element[U]]]
  }

  private val applyMaps: Map[Element[_], Map[Any, Any]] = Map()
  universe.register(applyMaps)

  /**
   * Gets the mapping from apply arguments to values.
   */
  def getMap[T1, U](apply: Apply1[T1, U]): Map[T1, U] = {
    getOrElseInsert(applyMaps, apply, Map[Any, Any]()).asInstanceOf[Map[T1, U]]
  }

  /**
   * Gets the mapping from apply arguments to values.
   */
  def getMap[T1, T2, U](apply: Apply2[T1, T2, U]): Map[(T1, T2), U] = {
    getOrElseInsert(applyMaps, apply, Map[Any, Any]()).asInstanceOf[Map[(T1, T2), U]]
  }

  /**
   * Gets the mapping from apply arguments to values.
   */
  def getMap[T1, T2, T3, U](apply: Apply3[T1, T2, T3, U]): Map[(T1, T2, T3), U] = {
    getOrElseInsert(applyMaps, apply, Map[Any, Any]()).asInstanceOf[Map[(T1, T2, T3), U]]
  }

  /**
   * Gets the mapping from apply arguments to values.
   */
  def getMap[T1, T2, T3, T4, U](apply: Apply4[T1, T2, T3, T4, U]): Map[(T1, T2, T3, T4), U] = {
    getOrElseInsert(applyMaps, apply, Map[Any, Any]()).asInstanceOf[Map[(T1, T2, T3, T4), U]]
  }

  /**
   * Gets the mapping from apply arguments to values.
   */
  def getMap[T1, T2, T3, T4, T5, U](apply: Apply5[T1, T2, T3, T4, T5, U]): Map[(T1, T2, T3, T4, T5), U] = {
    getOrElseInsert(applyMaps, apply, Map[Any, Any]()).asInstanceOf[Map[(T1, T2, T3, T4, T5), U]]
  }
}

object LazyValues {
  private val expansions = Map[Universe, LazyValues]()

  var debug = false

  def clear(universe: Universe) {
    expansions.get(universe) match {
      case Some(e) => {
        e.memoValues.clear()
        e.chainMaps.clear()
        e.applyMaps.clear()
        e.usedBy.clear()
        e.requiredDepths.clear()

        universe.deregister(e.memoValues)
        universe.deregister(e.chainMaps)
        universe.deregister(e.applyMaps)
        universe.deregister(e.usedBy)
        universe.deregister(e.requiredDepths)

        universe.deregisterUniverse(expansions)
        expansions -= universe
      }
      case _ => ()
    }
  }

  /**
   * Create an object for computing the range of values of elements in the universe. This object is only
   * created once for a universe.
   */
  def apply(universe: Universe = Universe.universe): LazyValues = {
    expansions.get(universe) match {
      case Some(e) => e
      case None =>
        val expansion = new LazyValues(universe)
        expansions += (universe -> expansion)
        universe.registerUniverse(expansions)
        expansion
    }
  }
}
