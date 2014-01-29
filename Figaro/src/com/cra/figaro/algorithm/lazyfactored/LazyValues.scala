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

/*
 * The Values class takes a universe and a depth and provides a 
 * Computing values in a lazy way requires a depth to be specified. Every time a Chain is expanded, the depth is decreased by 1. If the depth is 0, the chain is
 * not expanded, and its value set contains only Star, which means unspecified values.
 * 
 */

class LazyValues(universe: Universe) {
  private def values[T](element: Element[T], depth: Int): ValueSet[T] = {
    // In some cases (e.g. CompoundFlip), we might know the value of an element without getting the values of its arguments.
    // However, future algorithms rely on the values of the arguments having been gotten already.
    // Therefore, we compute the values of the arguments (to a lesser depth) first, 
    // and use the stored values of the arguments when we actually compute the values of the element.
    for { arg <- element.args } {
      this(arg, depth - 1)
      usedBy(arg) = usedBy.getOrElse(arg, Set()) + element
    }
    
    Abstraction.fromPragmas(element.pragmas) match {
      case None => concreteValues(element, depth)
      case Some(abstraction) => abstractValues(element, abstraction, depth)
    }
    
  }

  private def concreteValues[T](element: Element[T], depth: Int): ValueSet[T] =
    element match {
      case c: Constant[_] => withoutStar(Set(c.constant))
      case f: Flip => withoutStar(Set(true, false))
      case d: Select[_, _] => withoutStar(Set(d.outcomes: _*))
      case d: Dist[_, _] => 
        val componentSets = d.outcomes.map(storedValues(_))
        componentSets.reduce(_ ++ _)
      case i: FastIf[_] => withoutStar(Set(i.thn, i.els))
      case a: Apply1[_, _] => 
        val vs1 = storedValues(a.arg1)
        vs1.map(a.fn)
      case a: Apply2[_, _, _] =>
        val vs1 = storedValues(a.arg1)
        val vs2 = storedValues(a.arg2)
        val choices = cartesianProduct(vs1.values.toList, vs2.values.toList).asInstanceOf[List[List[Extended[_]]]]
        val resultsList = 
          for {
            List(arg1, arg2) <- choices
            if (arg1.isRegular && arg2.isRegular)
          } yield a.fn(arg1.value.asInstanceOf[a.Arg1Type], arg2.value.asInstanceOf[a.Arg2Type])
        val results = resultsList.toSet
        if (vs1.hasStar || vs2.hasStar) withStar(resultsList.toSet); else withoutStar(resultsList.toSet) 
     case a: Apply3[_, _, _, _] =>
        val vs1 = storedValues(a.arg1)
        val vs2 = storedValues(a.arg2)
        val vs3 = storedValues(a.arg3)
        val choices = cartesianProduct(vs1.values.toList, vs2.values.toList, vs3.values.toList).asInstanceOf[List[List[Extended[_]]]]
        val resultsList = 
          for {
            List(arg1, arg2, arg3) <- choices
            if (arg1.isRegular && arg2.isRegular && arg3.isRegular)
          } yield a.fn(arg1.value.asInstanceOf[a.Arg1Type], arg2.value.asInstanceOf[a.Arg2Type], arg3.value.asInstanceOf[a.Arg3Type])
        val results = resultsList.toSet
        if (vs1.hasStar || vs2.hasStar || vs3.hasStar) withStar(resultsList.toSet); else withoutStar(resultsList.toSet) 
      case a: Apply4[_, _, _, _, _] =>
        val vs1 = storedValues(a.arg1)
        val vs2 = storedValues(a.arg2)
        val vs3 = storedValues(a.arg3)
        val vs4 = storedValues(a.arg4)
        val choices = cartesianProduct(vs1.values.toList, vs2.values.toList, vs3.values.toList, vs4.values.toList).asInstanceOf[List[List[Extended[_]]]]
        val resultsList = 
          for {
            List(arg1, arg2, arg3, arg4) <- choices
            if (arg1.isRegular && arg2.isRegular && arg3.isRegular && arg4.isRegular)
          } yield a.fn(arg1.value.asInstanceOf[a.Arg1Type], arg2.value.asInstanceOf[a.Arg2Type], arg3.value.asInstanceOf[a.Arg3Type], arg4.value.asInstanceOf[a.Arg4Type])
        val results = resultsList.toSet
        if (vs1.hasStar || vs2.hasStar || vs3.hasStar || vs4.hasStar) withStar(resultsList.toSet); else withoutStar(resultsList.toSet) 
      case a: Apply5[_,_, _, _, _, _] =>
        val vs1 = storedValues(a.arg1)
        val vs2 = storedValues(a.arg2)
        val vs3 = storedValues(a.arg3)
        val vs4 = storedValues(a.arg4)
        val vs5 = storedValues(a.arg5)
        val choices = cartesianProduct(vs1.values.toList, vs2.values.toList, vs3.values.toList, vs4.values.toList, vs5.values.toList).asInstanceOf[List[List[Extended[_]]]]
        val resultsList = 
          for {
            List(arg1, arg2, arg3, arg4, arg5) <- choices
            if (arg1.isRegular && arg2.isRegular && arg3.isRegular && arg4.isRegular && arg5.isRegular)
          } yield a.fn(arg1.value.asInstanceOf[a.Arg1Type], arg2.value.asInstanceOf[a.Arg2Type], arg3.value.asInstanceOf[a.Arg3Type], arg4.value.asInstanceOf[a.Arg4Type], arg5.value.asInstanceOf[a.Arg5Type])
        val results = resultsList.toSet
        if (vs1.hasStar || vs2.hasStar || vs3.hasStar || vs4.hasStar || vs5.hasStar) withStar(resultsList.toSet); else withoutStar(resultsList.toSet) 
      case c: Chain[_, _] => 
	      val parentVS = storedValues(c.parent)
	      val resultVSs = 
	        for { 
	          parentValue <- parentVS.regularValues
	          resultElem = c.get(parentValue)
	        } yield {
	          val result = this(resultElem, depth - 1).asInstanceOf[ValueSet[c.Value]]
	          usedBy(resultElem) = usedBy.getOrElse(resultElem, Set()) + element
	          result
	        }
	      val startVS: ValueSet[c.Value] =
	        if (parentVS.hasStar) withStar[c.Value](Set()); else withoutStar[c.Value](Set())
	        resultVSs.foldLeft(startVS)(_ ++ _)
      case i: Inject[_] =>
        val elementVSs = i.args.map(storedValues(_))
        val incomplete = elementVSs.exists(_.hasStar)
        val elementValues = elementVSs.toList.map(_.regularValues.toList)
        val resultValues = homogeneousCartesianProduct(elementValues:_*).toSet.asInstanceOf[Set[i.Value]]
        if (incomplete) withStar(resultValues); else withoutStar(resultValues)
      case v: ValuesMaker[_] => {
        v.makeValues(depth)
      }
      case _ => 
        /* A new improvement - if we can't compute the values, we just make them *, so the rest of the computation can proceed */
        withStar(Set())
    }

  private def abstractValues[T](element: Element[T], abstraction: Abstraction[T], depth: Int): ValueSet[T] = {
    val (inputs, hasStar): (List[T], Boolean) = {
      element match {
        case _: Atomic[_] =>
          val values =
          	for { i <- 1 to abstraction.numAbstractPoints * abstraction.numConcretePointsPerAbstractPoint }
            	yield element.generateValue(element.generateRandomness)
          (values.toList, false)
        case _ => 
          val values = concreteValues(element, depth)
          (values.regularValues.toList, values.hasStar)
      }
    }
    val results = abstraction.scheme.select(inputs, abstraction.numAbstractPoints)
    if (hasStar) withStar(results); else withoutStar(results)
  }

  // We can't use Util.memo because the type of the Map contains existential variables.
  // The problem is that we need to use asInstanceOf to convert the result to a Set of the correct type.
  /* 
   * The memoized values for an element contains the value set that was found so far for the element, together with
   * the depth of expansion that was performed.
   */
  private var memoValues: Map[Element[_], (ValueSet[_], Int)] = Map()

  universe.register(memoValues)

  /**
   * Returns the elements whose values have been computed.
   */
  def expandedElements: scala.collection.Set[Element[_]] = memoValues.keySet
  
  private var requiredElements: Set[Element[_]] = Set()
  private var requiredDepth = 0
  
  /*
   * If we increase the depth at which an element was expanded, we need to make sure to recompute the values of elements that depend on them.
   * For this reason, we keep track of which elements a given element is used by.
   * This usedBy map is different from the one in Universe because it only contains usedBy elements in this values computation.
   */
  private val usedBy: Map[Element[_], Set[Element[_]]] = Map()
  
  
  /**
   * Returns the range of values of an element. This method is memoized.
   * If it has previously been called on the same element with a depth at least as great as this one,
   * or if a previous call has resulted in a result with no Star, the previous result is reused.
   */
  def apply[T](element: Element[T], depth: Int): ValueSet[T] = {
    val myDepth = if (requiredElements.contains(element)) depth.max(requiredDepth); else depth
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
        val vs = if (myDepth >= 0) values(element, myDepth); else withStar[T](Set())
        memoValues += element -> (vs, myDepth)
        if (LazyValues.debug) {
          println("Newly computed values for " + element.toNameString + ": " + vs)
        }
        // Propagate changes to any elements that this element is used by
        // We know the depth at which the other element was computed, according to memoValues.
        // We must make sure to clear the cache so we actually recompute the other element at its depth.
        // We must also make sure to clear the usedBy entry to make sure we don't get into a loop.
        for { otherElement <- usedBy.getOrElse(element, Set()) } { 
          memoValues.get(otherElement) match {
            case Some((_, otherDepth)) =>
              usedBy(element) -= otherElement
              memoValues -= otherElement
              this(otherElement, otherDepth)
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
  def expandAll(elements: Set[Element[_]], depth: Int) = {
    requiredElements = elements
    requiredDepth = depth
    elements.foreach(this(_, depth))
  }
  
  /**
   * Returns the previously computed values at maximum depth, if any.
   */
  def storedValues[T](element: Element[T]): ValueSet[T] = {
    memoValues.get(element) match {
      case Some((result, _)) => result.asInstanceOf[ValueSet[T]]
      case None => new ValueSet[T](Set())
    }
  }

  // A ChainMap maps a chain to a map from parent values to resulting elements
  private val chainMaps: Map[Element[_], Map[_, Element[_]]] = scala.collection.mutable.Map()

  /*
   * Creates the map for a particular chain.
   */
  private def makeMap[T, U](chain: Chain[T, U]): Map[T, Element[U]] = {
    val currentMap = Map[T, Element[U]]()
    for { 
      parentValue <- storedValues(chain.parent).regularValues
      if (!currentMap.contains(parentValue))  
    } currentMap += (parentValue -> chain.get(parentValue))
    currentMap
    //Map((values.storedValues(chain.parent).values.toSeq map (expandValue(chain, _))): _*)
  }

  /**
   * Gets the mapping from parent values to result elements associated with a chain.
   */
  def getMap[T, U](chain: Chain[T, U]): Map[T, Element[U]] = {
    makeMap(chain)
  }


}

object LazyValues {
  private val expansions = scala.collection.mutable.Map[Universe, LazyValues]()

  var debug = false
  
  def clear(universe: Universe) { 
    expansions.get(universe) match {
      case Some(e) => e.memoValues.clear()
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
