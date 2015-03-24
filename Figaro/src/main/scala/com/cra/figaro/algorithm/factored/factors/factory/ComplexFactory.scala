/*
 * ComplexFactory.scala
 * Description needed
 * 
 * Created By:      Glenn Takata (gtakata@cra.com)
 * Creation Date:   Dec 15, 2014
 * 
 * Copyright 2014 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.algorithm.factored.factors.factory

import com.cra.figaro.algorithm.lazyfactored._
import com.cra.figaro.language._
import com.cra.figaro.library.compound._
import com.cra.figaro.algorithm.factored.factors._

/**
 * A Sub-Factory for Complex Elements
 */
object ComplexFactory {

  /**
   * Factor constructor for a SingleValuedReferenceElement
   */
  def makeFactors[T](element: SingleValuedReferenceElement[T]): List[Factor[Double]] = {
    val (first, rest) = element.collection.getFirst(element.reference)
    rest match {
      case None =>
        val elementVar = Variable(element)
        val firstVar = Variable(first)
        val factor = new BasicFactor[Double](List(firstVar), List(elementVar))
        for {
          i <- 0 until firstVar.range.size
          j <- 0 until elementVar.range.size
        } {
          factor.set(List(i, j), (if (i == j) 1.0; else 0.0))
        }
        List(factor)
      case Some(restRef) =>
        val firstVar = Variable(first)
        val selectedFactors =
          for {
            (firstXvalue, firstIndex) <- firstVar.range.zipWithIndex
            firstCollection = firstXvalue.value.asInstanceOf[ElementCollection]
            restElement = element.embeddedElements(firstCollection)
          } yield {
            Factory.makeConditionalSelector(element, firstVar, firstIndex, Variable(restElement)) :: makeFactors(restElement)
          }
        selectedFactors.flatten
    }
  }
  
  /**
   * Factor constructor for a MultiValuedReferenceElement
   */
  def makeFactors[T](element: MultiValuedReferenceElement[T]): List[Factor[Double]] = {
    val (first, rest) = element.collection.getFirst(element.reference)
    val selectionFactors: List[List[Factor[Double]]] = {
      rest match {
        case None =>
          val elementVar = Variable(element)
          val firstVar = Variable(first)
          val factor = new BasicFactor[Double](List(firstVar), List(elementVar))
          for {
            i <- 0 until firstVar.range.size
            j <- 0 until elementVar.range.size
          } {
            factor.set(List(i, j), (if (i == j) 1.0; else 0.0))
          }
          List(List(factor))
        case Some(restRef) =>
          val firstVar = Variable(first)
          for {
            (firstXvalue, firstIndex) <- firstVar.range.zipWithIndex
          } yield {
            if (firstXvalue.isRegular) {
              firstXvalue.value match {
                case firstCollection: ElementCollection =>
                  val restElement = element.embeddedElements(firstCollection)
                  val result: List[Factor[Double]] =
                    Factory.makeConditionalSelector(element, firstVar, firstIndex, Variable(restElement)) :: Factory.make(restElement)
                  result
                case cs: Traversable[_] =>
                  // Create a multi-valued reference element (MVRE) for each collection in the value of the first name.
                  // Since the first name is multi-valued, its value is the union of the values of all these MVREs.
                  val collections = cs.asInstanceOf[Traversable[ElementCollection]].toList.distinct // Set semantics
                  val multis: List[MultiValuedReferenceElement[T]] = collections.map(element.embeddedElements(_)).toList
                  // Create the element that takes the union of the values of the all the MVREs.
                  // The combination and setMaker elements are encapsulated within this object and are created now, so we need to create factors for them.
                  // Finally, we create a conditional selector (see ProbFactor) to select the appropriate result value when the first
                  // name's value is these MVREs.
                  val combination = element.embeddedInject(collections)
                  val setMaker = element.embeddedApply(collections)
                  val result: List[Factor[Double]] =
                    Factory.makeConditionalSelector(element, firstVar, firstIndex, Variable(setMaker)) :: Factory.make(combination) :::
                      Factory.make(setMaker)
                  result
              }
            } else StarFactory.makeStarFactor(element)
          }
      }
    }
    selectionFactors.flatten
  }

  /**
   * Factor constructor for an Aggregate Element
   */
  def makeFactors[T, U](element: Aggregate[T, U]): List[Factor[Double]] = {
    val elementVar = Variable(element)
    val mvreVar = Variable(element.mvre)
    val factor = new BasicFactor[Double](List(mvreVar), List(elementVar))
    for {
      (mvreXvalue, mvreIndex) <- mvreVar.range.zipWithIndex
      (elementXvalue, elementIndex) <- elementVar.range.zipWithIndex
    } {
      if (elementXvalue.isRegular && mvreXvalue.isRegular) factor.set(List(mvreIndex, elementIndex), if (element.aggregate(mvreXvalue.value) == elementXvalue.value) 1.0; else 0.0)
    }
    // The MultiValuedReferenceElement for this aggregate is generated when values is called.
    // Therefore, it will be included in the expansion and have factors made for it automatically, so we do not create factors for it here.
    List(factor)
  }

  /**
   * Constructor for a MakeList Element
   */
  def makeFactors[T](element: MakeList[T]): List[Factor[Double]] = {
    val parentVar = Variable(element.numItems)
    // We need to create factors for the items and the lists themselves, which are encapsulated in this MakeList
    val regularParents = parentVar.range.filter(_.isRegular).map(_.value)
    val maxItem = regularParents.reduce(_ max _)
    val itemFactors = List.tabulate(maxItem)((i: Int) => Factory.make(element.items(i)))
    val indexedResultElemsAndFactors =
      for { i <- regularParents } yield {
        val elem = element.embeddedInject(i)
        val factors = Factory.make(elem)
        (Regular(i), elem, factors)
      }
    val conditionalFactors =
      parentVar.range.zipWithIndex map (pair =>
        Factory.makeConditionalSelector(element, parentVar, pair._2, Variable(indexedResultElemsAndFactors.find(_._1 == pair._1).get._2)))
    conditionalFactors ::: itemFactors.flatten ::: indexedResultElemsAndFactors.flatMap(_._3)
  }

  // adapted from Apply1
  /**
   * Factor constructor for a MakeArray Element
   */
  def makeFactors[T](element: com.cra.figaro.library.collection.MakeArray[T]): List[Factor[Double]] = {
    val arg1Var = Variable(element.numItems)
    val resultVar = Variable(element)
    val factor = new BasicFactor[Double](List(arg1Var), List(resultVar))
    val arg1Indices = arg1Var.range.zipWithIndex
    val resultIndices = resultVar.range.zipWithIndex
    for {
      (arg1Val, arg1Index) <- arg1Indices
      (resultVal, resultIndex) <- resultIndices
    } {
      val entry =
        if (arg1Val.isRegular && resultVal.isRegular) {
        if (resultVal.value == element.arrays(arg1Val.value)) 1.0
          else 0.0
        } else if (!arg1Val.isRegular && !resultVal.isRegular) 1.0
        else if (!arg1Val.isRegular && resultVal.isRegular) 0.0
        else 0.0
      factor.set(List(arg1Index, resultIndex), entry)
    }
    List(factor)
  }
  
  /**
   * Factor constructor for a FoldLeft Element
   */
  def makeFactors[T,U](fold: FoldLeft[T,U]): List[Factor[Double]] = {
    def makeOneFactor(currentAccumVar: Variable[U], elemVar: Variable[T], nextAccumVar: Variable[U]): Factor[Double] = {
      val result = new BasicFactor[Double](List(currentAccumVar, elemVar), List(nextAccumVar))
      val currentAccumIndices = currentAccumVar.range.zipWithIndex
      val elemIndices = elemVar.range.zipWithIndex
      val nextAccumIndices = nextAccumVar.range.zipWithIndex
      for {
        (currentAccumVal, currentAccumIndex) <- currentAccumIndices
        (elemVal, elemIndex) <- elemIndices
        (nextAccumVal, nextAccumIndex) <- nextAccumIndices
    } {
      val entry =
        if (currentAccumVal.isRegular && elemVal.isRegular && nextAccumVal.isRegular) {
          if (nextAccumVal.value == fold.function(currentAccumVal.value, elemVal.value)) 1.0
          else 0.0
        } else if ((!currentAccumVal.isRegular || !elemVal.isRegular) && !nextAccumVal.isRegular) 1.0
        else 0.0
      result.set(List(currentAccumIndex, elemIndex, nextAccumIndex), entry)
    }
      result
    }

    def makeFactorSequence(currentAccumVar: Variable[U], remaining: Seq[Element[T]]): List[Factor[Double]] = {
      if (remaining.isEmpty) List()
      else {
        val firstVar = Variable(remaining.head)
        val rest = remaining.tail
        val nextAccumVar =
          if (rest.isEmpty) Variable(fold)
          else {
          val currentAccumRegular = currentAccumVar.range.filter(_.isRegular).map(_.value)
          val firstRegular = firstVar.range.filter(_.isRegular).map(_.value)
          val nextVals =
            for {
              accum <- currentAccumRegular
              first <- firstRegular
            } yield fold.function(accum, first)
          val nextHasStar = currentAccumVar.range.exists(!_.isRegular) || firstVar.range.exists(!_.isRegular)
          val nextVS = if (nextHasStar) ValueSet.withStar(nextVals.toSet) else ValueSet.withoutStar(nextVals.toSet)
            new Variable(nextVS)
          }
        val nextFactor = makeOneFactor(currentAccumVar, firstVar, nextAccumVar)
        nextFactor :: makeFactorSequence(nextAccumVar, rest)
      }
    }
    val startVar = new Variable(ValueSet.withoutStar(Set(fold.start)))
    makeFactorSequence(startVar, fold.elements)
  }
}
