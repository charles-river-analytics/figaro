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

package com.cra.figaro.experimental.structured.factory

//import com.cra.figaro.algorithm.lazyfactored._
import com.cra.figaro.algorithm.lazyfactored.{ValueSet, Extended, Regular}
import ValueSet._
import com.cra.figaro.language._
import com.cra.figaro.library.compound._
import com.cra.figaro.algorithm.factored.factors._
import com.cra.figaro.experimental.structured.{ComponentCollection, ProblemComponent, Range}
import com.cra.figaro.util.{MultiSet, HashMultiSet, homogeneousCartesianProduct}

/**
 * A Sub-Factory for Complex Elements
 */
object ComplexFactory {

  /**
   * Factor constructor for a SingleValuedReferenceElement
   */
  def makeFactors[T](cc: ComponentCollection, element: SingleValuedReferenceElement[T]): List[Factor[Double]] = {
    // Make the list of factors starting from the given element collection with the given reference.
    // startVar represents the variable whose value is the value of the reference.
    def make(ec: ElementCollection, startVar: Variable[T], reference: Reference[T]): List[Factor[Double]] = {
      val (firstElem, restRefOpt) = ec.getFirst(reference)
      restRefOpt match {
        case None =>
          val firstVar = Factory.getVariable(cc, firstElem)
          val factor = new SparseFactor[Double](List(firstVar), List(startVar))
          for {
            (firstVal, firstIndex) <- firstVar.range.zipWithIndex
          } {
            val startIndex = startVar.range.indexOf(firstVal)
            factor.set(List(firstIndex, startIndex), 1.0)
          }
          List(factor)

        case Some(restRef) =>
          val firstVar: Variable[firstElem.Value] = Factory.getVariable(cc, firstElem).asInstanceOf[Variable[firstElem.Value]]
          // For each possible first element collection, we:
          // - create an element named restVar to denote the value of the reference starting from that collection
          // - get the factors associated with restVar by calling make(firstCollection, restVar, restRef)
          // - use a conditional selector to select thos factors appropriately
          // Then we take all of the factors for all first element collections.
          val selectedFactors =
            for {
              (firstXValue, firstIndex) <- firstVar.range.zipWithIndex
            } yield {
              if (firstXValue.isRegular) {
                val firstCollection = firstXValue.value.asInstanceOf[ElementCollection]
                val restRange = Range.getRangeOfSingleValuedReference(cc, firstCollection, restRef)
                val restVar: Variable[T] = new Variable(restRange)
                val restFactors = make(firstCollection, restVar, restRef)
                Factory.makeConditionalSelector(cc, startVar, firstVar, firstIndex, restVar) :: restFactors
              } else {
                val dummy = new Variable(ValueSet.withStar[T](Set()))
                List(Factory.makeConditionalSelector(cc, startVar, firstVar, firstIndex, dummy))
              }
            }
        selectedFactors.flatten
      }
    }

    make(element.collection, Factory.getVariable(cc, element), element.reference)
  }

  /**
   * Factor constructor for a MultiValuedReferenceElement
   */
  def makeFactors[T](cc: ComponentCollection, element: MultiValuedReferenceElement[T]): List[Factor[Double]] = {
    def makeEmbeddedInject(inputVariables: List[Variable[MultiSet[T]]]): (Variable[List[MultiSet[T]]], Factor[Double]) = {
      def rule(values: List[Extended[_]]) = {
        val inputXvalues :+ resultXvalue = values
        // See logic under makeCares
        if (inputXvalues.exists(!_.isRegular)) {
          if (!resultXvalue.isRegular) 1.0; else 0.0
        } else if (resultXvalue.isRegular) {
          if (resultXvalue.value.asInstanceOf[List[T]] == inputXvalues.map(_.value.asInstanceOf[T])) 1.0; else 0.0
        } else 0.0
      }
      val argVSs = inputVariables.map(_.valueSet)
      val incomplete = argVSs.exists(_.hasStar)
      val argChoices = argVSs.toList.map(_.regularValues.toList)
      val resultValues: Set[List[MultiSet[T]]] = homogeneousCartesianProduct(argChoices: _*).toSet
      val injectRange = if (incomplete) withStar(resultValues); else withoutStar(resultValues)

      val resultVariable = new Variable(injectRange)
      val factor = new BasicFactor[Double](inputVariables, List(resultVariable))
      factor.fillByRule(rule _)
      (resultVariable, factor)
    }

    def makeEmbeddedApply(injectVar: Variable[List[MultiSet[T]]]): (Variable[MultiSet[T]], Factor[Double]) = {
      def rule(sets: List[MultiSet[T]]): MultiSet[T] = {
        val starter: MultiSet[T] = HashMultiSet[T]()
        sets.foldLeft(starter)(_ union _)
      }
      val applyVS: ValueSet[MultiSet[T]] = injectVar.valueSet.map(rule(_))
      val applyVar = new Variable(applyVS)
      val factor = new SparseFactor[Double](List(injectVar), List(applyVar))
      for { (injectVal, injectIndex) <- injectVar.range.zipWithIndex } {
        if (injectVal.isRegular) {
//        val resultVal = mapper.map(apply.fn(arg1Val.value), applyValues.regularValues)
          val resultVal = rule(injectVal.value)
          val resultIndex = applyVar.range.indexWhere(_.value == resultVal)
          factor.set(List(injectIndex, resultIndex), 1.0)
        } else if (!injectVal.isRegular && applyVar.range.exists(!_.isRegular)) {
          val resultIndex = applyVar.range.indexWhere(!_.isRegular)
          factor.set(List(injectIndex, resultIndex), 1.0)
        }
      }
      (applyVar, factor)
    }

    // Make the list of factors starting from the given element collection with the given reference.
    // startVar represents the variable whose value is the value of the reference.
    def make(ec: ElementCollection, startVar: Variable[MultiSet[T]], reference: Reference[T]): List[Factor[Double]] = {
      val (firstElem, restRefOpt) = ec.getFirst(reference)
      val selectionFactors: List[List[Factor[Double]]] = {
        restRefOpt match {
          case None => {
            // When the reference is simple, it only refers to a single element, so we just get the factor mapping
            // values of that element to values of startVar
            val firstVar = Factory.getVariable(cc, firstElem)
            val factor = new SparseFactor[Double](List(firstVar), List(startVar))
            for {
              (firstVal, firstIndex) <- firstVar.range.zipWithIndex
            } {
              val startIndex =
                if (firstVal.isRegular) startVar.range.indexWhere(_.value == HashMultiSet(firstVal.value))
                else startVar.range.indexWhere(!_.isRegular)
              factor.set(List(firstIndex, startIndex), 1.0)
            }
            List(List(factor))
          }

          case Some(restRef) => {
            // When the reference is indirect, we get all factors associated with all values of the first name in the reference.
            // Each first value is either a single element collection, in which case we recurse simply, just like for a single-valued reference.
            // Otherwise, the first value is a traversable of element collections, in which case see below.
            val firstVar = Factory.getVariable(cc, firstElem).asInstanceOf[Variable[firstElem.Value]]
            val selectedFactors =
              for {
                (firstXvalue, firstIndex) <- firstVar.range.zipWithIndex
              } yield {
                if (!firstXvalue.isRegular) {
                  val dummy = new Variable(ValueSet.withStar[MultiSet[T]](Set()))
                  List(Factory.makeConditionalSelector(cc, startVar, firstVar, firstIndex, dummy))
                }
                else {
                  firstXvalue.value match {
                    case firstCollection: ElementCollection =>
                      val restRange = Range.getRangeOfMultiValuedReference(cc, firstCollection, restRef)
                      val restVar: Variable[MultiSet[T]] = new Variable(restRange)
                      val restFactors = make(firstCollection, restVar, restRef)
                      Factory.makeConditionalSelector(cc, startVar, firstVar, firstIndex, restVar) :: restFactors
                    case cs: Traversable[_] =>
                      // If the first value consists of multiple element collections, we first get a list of distinct collections.
                      // This is because multi-valued references use set semantics, whereby if an element is pointed to more than once,
                      // its value only counts once in the multiset value of the reference element.
                      // So, a multiset value consists of a value for each of the distinct element collections.
                      // For each collection, we get the factors for it using make(firstCollection, restVar, restRef)
                      // We add a conditional selector on the first value to determine when these factors are relevant.
                      // Then, we essentially create an embedded Inject on the variables representing these collections,
                      // and then an Apply that takes a list of values from these collections and turns them into a multiset.
                      // However, unlike the old implementation, we do not actually create these as elements, as that would
                      // break the atomicity of makeFactors required by structured factored inference.
                      // We just create a variable for the Inject and a variable for the Apply and create the factors directly.
                      val collections = cs.asInstanceOf[Traversable[ElementCollection]].toList.distinct // Set semantics
                      val factorsForCollections =
                        for { firstCollection <- collections } yield {
                          val restRange = Range.getRangeOfMultiValuedReference(cc, firstCollection, restRef)
                          val restVar: Variable[MultiSet[T]] = new Variable(restRange)
                          val restFactors = make(firstCollection, restVar, restRef)
                          (restVar, restFactors)
                        }
                      val (injectVar, injectFactor) = makeEmbeddedInject(factorsForCollections.map(_._1))
                      val (applyVar, applyFactor) = makeEmbeddedApply(injectVar)
                      val valueFactors = applyFactor :: injectFactor :: factorsForCollections.map(_._2).flatten
                      Factory.makeConditionalSelector(cc, startVar, firstVar, firstIndex, applyVar) :: valueFactors
                  }
                }
              }
            selectedFactors
          }
        }
      }
      selectionFactors.flatten
    }

    make(element.collection, Factory.getVariable(cc, element), element.reference)
  }

  /**
   * Factor constructor for an Aggregate Element
   */
  def makeFactors[T, U](cc: ComponentCollection, element: Aggregate[T, U]): List[Factor[Double]] = {
    val elementVar = Factory.getVariable(cc, element)
    val mvreVar = Factory.getVariable(cc, element.mvre)
    val factor = new SparseFactor[Double](List(mvreVar), List(elementVar)).setBasicMap
    for {
      (mvreXvalue, mvreIndex) <- mvreVar.range.zipWithIndex
    } {
      if (mvreXvalue.isRegular) {
        val aggValue = element.aggregate(mvreXvalue.value)
        val elementIndex = elementVar.range.indexOf(Regular(aggValue))
        factor.set(List(mvreIndex, elementIndex), 1.0)
      } else factor.set(List(mvreIndex, elementVar.range.indexWhere(!_.isRegular)), 1.0)
    }
    // The MultiValuedReferenceElement for this aggregate is generated when values is called.
    // Therefore, it will be included in the expansion and have factors made for it automatically, so we do not create factors for it here.
    List(factor)
  }

  /*
   * Constructor for a MakeList Element.
   *
   * MakeList is not supported by SFI.
   */
//  def makeFactors[T](cc: ComponentCollection, element: MakeList[T]): List[Factor[Double]] = {
//    val parentVar = Variable(element.numItems)
//    // We need to create factors for the items and the lists themselves, which are encapsulated in this MakeList
//    val regularParents = parentVar.range.filter(_.isRegular).map(_.value)
//    val maxItem = regularParents.reduce(_ max _)
//    val itemFactors = List.tabulate(maxItem)((i: Int) => Factory.make(element.items(i)))
//    val indexedResultElemsAndFactors =
//      for { i <- regularParents } yield {
//        val elem = element.embeddedInject(i)
//        val factors = Factory.make(elem)
//        (Regular(i), elem, factors)
//      }
//    val conditionalFactors =
//      parentVar.range.zipWithIndex map (pair =>
//        Factory.makeConditionalSelector(element, parentVar, pair._2, Variable(indexedResultElemsAndFactors.find(_._1 == pair._1).get._2)))
//    conditionalFactors ::: itemFactors.flatten ::: indexedResultElemsAndFactors.flatMap(_._3)
//  }

  // adapted from Apply1
  /**
   * Factor constructor for a MakeArray Element
   */
  def makeFactors[T](cc: ComponentCollection, element: com.cra.figaro.library.collection.MakeArray[T]): List[Factor[Double]] = {
    val arg1Var = Factory.getVariable(cc, element.numItems)
    val resultVar = Factory.getVariable(cc, element)
    val makeArrayComponent = cc(element)
    val factor = new SparseFactor[Double](List(arg1Var), List(resultVar)).setBasicMap
    val arg1Indices = arg1Var.range.zipWithIndex
    val resultIndices = resultVar.range.zipWithIndex
    for {
      (arg1Val, arg1Index) <- arg1Indices
      (resultVal, resultIndex) <- resultIndices
    } {
      if ((arg1Val.isRegular && arg1Val.value <= makeArrayComponent.maxExpanded &&
           resultVal.isRegular && resultVal.value == element.arrays(arg1Val.value) ||
          (!arg1Val.isRegular || arg1Val.value > makeArrayComponent.maxExpanded) && !resultVal.isRegular)) {
        factor.set(List(arg1Index, resultIndex), 1.0)
      }
    }
    List(factor)
  }

  /**
   * Factor constructor for a FoldLeft Element
   */
  def makeFactors[T,U](cc: ComponentCollection, fold: FoldLeft[T,U]): List[Factor[Double]] = {
    def makeOneFactor(currentAccumVar: Variable[U], elemVar: Variable[T], nextAccumVar: Variable[U]): Factor[Double] = {
      val result = new SparseFactor[Double](List(currentAccumVar, elemVar), List(nextAccumVar))
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
          if (nextAccumVal.value == fold.function(currentAccumVal.value, elemVal.value))
          result.set(List(currentAccumIndex, elemIndex, nextAccumIndex), 1.0)
        } else if ((!currentAccumVal.isRegular || !elemVal.isRegular) && !nextAccumVal.isRegular) {
          result.set(List(currentAccumIndex, elemIndex, nextAccumIndex), 1.0)
        }
    }
      result
    }

    def makeFactorSequence(currentAccumVar: Variable[U], remaining: Seq[Element[T]]): List[Factor[Double]] = {
      if (remaining.isEmpty) List()
      else {
        val firstVar = Factory.getVariable(cc, remaining.head)
        val rest = remaining.tail
        val nextAccumVar =
          if (rest.isEmpty) Factory.getVariable(cc, fold)
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
