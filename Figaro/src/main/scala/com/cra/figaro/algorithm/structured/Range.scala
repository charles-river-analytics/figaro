/*
 * Range.scala
 * Methods for computing the range of elements associated with problem components.
 *
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   March 1, 2015
 *
 * Copyright 2015 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.algorithm.structured

import com.cra.figaro.algorithm.lazyfactored.ValueSet
import com.cra.figaro.algorithm.lazyfactored.ValueSet._
import com.cra.figaro.language._
import com.cra.figaro.library.compound.FastIf
import com.cra.figaro.util.{ MultiSet, homogeneousCartesianProduct }
import com.cra.figaro.util.HashMultiSet
import com.cra.figaro.algorithm.factored.ParticleGenerator
import com.cra.figaro.library.collection.FixedSizeArray
import com.cra.figaro.library.atomic.discrete.{ AtomicBinomial, ParameterizedBinomialFixedNumTrials }
import com.cra.figaro.library.compound.FoldLeft
import com.cra.figaro.library.compound.IntSelector

object Range {
  private def getRange[U](collection: ComponentCollection, otherElement: Element[U]): ValueSet[U] = {
    if (collection.contains(otherElement)) collection(otherElement).range
    else withStar(Set())
  }

  // Get the range of the reference by taking the union of the current ranges of all the current possible targets.
  // Do not add any elements or expand any other ranges in the process in the process.
  private[algorithm] def getRangeOfSingleValuedReference[V](cc: ComponentCollection, ec: ElementCollection, ref: Reference[V]): ValueSet[V] = {
    val (firstElem, restRefOpt) = ec.getFirst(ref)
    restRefOpt match {
      case None => getRange(cc, firstElem.asInstanceOf[Element[V]])
      case Some(restRef) =>
        try {
          val firstRange = getRange(cc, firstElem.asInstanceOf[Element[ElementCollection]])
          val vss: scala.collection.immutable.Set[ValueSet[V]] =
            for {
              firstEC <- firstRange.regularValues
            } yield {
              getRangeOfSingleValuedReference(cc, firstEC, restRef)
            }
          val starter: ValueSet[V] = if (firstRange.hasStar) withStar(Set()) else withoutStar(Set())
          vss.foldLeft(starter)(_ ++ _)
        } catch {
          case _: ClassCastException =>
            println("Warning: head of indirect reference does not refer to an element collection; setting range to empty with *")
            withStar(Set())
        }
    }
  }

  private[algorithm] def getRangeOfMultiValuedReference[V](cc: ComponentCollection, ec: ElementCollection, ref: Reference[V]): ValueSet[MultiSet[V]] = {

    // This function gets all the lists of elements that could be joint targets of the given reference, starting in the given element collection.
    // The first return value of this function is a set of all the possible lists of targets referred to by this reference.
    // The second return value is a flag indicating whether any internal element in the path has *.
    def getTargetSets(currentEC: ElementCollection, currentRef: Reference[V]): (Set[List[Element[V]]], Boolean) = {
      val (firstElem, restRefOpt) = currentEC.getFirst(currentRef)
      restRefOpt match {
        case None => (Set(List(firstElem.asInstanceOf[Element[V]])), false)
        case Some(restRef) =>
          try {
            var hasStar = false
            val firstRange = getRange(cc, firstElem)
            val targetSetSet: Set[Set[List[Element[V]]]] =
              for {
                value <- firstRange.regularValues
              } yield {
                val (targetSets, hs) = getTargetSetsHelper(value, restRef)
                if (hs) hasStar = true
                targetSets
              }
            (targetSetSet.flatten, hasStar || firstRange.hasStar)
          } catch {
            case _: IllegalArgumentException =>
              println("Warning: head of indirect reference does not refer to an element collection; setting range to empty with *")
              (Set(), true)
          }
      }
    }

    // This function gets all the lists of elements that could be joint targets of a set of element collections.
    // The first argument is a value that can be used to create a set of element collections.
    // The second argument is the remaining reference that starts from each of these element collections.
    // For each element collection in the set, we get the possible lists of targets resulting from that element collection.
    // Then we get all possible lists that contain a list for each element collection.
    // Each one of these lists, when flattened, contains a list of elements that is one possible joint target,
    // so it is returned in the result.
    // This function also keeps track of whether any element involved has * in its range.
    def getTargetSetsHelper[T](ecMaker: T, restRef: Reference[V]): (Set[List[Element[V]]], Boolean) = {
      val ecs: List[ElementCollection] = ElementCollection.makeElementCollectionSet(ecMaker).toList
      var hasStar = false
      val subTargetSets: List[List[List[Element[V]]]] =
        for { ec <- ecs.toList } yield {
          val (subTargetSet, hs) = getTargetSets(ec, restRef)
          if (hs) hasStar = true
          subTargetSet.toList
        }
      val combinations: List[List[List[Element[V]]]] = homogeneousCartesianProduct(subTargetSets: _*)
      val targetSets = combinations.map(_.flatten).toSet
      (targetSets, hasStar)
    }

    def getMultiSetPossibilities(targetSet: List[Element[V]]): ValueSet[MultiSet[V]] = {
      val ranges: List[ValueSet[V]] = targetSet.map(getRange(cc, _))
      val regularRanges: List[List[V]] = ranges.map(_.regularValues.toList)
      val possibilities: List[List[V]] = homogeneousCartesianProduct(regularRanges: _*)
      val multiSets: List[MultiSet[V]] =
        for {
          possibility <- possibilities
        } yield {
          val multiSet = new HashMultiSet[V]
          possibility.foreach(multiSet.addOne(_))
          multiSet
        }
      if (ranges.exists(_.hasStar)) withStar(multiSets.toSet) else withoutStar(multiSets.toSet)
    }

    // First step is to get the current possible target sets.
    // Then, for each target set, we get all the possible multisets of their values.
    // Then, we take the union of these multisets.
    // Does not add any elements or expand any other ranges.
    val (targetSets, hasStar) = getTargetSets(ec, ref)
    val multiSetPossibilities = targetSets.map(getMultiSetPossibilities(_))
    val starter: ValueSet[MultiSet[V]] = if (hasStar) withStar(Set()) else withoutStar(Set())
    multiSetPossibilities.foldLeft(starter)(_ ++ _)
  }

  def getRangeOfFold[T, U](cc: ComponentCollection, fold: FoldLeft[T, U]): ValueSet[U] = {
    def helper(currentAccum: ValueSet[U], remainingElements: Seq[Element[T]]): ValueSet[U] = {
      if (remainingElements.isEmpty) currentAccum
      else {
        val firstVS = getRange(cc, remainingElements.head)
        val nextRegular =
          for {
            currentAccumVal <- currentAccum.regularValues
            firstVal <- firstVS.regularValues
          } yield fold.function(currentAccumVal, firstVal)
        val nextHasStar = currentAccum.hasStar || firstVS.hasStar
        val nextAccum = if (nextHasStar) ValueSet.withStar(nextRegular) else ValueSet.withoutStar(nextRegular)
        helper(nextAccum, remainingElements.tail)
      }
    }

    helper(ValueSet.withoutStar(Set(fold.start)), fold.elements)
  }

  def apply[V](component: ProblemComponent[V], numValues: Int): ValueSet[V] = {
    component match {
      case cc: ChainComponent[_, V] => chainRange(cc)
      case mc: MakeArrayComponent[V] => makeArrayRange(mc)
      case ac: ApplyComponent[V] => applyRange(ac)
      case _ => otherRange(component, numValues)
    }
  }

  private def chainRange[P, V](component: ChainComponent[P, V]): ValueSet[V] = {
    val collection = component.problem.collection
    component.chain match {
      // Parameterized binomial needs to be handled specially, because creating the factors for a parameterized element,
      // when the parameterized flag is true, creates a simple factor over the element.
      case b: ParameterizedBinomialFixedNumTrials =>
        val values = (0 to b.numTrials).toSet
        if (getRange(collection, b.parameter).hasStar) withStar(values) else withoutStar(values)

      case _ =>
        val parentVs = getRange(collection, component.chain.parent)
        val resultVs =
          for {
            parentV <- parentVs.regularValues
            subproblem <- component.subproblems.get(parentV)
          } yield getRange(collection, subproblem.target)
        val fullyExpanded = parentVs.regularValues.forall(component.subproblems.contains(_))
        val starter: ValueSet[V] = if (parentVs.hasStar || !fullyExpanded) withStar(Set()) else withoutStar(Set())
        resultVs.foldLeft(starter)(_ ++ _)
    }
  }

  private def makeArrayRange[V](component: MakeArrayComponent[V]): ValueSet[FixedSizeArray[V]] = {
    val collection = component.problem.collection
    val numItemsRange = getRange(collection, component.makeArray.numItems)
    val numItemsMax = numItemsRange.regularValues.foldLeft(0)(_ max _)
    if (numItemsMax <= component.maxExpanded) {
      val resultVs = numItemsRange.regularValues.map(component.makeArray.arrays(_))
      if (numItemsRange.hasStar) withStar(resultVs) else withoutStar(resultVs)
    } else {
      val resultVs = numItemsRange.regularValues.filter(_ <= component.maxExpanded).map(component.makeArray.arrays(_))
      withStar(resultVs)
    }
  }

  private def applyRange[V](component: ApplyComponent[V]): ValueSet[V] = {
    val collection = component.problem.collection
    val applyMap = component.getMap()
    component.element match {

      case i: FastIf[_] =>
        if (getRange(collection, i.test).hasStar) withStar(Set(i.thn, i.els)) else withoutStar(Set(i.thn, i.els))

      case a: Apply1[_, V] =>
        val vs1 = getRange(collection, a.arg1)
        val resultsSet =
          for {
            arg1Val <- vs1.regularValues
          } yield {
            applyMap.getOrElseUpdate(arg1Val, a.fn(arg1Val))
          }
        if (vs1.hasStar) withStar(resultsSet); else withoutStar(resultsSet)

      case a: Apply2[_, _, _] =>
        val vs1 = getRange(collection, a.arg1)
        val vs2 = getRange(collection, a.arg2)
        val resultSet =
          for {
            v1 <- vs1.regularValues
            v2 <- vs2.regularValues
          } yield {
            applyMap.getOrElseUpdate((v1, v2), a.fn(v1, v2))
          }
        if (vs1.hasStar || vs2.hasStar) withStar(resultSet) else withoutStar(resultSet)

      case a: Apply3[_, _, _, _] =>
        val vs1 = getRange(collection, a.arg1)
        val vs2 = getRange(collection, a.arg2)
        val vs3 = getRange(collection, a.arg3)
        val resultSet =
          for {
            v1 <- vs1.regularValues
            v2 <- vs2.regularValues
            v3 <- vs3.regularValues
          } yield {
            applyMap.getOrElseUpdate((v1, v2, v3), a.fn(v1, v2, v3))
          }
        if (vs1.hasStar || vs2.hasStar || vs3.hasStar) withStar(resultSet) else withoutStar(resultSet)

      case a: Apply4[_, _, _, _, _] =>
        val vs1 = getRange(collection, a.arg1)
        val vs2 = getRange(collection, a.arg2)
        val vs3 = getRange(collection, a.arg3)
        val vs4 = getRange(collection, a.arg4)
        val resultSet =
          for {
            v1 <- vs1.regularValues
            v2 <- vs2.regularValues
            v3 <- vs3.regularValues
            v4 <- vs4.regularValues
          } yield {
            applyMap.getOrElseUpdate((v1, v2, v3, v4), a.fn(v1, v2, v3, v4))
          }
        if (vs1.hasStar || vs2.hasStar || vs3.hasStar || vs4.hasStar) withStar(resultSet) else withoutStar(resultSet)

      case a: Apply5[_, _, _, _, _, _] =>
        val vs1 = getRange(collection, a.arg1)
        val vs2 = getRange(collection, a.arg2)
        val vs3 = getRange(collection, a.arg3)
        val vs4 = getRange(collection, a.arg4)
        val vs5 = getRange(collection, a.arg5)
        val resultSet =
          for {
            v1 <- vs1.regularValues
            v2 <- vs2.regularValues
            v3 <- vs3.regularValues
            v4 <- vs4.regularValues
            v5 <- vs5.regularValues
          } yield {
            applyMap.getOrElseUpdate((v1, v2, v3, v4, v5), a.fn(v1, v2, v3, v4, v5))
          }
        if (vs1.hasStar || vs2.hasStar || vs3.hasStar || vs4.hasStar || vs5.hasStar) withStar(resultSet) else withoutStar(resultSet)
    }
  }

  private def otherRange[V](component: ProblemComponent[V], numValues: Int): ValueSet[V] = {
    val collection = component.problem.collection
    component.element match {
      case c: Constant[_] => withoutStar(Set(c.constant))

      case f: AtomicFlip => withoutStar(Set(true, false))

      case f: ParameterizedFlip =>
        if (getRange(collection, f.parameter).hasStar) withStar(Set(true, false)) else withoutStar(Set(true, false))

      case f: CompoundFlip =>
        if (getRange(collection, f.prob).hasStar) withStar(Set(true, false)) else withoutStar(Set(true, false))

      case s: AtomicSelect[_] => withoutStar(Set(s.outcomes: _*))

      case s: ParameterizedSelect[_] =>
        val values = Set(s.outcomes: _*)
        if (getRange(collection, s.parameter).hasStar) withStar(values) else withoutStar(values)

      case s: CompoundSelect[_] =>
        val values = Set(s.outcomes: _*)
        if (s.probs.map(getRange(collection, _)).exists(_.hasStar)) withStar(values) else withoutStar(values)

      case b: AtomicBinomial => ValueSet.withoutStar((0 to b.numTrials).toSet)

      case d: AtomicDist[_] =>
        val componentSets = d.outcomes.map(getRange(collection, _))
        componentSets.reduce(_ ++ _)

      case d: CompoundDist[_] =>
        val componentSets = d.outcomes.map(getRange(collection, _))
        val values = componentSets.reduce(_ ++ _)
        if (d.probs.map(getRange(collection, _)).exists(_.hasStar)) values ++ withStar(Set()) else values

      //case i: FastIf[_] =>
      //  if (getRange(collection, i.test).hasStar) withStar(Set(i.thn, i.els)) else withoutStar(Set(i.thn, i.els))

      case c: Chain[_, _] =>
        throw new RuntimeException("This shouldn't be called") // The other version of apply should always be called for a chain

      case i: Inject[_] =>
        val argVSs = i.args.map(getRange(collection, _))
        //        val elementVSs = i.args.map(arg => LazyValues(arg.universe).storedValues(arg))
        val incomplete = argVSs.exists(_.hasStar)
        val elementValues = argVSs.toList.map(_.regularValues.toList)
        val resultValues = homogeneousCartesianProduct(elementValues: _*).toSet.asInstanceOf[Set[i.Value]]
        if (incomplete) withStar(resultValues); else withoutStar(resultValues)

      case a: Atomic[_] => {
        if (!ParticleGenerator.exists(a.universe)) {
          println("Warning: Sampling element " + a + " even though no sampler defined for this universe")
        }
        val thisSampler = ParticleGenerator(a.universe)
        val samples = thisSampler(a, numValues)
        withoutStar(samples.unzip._2.toSet)
      }

      case r: SingleValuedReferenceElement[_] => getRangeOfSingleValuedReference(collection, r.collection, r.reference)

      case r: MultiValuedReferenceElement[_] => getRangeOfMultiValuedReference(collection, r.collection, r.reference)

      case a: Aggregate[_, _] =>
        val inputs = getRange(collection, a.mvre)
        val resultValues = inputs.regularValues.map(a.aggregate(_))
        if (inputs.hasStar) withStar(resultValues); else withoutStar(resultValues)

      case f: FoldLeft[_, _] => getRangeOfFold(collection, f)

      case i: IntSelector =>
        val counterValues = getRange(collection, i.counter)
        if (counterValues.regularValues.nonEmpty) {
          val maxCounter = counterValues.regularValues.max
          //          val all = List.tabulate(maxCounter)(i => i).toSet
          val all = Set((0 until maxCounter): _*)
          if (counterValues.hasStar) ValueSet.withStar(all); else ValueSet.withoutStar(all)
        } else { ValueSet.withStar(Set()) }

      case _ =>
        /* A new improvement - if we can't compute the values, we just make them *, so the rest of the computation can proceed */
        withStar(Set())
    }
  }

}
