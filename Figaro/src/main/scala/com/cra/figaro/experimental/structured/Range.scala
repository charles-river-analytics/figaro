package com.cra.figaro.experimental.structured

import com.cra.figaro.algorithm.lazyfactored.ValueSet
import com.cra.figaro.algorithm.lazyfactored.ValueSet._
import com.cra.figaro.language._
import com.cra.figaro.library.compound.FastIf
import com.cra.figaro.util.{MultiSet, homogeneousCartesianProduct}
import com.cra.figaro.util.HashMultiSet



object Range {
  private def getRange[U](collection: ComponentCollection, otherElement: Element[U]): ValueSet[U] = {
    if (collection.contains(otherElement)) collection(otherElement).range
    else withStar(Set())
  }

  // Get the range of the reference by taking the union of the current ranges of all the current possible targets.
  // Do not add any elements or expand any other ranges in the process in the process.
  private def getRangeOfSingleValuedReference[V](cc: ComponentCollection, ec: ElementCollection, ref: Reference[V]): ValueSet[V] = {
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

  private def getRangeOfMultiValuedReference[V](cc: ComponentCollection, ec: ElementCollection, ref: Reference[V]): ValueSet[MultiSet[V]] = {

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
      val combinations: List[List[List[Element[V]]]] = homogeneousCartesianProduct(subTargetSets:_*)
      val targetSets = combinations.map(_.flatten).toSet
      (targetSets, hasStar)
    }
    
    def getMultiSetPossibilities(targetSet: List[Element[V]]): ValueSet[MultiSet[V]] = {
      val ranges: List[ValueSet[V]] = targetSet.map(getRange(cc, _))
      val regularRanges: List[List[V]] = ranges.map(_.regularValues.toList)
      val possibilities: List[List[V]] = homogeneousCartesianProduct(regularRanges:_*)
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

  def apply[V](component: ProblemComponent[V]): ValueSet[V] = {
    component match {
      case cc: ChainComponent[_,V] => chainRange(cc)
      case _ => nonChainRange(component)
    }
  }

  private def chainRange[P, V](component: ChainComponent[P, V]): ValueSet[V] = {
    val collection = component.problem.collection
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

  private def nonChainRange[V](component: ProblemComponent[V]): ValueSet[V] = {
    val collection = component.problem.collection
    component.element match {
      case c: Constant[_] => withoutStar(Set(c.constant))

      case f: Flip => withoutStar(Set(true, false))

      case s: Select[_, _] => withoutStar(Set(s.outcomes: _*))

      case d: Dist[_, _] =>
        val componentSets = d.outcomes.map(getRange(collection, _))
        componentSets.reduce(_ ++ _)

      case i: FastIf[_] => withoutStar(Set(i.thn, i.els))

      case a: Apply1[_, _] =>
        val vs1 = getRange(collection, a.arg1)
        vs1.map(a.fn)
//        val applyMap = getMap(a)
//        val vs1 = LazyValues(a.arg1.universe).storedValues(a.arg1)
//        val resultsSet =
//          for {
//            arg1Val <- vs1.regularValues
//          } yield {
//            getOrElseInsert(applyMap, arg1Val, a.fn(arg1Val))
//          }
//        if (vs1.hasStar) withStar(resultsSet); else withoutStar(resultsSet)

      case a: Apply2[_, _, _] =>
        val vs1 = getRange(collection, a.arg1)
        val vs2 = getRange(collection, a.arg2)
        val resultSet =
          for {
            v1 <- vs1.regularValues
            v2 <- vs2.regularValues
          } yield a.fn(v1, v2)
        if (vs1.hasStar || vs2.hasStar) withStar(resultSet) else withoutStar(resultSet)
//        val applyMap = getMap(a)
//        val vs1 = LazyValues(a.arg1.universe).storedValues(a.arg1)
//        val vs2 = LazyValues(a.arg2.universe).storedValues(a.arg2)
//        val choices = cartesianProduct(vs1.xvalues.toList, vs2.xvalues.toList).asInstanceOf[List[List[Extended[_]]]]
//        val resultsList =
//          for {
//            List(arg1, arg2) <- choices
//            if (arg1.isRegular && arg2.isRegular)
//          } yield {
//            val arg1Val = arg1.value.asInstanceOf[a.Arg1Type]
//            val arg2Val = arg2.value.asInstanceOf[a.Arg2Type]
//            getOrElseInsert(applyMap, (arg1Val, arg2Val), a.fn(arg1Val, arg2Val))
//          }
//        if (vs1.hasStar || vs2.hasStar) withStar(resultsList.toSet); else withoutStar(resultsList.toSet)

      case a: Apply3[_, _, _, _] =>
        val vs1 = getRange(collection, a.arg1)
        val vs2 = getRange(collection, a.arg2)
        val vs3 = getRange(collection, a.arg3)
        val resultSet =
          for {
            v1 <- vs1.regularValues
            v2 <- vs2.regularValues
            v3 <- vs3.regularValues
          } yield a.fn(v1, v2, v3)
        if (vs1.hasStar || vs2.hasStar || vs3.hasStar) withStar(resultSet) else withoutStar(resultSet)
//        val applyMap = getMap(a)
//        val vs1 = LazyValues(a.arg1.universe).storedValues(a.arg1)
//        val vs2 = LazyValues(a.arg2.universe).storedValues(a.arg2)
//        val vs3 = LazyValues(a.arg3.universe).storedValues(a.arg3)
//        val choices = cartesianProduct(vs1.xvalues.toList, vs2.xvalues.toList, vs3.xvalues.toList).asInstanceOf[List[List[Extended[_]]]]
//        val resultsList =
//          for {
//            List(arg1, arg2, arg3) <- choices
//            if (arg1.isRegular && arg2.isRegular && arg3.isRegular)
//          } yield {
//            val arg1Val = arg1.value.asInstanceOf[a.Arg1Type]
//            val arg2Val = arg2.value.asInstanceOf[a.Arg2Type]
//            val arg3Val = arg3.value.asInstanceOf[a.Arg3Type]
//            getOrElseInsert(applyMap, (arg1Val, arg2Val, arg3Val), a.fn(arg1Val, arg2Val, arg3Val))
//          }
//        if (vs1.hasStar || vs2.hasStar || vs3.hasStar) withStar(resultsList.toSet); else withoutStar(resultsList.toSet)

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
          } yield a.fn(v1, v2, v3, v4)
        if (vs1.hasStar || vs2.hasStar || vs3.hasStar || vs4.hasStar) withStar(resultSet) else withoutStar(resultSet)
//        val applyMap = getMap(a)
//        val vs1 = LazyValues(a.arg1.universe).storedValues(a.arg1)
//        val vs2 = LazyValues(a.arg2.universe).storedValues(a.arg2)
//        val vs3 = LazyValues(a.arg3.universe).storedValues(a.arg3)
//        val vs4 = LazyValues(a.arg4.universe).storedValues(a.arg4)
//        val choices = cartesianProduct(vs1.xvalues.toList, vs2.xvalues.toList, vs3.xvalues.toList, vs4.xvalues.toList).asInstanceOf[List[List[Extended[_]]]]
//        val resultsList =
//          for {
//            List(arg1, arg2, arg3, arg4) <- choices
//            if (arg1.isRegular && arg2.isRegular && arg3.isRegular && arg4.isRegular)
//          } yield {
//            val arg1Val = arg1.value.asInstanceOf[a.Arg1Type]
//            val arg2Val = arg2.value.asInstanceOf[a.Arg2Type]
//            val arg3Val = arg3.value.asInstanceOf[a.Arg3Type]
//            val arg4Val = arg4.value.asInstanceOf[a.Arg4Type]
//            getOrElseInsert(applyMap, (arg1Val, arg2Val, arg3Val, arg4Val), a.fn(arg1Val, arg2Val, arg3Val, arg4Val))
//          }
//        if (vs1.hasStar || vs2.hasStar || vs3.hasStar || vs4.hasStar) withStar(resultsList.toSet); else withoutStar(resultsList.toSet)

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
          } yield a.fn(v1, v2, v3, v4, v5)
        if (vs1.hasStar || vs2.hasStar || vs3.hasStar || vs4.hasStar || vs5.hasStar) withStar(resultSet) else withoutStar(resultSet)
//        val applyMap = getMap(a)
//        val vs1 = LazyValues(a.arg1.universe).storedValues(a.arg1)
//        val vs2 = LazyValues(a.arg2.universe).storedValues(a.arg2)
//        val vs3 = LazyValues(a.arg3.universe).storedValues(a.arg3)
//        val vs4 = LazyValues(a.arg4.universe).storedValues(a.arg4)
//        val vs5 = LazyValues(a.arg5.universe).storedValues(a.arg5)
//        val choices = cartesianProduct(vs1.xvalues.toList, vs2.xvalues.toList, vs3.xvalues.toList, vs4.xvalues.toList, vs5.xvalues.toList).asInstanceOf[List[List[Extended[_]]]]
//        val resultsList =
//          for {
//            List(arg1, arg2, arg3, arg4, arg5) <- choices
//            if (arg1.isRegular && arg2.isRegular && arg3.isRegular && arg4.isRegular && arg5.isRegular)
//          } yield {
//            val arg1Val = arg1.value.asInstanceOf[a.Arg1Type]
//            val arg2Val = arg2.value.asInstanceOf[a.Arg2Type]
//            val arg3Val = arg3.value.asInstanceOf[a.Arg3Type]
//            val arg4Val = arg4.value.asInstanceOf[a.Arg4Type]
//            val arg5Val = arg5.value.asInstanceOf[a.Arg5Type]
//            getOrElseInsert(applyMap, (arg1Val, arg2Val, arg3Val, arg4Val, arg5Val), a.fn(arg1Val, arg2Val, arg3Val, arg4Val, arg5Val))
//          }
//        if (vs1.hasStar || vs2.hasStar || vs3.hasStar || vs4.hasStar || vs5.hasStar) withStar(resultsList.toSet); else withoutStar(resultsList.toSet)

      case c: Chain[_, _] =>
        throw new RuntimeException("This shouldn't be called") // The other version of apply should always be called for a chain
//        def findChainValues[T, U](chain: Chain[T, U], cmap: Map[T, Element[U]], pVals: ValueSet[T], samples: Int): Set[ValueSet[U]] = {
//          val chainVals = pVals.regularValues.map { parentVal =>
//            val resultElem = getOrElseInsert(cmap, parentVal, chain.getUncached(parentVal))
//            val result = LazyValues(resultElem.universe)(resultElem, depth - 1, samples, samples)
//            usedBy(resultElem) = usedBy.getOrElse(resultElem, Set()) + element
//            result
//          }
//          val newParentVals = LazyValues(chain.parent.universe).storedValues(chain.parent)
//          if (newParentVals == pVals) {
//            chainVals
//          } else {
//            findChainValues(chain, cmap, newParentVals, samples)
//          }
//        }
//
//        val chainMap = getMap(c)
//        val parentVS = LazyValues(c.parent.universe).storedValues(c.parent)
//        val samplesPerValue = math.max(1, (numTotalSamples.toDouble/parentVS.regularValues.size).toInt)
//
//        val resultVSs = findChainValues(c, chainMap, parentVS, samplesPerValue)
//
//        val startVS: ValueSet[c.Value] =
//          if (parentVS.hasStar) withStar[c.Value](Set()); else withoutStar[c.Value](Set())
//        resultVSs.foldLeft(startVS)(_ ++ _)

      case i: Inject[_] =>
        val argVSs = i.args.map(getRange(collection, _))
//        val elementVSs = i.args.map(arg => LazyValues(arg.universe).storedValues(arg))
        val incomplete = argVSs.exists(_.hasStar)
        val elementValues = argVSs.toList.map(_.regularValues.toList)
        val resultValues = homogeneousCartesianProduct(elementValues: _*).toSet.asInstanceOf[Set[i.Value]]
        if (incomplete) withStar(resultValues); else withoutStar(resultValues)

//      case v: ValuesMaker[_] => {
//        v.makeValues(depth)
//      }

//      case a: Atomic[_] => {
//        if (!ParticleGenerator.exists(element.universe)) {
//          println("Warning: Sampling element " + a + " even though no sampler defined for this universe")
//        }
//        val thisSampler = ParticleGenerator(element.universe)
//        val samples = thisSampler(a, thisSampler.numArgSamples)
//        withoutStar(samples.unzip._2.toSet)
//      }

      case r: SingleValuedReferenceElement[_] => getRangeOfSingleValuedReference(collection, r.collection, r.reference)

      case r: MultiValuedReferenceElement[_] => getRangeOfMultiValuedReference(collection, r.collection, r.reference)

      case a: Aggregate[_,_] =>
        val inputs = getRange(collection, a.mvre)
        val resultValues = inputs.regularValues.map(a.aggregate(_))
        if (inputs.hasStar) withStar(resultValues); else withoutStar(resultValues)

      case _ =>
        /* A new improvement - if we can't compute the values, we just make them *, so the rest of the computation can proceed */
        withStar(Set())
    }
  }

}
