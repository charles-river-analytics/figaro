package com.cra.figaro.experimental.structured

import com.cra.figaro.language._
import com.cra.figaro.algorithm.lazyfactored.ValueSet
import ValueSet._
import com.cra.figaro.util.homogeneousCartesianProduct
import com.cra.figaro.algorithm.factored.ParticleGenerator

object Range {
  def apply[V](component: ProblemComponent[V]): ValueSet[V] = {
    component match {
      case cc: ChainComponent[_,V] => chainRange(cc)
      case _ => nonChainRange(component)
    }
  }

  private def chainRange[P, V](component: ChainComponent[P, V]): ValueSet[V] = {
    def getRange[U](otherElement: Element[U]): ValueSet[U] = {
      val collection = component.problem.collection
      if (collection.contains(otherElement)) collection(otherElement).range
      else withStar(Set())
    }

    val parentVs = getRange(component.chain.parent)
    val resultVs =
      for {
        parentV <- parentVs.regularValues
        subproblem <- component.subproblems.get(parentV)
      } yield getRange(subproblem.target)
    val fullyExpanded = parentVs.regularValues.forall(component.subproblems.contains(_))
    val starter: ValueSet[V] = if (parentVs.hasStar || !fullyExpanded) withStar(Set()) else withoutStar(Set())
    resultVs.foldLeft(starter)(_ ++ _)
  }

  private def nonChainRange[V](component: ProblemComponent[V]): ValueSet[V] = {
    def getRange[U](otherElement: Element[U]): ValueSet[U] = {
      val collection = component.problem.collection
      if (collection.contains(otherElement)) collection(otherElement).range
      else withStar(Set())
    }

    component.element match {
      case c: Constant[_] => withoutStar(Set(c.constant))

      case f: Flip => withoutStar(Set(true, false))

      case s: Select[_, _] => withoutStar(Set(s.outcomes: _*))

      case d: Dist[_, _] =>
        val componentSets = d.outcomes.map(getRange(_))
        componentSets.reduce(_ ++ _)

      //case i: FastIf[_] => withoutStar(Set(i.thn, i.els))

      case a: Apply1[_, _] =>
        val vs1 = getRange(a.arg1)
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
        val vs1 = getRange(a.arg1)
        val vs2 = getRange(a.arg2)
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
        val vs1 = getRange(a.arg1)
        val vs2 = getRange(a.arg2)
        val vs3 = getRange(a.arg3)
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
        val vs1 = getRange(a.arg1)
        val vs2 = getRange(a.arg2)
        val vs3 = getRange(a.arg3)
        val vs4 = getRange(a.arg4)
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
        val vs1 = getRange(a.arg1)
        val vs2 = getRange(a.arg2)
        val vs3 = getRange(a.arg3)
        val vs4 = getRange(a.arg4)
        val vs5 = getRange(a.arg5)
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
        val argVSs = i.args.map(getRange(_))
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

      case _ =>
        /* A new improvement - if we can't compute the values, we just make them *, so the rest of the computation can proceed */
        withStar(Set())
    }
  }

}
