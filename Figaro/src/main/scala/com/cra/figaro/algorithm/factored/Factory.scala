/*
 * ProbFactor.scala
 * Factors over variables.
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Jan 1, 2009
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.algorithm.factored

import com.cra.figaro.algorithm._
import com.cra.figaro.language._
import com.cra.figaro.util._
import annotation.tailrec
import scala.language.existentials
import com.cra.figaro.algorithm.lazyfactored._
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.SetBuilder

/**
 * Methods for creating probabilistic factors associated with elements.
 */
object Factory {
   def makeStarFactor[T](elem: Element[T]): List[Factor[Double]] = {
    val elemVar = Variable(elem)
    require(elemVar.range.size == 1 && elemVar.range(0) == Star[T], "Trying to create a star factor from a value set that is not only star")
    val factor = new BasicFactor[Double](List(elemVar))
    factor.set(List(0), 1.0)
    List(factor)
  }
  
  private def makeFactors[T](const: Constant[T]): List[Factor[Double]] = {
    val factor = new BasicFactor[Double](List(Variable(const)))
    factor.set(List(0), 1.0)
    List(factor)
  }

  private def makeFactors(flip: AtomicFlip): List[Factor[Double]] = {
    val flipVar = Variable(flip)
    if (flipVar.range.exists(!_.isRegular)) {
      assert(flipVar.range.size == 1) // Flip's range must either be {T,F} or {*}
      makeStarFactor(flip)
    } else {
      val factor = new BasicFactor[Double](List(flipVar))
      val i = flipVar.range.indexOf(Regular(true))
      factor.set(List(i), flip.prob)
      factor.set(List(1 - i), 1.0 - flip.prob)
      List(factor)
    } 
  }

  private def makeFactors(flip: CompoundFlip): List[Factor[Double]] = {
    val flipVar = Variable(flip)
    if (flipVar.range.exists(!_.isRegular)) {
      assert(flipVar.range.size == 1) // Flip's range must either be {T,F} or {*}
      makeStarFactor(flip)
    } else {
      val probVar = Variable(flip.prob)
      val factor = new BasicFactor[Double](List(probVar, flipVar))
      val parentVals = probVar.range
      val i = flipVar.range.indexOf(Regular(true))
      for { j <- 0 until parentVals.size } {
        if (parentVals(j).isRegular) {
          val value = parentVals(j).value
          factor.set(List(j, i), value)
          factor.set(List(j, 1 - i), 1.0 - value)
        } else {
          factor.set(List(j, 0), 0.0)
          factor.set(List(j, 1), 0.0)
        }
      }
      List(factor)
    }
  }

  private def makeSimpleDistribution[T](target: Variable[T], probs: List[Double]): Factor[Double] = {
    val factor = new BasicFactor[Double](List(target))
    for { (prob, index) <- probs.zipWithIndex } {
      factor.set(List(index), prob)
    }
    factor
  }

  private def makeComplexDistribution[T](target: Variable[T], probElems: List[Element[Double]]): Factor[Double] = {
    val probVars: List[Variable[Double]] = probElems map (Variable(_))
    val factor = new BasicFactor[Double](List((target :: probVars): _*))
    val probVals: List[List[Extended[Double]]] = probVars map (_.range)
    for { indices <- factor.allIndices } {
      // unnormalized is a list, one for each probability element, of the value of that element under these indices
      val unnormalized = 
        for { (probIndex, position) <- indices.toList.tail.zipWithIndex } yield {
          val xprob = probVals(position)(probIndex) // The probability of the particular value of the probability element in this position
          if (xprob.isRegular) xprob.value; else 0.0
      }
      val normalized = normalize(unnormalized).toArray
      // The first variable specifies the position of the remaining variables, so indices(0) is the correct probability
      factor.set(indices, normalized(indices(0)))
    }
    factor
  }

  private def getProbs[U, T](select: Select[U, T]): List[U] = {
    val selectVar = Variable(select)
    def getProb(xvalue: Extended[T]): U = {
      select.clauses.find(_._2 == xvalue.value).get._1 // * cannot be a value of a Select
    }
    val probs = 
      for { xvalue <- selectVar.range } yield getProb(xvalue) 
    probs
  }

  private def parameterizedGetProbs[T](select: ParameterizedSelect[T]): List[Double] = {
    val outcomes = select.outcomes
    val map = select.parameter.MAPValue
    for {
      xvalue <- Variable(select).range
      index = outcomes.indexOf(xvalue.value)
    } yield map(index)
  }

  private def makeFactors[T](select: AtomicSelect[T]): List[Factor[Double]] = {
    val selectVar = Variable(select)
    if (selectVar.range.exists(!_.isRegular)) {
      assert(selectVar.range.size == 1) // Select's range must either be a list of regular values or {*}
      makeStarFactor(select)
    } else {
      val probs = getProbs(select)
      List(makeSimpleDistribution(selectVar, probs))
    }
  }

  private def makeFactors[T](select: CompoundSelect[T]): List[Factor[Double]] = {
    val selectVar = Variable(select)
    if (selectVar.range.exists(!_.isRegular)) {
      assert(selectVar.range.size == 1) // Select's range must either be a list of regular values or {*}
      makeStarFactor(select)
    } else {
      val probs = getProbs(select)
      List(makeComplexDistribution(selectVar, probs))
    }
  }

  private def makeFactors[T](select: ParameterizedSelect[T]): List[Factor[Double]] = {
    val selectVar = Variable(select)
    if (selectVar.range.exists(!_.isRegular)) {
      assert(selectVar.range.size == 1) // Select's range must either be a list of regular values or {*}
      makeStarFactor(select)
    } else {
      val probs = parameterizedGetProbs(select)
      List(makeSimpleDistribution(selectVar, probs))
    }
  }

  private def makeDontCares[U](factor: Factor[Double],
    intermedIndex: Int,
    overallVar: Variable[U],
    outcomeVar: Variable[U]): Unit = {
    // If we don't care, we assign 1.0 to all combinations of the distVar and outcomeVar
    for {
      j <- 0 until overallVar.size
      k <- 0 until outcomeVar.size
    } {
      factor.set(List(intermedIndex, j, k), 1.0)
    }
  }

  private def makeCares[U](factor: Factor[Double], intermedIndex: Int,
    overallVar: Variable[U], outcomeVar: Variable[U], choices: Set[U])(implicit mapper: PointMapper[U]): Unit = {
    // We care to match up overallVar with outcomeVar
    for {
      (overallVal, j) <- overallVar.range.zipWithIndex
      (outcomeVal, k) <- outcomeVar.range.zipWithIndex
    } {
      // Star stands for "something". If outcomeVal is Star and overallVal is Star, we know something will match something, so the entry is (1,1).
      // If outcomeVal is Star and overallVal is a regular value, then maybe there will be a match, so the entry is (0,1).
      // If outcomeVal is regular, all the probability mass associated with that outcome should be on regular values of overallVal, so the entry is (0,0).
      val entry = 
        if (overallVal.isRegular && outcomeVal.isRegular) {
            if (overallVal.value == mapper.map(outcomeVal.value, choices)) 1.0
            else 0.0 
        } else if (!overallVal.isRegular && !outcomeVal.isRegular) 1.0
        else 0.0
      factor.set(List(intermedIndex, j, k), entry)
    }
  }

    /*
   * The conditional selector creates a factor in which, when the selector's value is such that the result
   * element is relevant to the final result, the result element and overall element must have the same
   * value (handled by makeCares). Otherwise, the result element and overall element can take on any
   * value (handled by makeDontCares)
   */
    /**
   * Make a conditional selector factor used in the decomposition of chain and other elements.
   * A chain defines a factor over the parent element, each of the possible result elements of the chain,
   * and the overall chain element. This can produce a very large factor when there are many result elements.
   * This is solved by decomposing the chain factor into a product of factors, each of which contains the
   * parent element, one of the result elements, and the overall chain element.
   */
  def makeConditionalSelector[T, U](overallElem: Element[U], selector: Variable[T],
    outcomeIndex: Int, outcomeVar: Variable[U])(implicit mapper: PointMapper[U]): Factor[Double] = {
    val overallVar = Variable(overallElem)
    //val outcomeVar = Variable(outcomeElem)
    val overallValues = LazyValues(overallElem.universe).storedValues(overallElem)
    val factor = new BasicFactor[Double](List(selector, overallVar, outcomeVar))
    for { i <- 0 until outcomeIndex } { makeDontCares(factor, i, overallVar, outcomeVar) }
    makeCares(factor, outcomeIndex, overallVar, outcomeVar, overallValues.regularValues)(mapper)
    for { i <- outcomeIndex + 1 until selector.size } { makeDontCares(factor, i, overallVar, outcomeVar) }
    factor
  }

  private def intermedAndClauseFactors[U, T](dist: Dist[U, T]): (Variable[Int], List[Factor[Double]]) = {
    val intermed = new Variable(ValueSet.withoutStar((0 until dist.clauses.size).toSet))
    val clauseFactors = dist.outcomes.zipWithIndex map (pair =>
      makeConditionalSelector(dist, intermed, pair._2, Variable(pair._1)))
    (intermed, clauseFactors)
  }

  private def makeFactors[T](dist: AtomicDist[T]): List[Factor[Double]] = {
    val (intermed, clauseFactors) = intermedAndClauseFactors(dist)
    val intermedFactor = makeSimpleDistribution(intermed, dist.probs)
    intermedFactor :: clauseFactors
  }

  private def makeFactors[T](dist: CompoundDist[T]): List[Factor[Double]] = {
    val (intermed, clauseFactors) = intermedAndClauseFactors(dist)
    val intermedFactor = makeComplexDistribution(intermed, dist.probs)
    intermedFactor :: clauseFactors
  }

  private def makeFactors[T, U](chain: Chain[T, U])(implicit mapper: PointMapper[U]): List[Factor[Double]] = {
    val chainMap: scala.collection.mutable.Map[T, Element[U]] = LazyValues(chain.universe).getMap(chain)
    val parentVar = Variable(chain.parent)
    var tempFactors = parentVar.range.zipWithIndex flatMap (pair => {
      val parentVal = pair._1
      // parentVal.value should have been placed in applyMap at the time the values of this apply were computed.
      // By using chainMap, we can make sure that the result element is the same now as they were when values were computed.
      if (parentVal.isRegular) List(makeConditionalSelector(chain, parentVar, pair._2, Variable(chainMap(parentVal.value)))(mapper))
      else {
        // We create a dummy variable for the outcome variable whose value is always star.
        // We create a dummy factor for that variable.
        // Then we use makeConditionalSelector with the dummy variable
        val dummy = new Variable(ValueSet.withStar[U](Set()))
        val dummyFactor = new BasicFactor[Double](List(dummy))
        dummyFactor.set(List(0), 1.0)
        List(makeConditionalSelector(chain, parentVar, pair._2, dummy), dummyFactor)
      }
    })
    tempFactors
  }

  val maxElementCount = 6
  val maxSize = 500
  val newFactors = ListBuffer[Factor[Double]]()
  val tempFactors = ListBuffer[Factor[Double]]()

  def combineFactors(oldFactors: List[Factor[Double]], semiring: Semiring[Double], removeTemporaries: Boolean): List[Factor[Double]] = {
	newFactors.clear
	tempFactors.clear

	for (factor <- oldFactors)
    {
      if (factor.hasStar)
      {
        newFactors += factor
      }
      else
      {
        tempFactors += factor
      }
    }

    var nextFactor = tempFactors.head

    for (factor <- tempFactors.tail)
    {
        val commonVariables = factor.variables.toSet & nextFactor.variables.toSet
    	
        if (commonVariables.size > 0) 
        {
          val newVariables = factor.variables.toSet -- nextFactor.variables.toSet
          val potentialSize = calculateSize(nextFactor.size, newVariables)
          if ((nextFactor.numVars + newVariables.size) < maxElementCount 
              && potentialSize < maxSize)
          {  
            nextFactor = nextFactor.product(factor, semiring)
          }
          else
          {
            if (removeTemporaries)
            {
              newFactors ++= reduceFactor(nextFactor, semiring, maxElementCount)
            }
            else
            {
              newFactors += nextFactor
            }
            nextFactor = factor
          }
        }
        else
        {
          newFactors += nextFactor
          nextFactor = factor
        } 
    }
    
    if (nextFactor.numVars > 0)
    {
      if (removeTemporaries)
      {
    	  newFactors ++= reduceFactor(nextFactor, semiring, maxElementCount)
      }
      else
      {
          newFactors += nextFactor
      }
    }
    newFactors.toList
  }
 
   val variableSet = scala.collection.mutable.Set[ElementVariable[_]]()
   val nextFactors = ListBuffer[Factor[Double]]()

  private def reduceFactor(factor: Factor[Double], semiring: Semiring[Double], maxElementCount: Int):List[Factor[Double]] = {
	   variableSet.clear
	   
	   (variableSet /: List(factor))(_ ++= _.variables.asInstanceOf[List[ElementVariable[_]]])
       var elementCount = variableSet count (v => !isTemporary(v))
       var resultFactor = Factor.unit[Double](semiring).product(factor, semiring)
       
       var tempCount = 0;
       
       for {variable <- variableSet}
       {   	
    		if (isTemporary(variable) && elementCount <= maxElementCount)
    		{
    			nextFactors.clear
    			nextFactors ++= concreteFactors(variable.asInstanceOf[ElementVariable[_]].element)
    			(variableSet /: nextFactors)(_ ++= _.variables.asInstanceOf[List[ElementVariable[_]]]) 
     	        elementCount = variableSet count (v => !isTemporary(v))
    	  
    			for (nextFactor <- nextFactors)
    			{
    			  resultFactor = resultFactor.product(nextFactor, semiring)
    			}
    			tempCount += 1
    		}
    	}
    
    	if (tempCount > 0 && elementCount <= maxElementCount)
    	{   	  
    		for {variable <- variableSet}
    		{
    			if (isTemporary(variable))
    			{
    				resultFactor = resultFactor.sumOver(variable, semiring)
    			}
    		}
    		
    	}
    	List(resultFactor)
  }
    
  private def calculateSize(currentSize: Int, variables: Set[Variable[_]]) = {
     (currentSize /: variables)(_ * _.size) 
  }
  
  private def isTemporary[_T](variable: Variable[_]): Boolean = {
    variable match {
      case e: ElementVariable[_] => e.element.isTemporary
      case _ => false
    }
  }
  
  private def makeFactors[T, U](apply: Apply1[T, U])(implicit mapper: PointMapper[U]): List[Factor[Double]] = {
    val applyMap: scala.collection.mutable.Map[T, U] = LazyValues(apply.universe).getMap(apply)
    val arg1Var = Variable(apply.arg1)
    val resultVar = Variable(apply)
    val applyValues = LazyValues(apply.universe).storedValues(apply)
    val factor = new BasicFactor[Double](List(arg1Var, resultVar))
    val arg1Indices = arg1Var.range.zipWithIndex
    val resultIndices = resultVar.range.zipWithIndex
    for {
      (arg1Val, arg1Index) <- arg1Indices
      (resultVal, resultIndex) <- resultIndices
    } {
      // See logic in makeCares
      val entry =
        if (arg1Val.isRegular && resultVal.isRegular) {
        // arg1Val.value should have been placed in applyMap at the time the values of this apply were computed.
        // By using applyMap, we can make sure that any contained elements in the result of the apply are the same now as they were when values were computed.
        if (resultVal.value == mapper.map(applyMap(arg1Val.value), applyValues.regularValues)) 1.0
          else 0.0
        } else if (!arg1Val.isRegular && !resultVal.isRegular) 1.0
        else if (!arg1Val.isRegular && resultVal.isRegular) 0.0
        else 0.0
      factor.set(List(arg1Index, resultIndex), entry)
    }
    List(factor)
  }

  private def makeFactors[T1, T2, U](apply: Apply2[T1, T2, U])(implicit mapper: PointMapper[U]): List[Factor[Double]] = {
    val applyMap: scala.collection.mutable.Map[(T1, T2), U] = LazyValues(apply.universe).getMap(apply)
    val arg1Var = Variable(apply.arg1)
    val arg2Var = Variable(apply.arg2)
    val resultVar = Variable(apply)
    val applyValues = LazyValues(apply.universe).storedValues(apply)
    val factor = new BasicFactor[Double](List(arg1Var, arg2Var, resultVar))
    val arg1Indices = arg1Var.range.zipWithIndex
    val arg2Indices = arg2Var.range.zipWithIndex
    val resultIndices = resultVar.range.zipWithIndex
    for {
      (arg1Val, arg1Index) <- arg1Indices
      (arg2Val, arg2Index) <- arg2Indices
      (resultVal, resultIndex) <- resultIndices
    } {
      val entry =
        if (arg1Val.isRegular && arg2Val.isRegular && resultVal.isRegular) {
          // The argument values should have been placed in applyMap at the time the values of this apply were computed.
          // By using applyMap, we can make sure that any contained elements in the result of the apply are the same now as they were when values were computed.
          if (resultVal.value == mapper.map(applyMap((arg1Val.value, arg2Val.value)), applyValues.regularValues)) 1.0
          else 0.0
        } else if ((!arg1Val.isRegular || !arg2Val.isRegular) && !resultVal.isRegular) 1.0
        else if ((!arg1Val.isRegular || !arg2Val.isRegular) && resultVal.isRegular) 0.0
        else 0.0
      factor.set(List(arg1Index, arg2Index, resultIndex), entry)
    }
    List(factor)
  }

  private def makeFactors[T1, T2, T3, U](apply: Apply3[T1, T2, T3, U])(implicit mapper: PointMapper[U]): List[Factor[Double]] = {
    val applyMap: scala.collection.mutable.Map[(T1, T2, T3), U] = LazyValues(apply.universe).getMap(apply)
    val arg1Var = Variable(apply.arg1)
    val arg2Var = Variable(apply.arg2)
    val arg3Var = Variable(apply.arg3)
    val resultVar = Variable(apply)
    val applyValues = LazyValues(apply.universe).storedValues(apply)
    val factor = new BasicFactor[Double](List(arg1Var, arg2Var, arg3Var, resultVar))
    val arg1Indices = arg1Var.range.zipWithIndex
    val arg2Indices = arg2Var.range.zipWithIndex
    val arg3Indices = arg3Var.range.zipWithIndex
    val resultIndices = resultVar.range.zipWithIndex
    for {
      (arg1Val, arg1Index) <- arg1Indices
      (arg2Val, arg2Index) <- arg2Indices
      (arg3Val, arg3Index) <- arg3Indices
      (resultVal, resultIndex) <- resultIndices
    } {
      val entry =
        if (arg1Val.isRegular && arg2Val.isRegular && arg3Val.isRegular && resultVal.isRegular) {
          // The argument values should have been placed in applyMap at the time the values of this apply were computed.
          // By using applyMap, we can make sure that any contained elements in the result of the apply are the same now as they were when values were computed.
          if (resultVal.value == mapper.map(applyMap((arg1Val.value, arg2Val.value, arg3Val.value)), applyValues.regularValues)) 1.0
          else 0.0
        } else if ((!arg1Val.isRegular || !arg2Val.isRegular || !arg3Val.isRegular) && !resultVal.isRegular) 1.0
        else if ((!arg1Val.isRegular || !arg2Val.isRegular || !arg3Val.isRegular) && resultVal.isRegular) 0.0
        else 0.0
      factor.set(List(arg1Index, arg2Index, arg3Index, resultIndex), entry)
    }
    List(factor)
  }

  private def makeFactors[T1, T2, T3, T4, U](apply: Apply4[T1, T2, T3, T4, U])(implicit mapper: PointMapper[U]): List[Factor[Double]] = {
    val applyMap: scala.collection.mutable.Map[(T1, T2, T3, T4), U] = LazyValues(apply.universe).getMap(apply)
    val arg1Var = Variable(apply.arg1)
    val arg2Var = Variable(apply.arg2)
    val arg3Var = Variable(apply.arg3)
    val arg4Var = Variable(apply.arg4)
    val resultVar = Variable(apply)
    val applyValues = LazyValues(apply.universe).storedValues(apply)
    val factor = new BasicFactor[Double](List(arg1Var, arg2Var, arg3Var, arg4Var, resultVar))
    val arg1Indices = arg1Var.range.zipWithIndex
    val arg2Indices = arg2Var.range.zipWithIndex
    val arg3Indices = arg3Var.range.zipWithIndex
    val arg4Indices = arg4Var.range.zipWithIndex
    val resultIndices = resultVar.range.zipWithIndex
    for {
      (arg1Val, arg1Index) <- arg1Indices
      (arg2Val, arg2Index) <- arg2Indices
      (arg3Val, arg3Index) <- arg3Indices
      (arg4Val, arg4Index) <- arg4Indices
      (resultVal, resultIndex) <- resultIndices
    } {
      val entry =
        if (arg1Val.isRegular && arg2Val.isRegular && arg3Val.isRegular && arg4Val.isRegular && resultVal.isRegular) {
          // The argument values should have been placed in applyMap at the time the values of this apply were computed.
          // By using applyMap, we can make sure that any contained elements in the result of the apply are the same now as they were when values were computed.
          if (resultVal.value == mapper.map(applyMap((arg1Val.value, arg2Val.value, arg3Val.value, arg4Val.value)), applyValues.regularValues)) 1.0
          else 0.0
        } else if ((!arg1Val.isRegular || !arg2Val.isRegular || !arg3Val.isRegular || !arg4Val.isRegular) && !resultVal.isRegular) 1.0
        else if ((!arg1Val.isRegular || !arg2Val.isRegular || !arg3Val.isRegular || !arg4Val.isRegular) && resultVal.isRegular) 0.0
        else 0.0
      factor.set(List(arg1Index, arg2Index, arg3Index, arg4Index, resultIndex), entry)
    }
    List(factor)
  }

  private def makeFactors[T1, T2, T3, T4, T5, U](apply: Apply5[T1, T2, T3, T4, T5, U])(implicit mapper: PointMapper[U]): List[Factor[Double]] = {
    val applyMap: scala.collection.mutable.Map[(T1, T2, T3, T4, T5), U] = LazyValues(apply.universe).getMap(apply)
    val arg1Var = Variable(apply.arg1)
    val arg2Var = Variable(apply.arg2)
    val arg3Var = Variable(apply.arg3)
    val arg4Var = Variable(apply.arg4)
    val arg5Var = Variable(apply.arg5)
    val resultVar = Variable(apply)
    val applyValues = LazyValues(apply.universe).storedValues(apply)
    val factor = new BasicFactor[Double](List(arg1Var, arg2Var, arg3Var, arg4Var, arg5Var, resultVar))
    val arg1Indices = arg1Var.range.zipWithIndex
    val arg2Indices = arg2Var.range.zipWithIndex
    val arg3Indices = arg3Var.range.zipWithIndex
    val arg4Indices = arg4Var.range.zipWithIndex
    val arg5Indices = arg5Var.range.zipWithIndex
    val resultIndices = resultVar.range.zipWithIndex
    for {
      (arg1Val, arg1Index) <- arg1Indices
      (arg2Val, arg2Index) <- arg2Indices
      (arg3Val, arg3Index) <- arg3Indices
      (arg4Val, arg4Index) <- arg4Indices
      (arg5Val, arg5Index) <- arg5Indices
      (resultVal, resultIndex) <- resultIndices
    } {
      val entry =
        if (arg1Val.isRegular && arg2Val.isRegular && arg3Val.isRegular && arg4Val.isRegular && arg5Val.isRegular && resultVal.isRegular) {
          // The argument values should have been placed in applyMap at the time the values of this apply were computed.
          // By using applyMap, we can make sure that any contained elements in the result of the apply are the same now as they were when values were computed.
          if (resultVal.value == mapper.map(applyMap((arg1Val.value, arg2Val.value, arg3Val.value, arg4Val.value, arg5Val.value)), applyValues.regularValues)) 1.0
          else 0.0
        } else if ((!arg1Val.isRegular || !arg2Val.isRegular || !arg3Val.isRegular || !arg4Val.isRegular || !arg5Val.isRegular) && !resultVal.isRegular) 1.0
        else if ((!arg1Val.isRegular || !arg2Val.isRegular || !arg3Val.isRegular || !arg4Val.isRegular || !arg5Val.isRegular) && resultVal.isRegular) 0.0
        else 0.0
      factor.set(List(arg1Index, arg2Index, arg3Index, arg4Index, arg5Index, resultIndex), entry)
    }
    List(factor)
  }

  private def makeFactors[T](inject: Inject[T]): List[Factor[Double]] = {
    def rule(values: List[Extended[_]]) = {
      val resultXvalue :: inputXvalues = values
      // See logic under makeCares
      if (inputXvalues.exists(!_.isRegular)) {
        if (!resultXvalue.isRegular) 1.0; else 0.0
      }
      else if (resultXvalue.isRegular) {
        if (resultXvalue.value.asInstanceOf[List[T]] == inputXvalues.map(_.value.asInstanceOf[T])) 1.0; else 0.0
      } else 0.0
    }
    val inputVariables = inject.args map (Variable(_))
    val resultVariable = Variable(inject)
    val variables = resultVariable :: inputVariables
    val factor = new BasicFactor[Double](variables)
    factor.fillByRule(rule _)
    List(factor)
  }

  // When we're using a parameter to compute expected sufficient statistics, we just use its expected value
  private def makeFactors(param: Parameter[_]): List[Factor[Double]] = {
    // The parameter should have one possible value, which is its expected value
    assert(Variable(param).range.size == 1)
    val factor = new BasicFactor[Double](List(Variable(param)))
    factor.set(List(0), 1.0)
    List(factor)
  }

  private def makeFactors(flip: ParameterizedFlip): List[Factor[Double]] = {
    val flipVar = Variable(flip)
    val factor = new BasicFactor[Double](List(flipVar))
    val prob = flip.parameter.MAPValue
    val i = flipVar.range.indexOf(Regular(true))
    factor.set(List(i), prob)
    factor.set(List(1 - i), 1.0 - prob)
    List(factor)
  }

  def concreteFactors[T](elem: Element[T]): List[Factor[Double]] =
    elem match {
      case f: ParameterizedFlip => makeFactors(f)
      case s: ParameterizedSelect[_] => makeFactors(s)
      case p: Parameter[_] => makeFactors(p)
      case c: Constant[_] => makeFactors(c)
      case f: AtomicFlip => makeFactors(f)
      case f: CompoundFlip => makeFactors(f)
      case s: AtomicSelect[_] => makeFactors(s)
      case s: CompoundSelect[_] => makeFactors(s)
      case d: AtomicDist[_] => makeFactors(d)
      case d: CompoundDist[_] => makeFactors(d)
      case c: Chain[_, _] => makeFactors(c)
      case a: Apply1[_, _] => makeFactors(a)
      case a: Apply2[_, _, _] => makeFactors(a)
      case a: Apply3[_, _, _, _] => makeFactors(a)
      case a: Apply4[_, _, _, _, _] => makeFactors(a)
      case a: Apply5[_, _, _, _, _, _] => makeFactors(a)
      case i: Inject[_] => makeFactors(i)
      case f: ProbFactorMaker => f.makeFactors

      case _ => throw new UnsupportedAlgorithmException(elem)
    }

  private def makeAbstract[T](atomic: Atomic[T], abstraction: Abstraction[T]): List[Factor[Double]] = {
    val variable = Variable(atomic)
    val values = variable.range.map(_.value)
    val densityMap = scala.collection.mutable.Map[T, Double]()
    for { v <- values } {
      val currentDensity = densityMap.getOrElse(v, 0.0)
      densityMap.update(v, currentDensity + atomic.density(v))
    }
    val factor = new BasicFactor[Double](List(variable))
    for { (v, i) <- values.zipWithIndex } {
      factor.set(List(i), densityMap(v))
    }
    List(factor)
  }

  private def makeAbstract[T](elem: Element[T], abstraction: Abstraction[T]): List[Factor[Double]] =
    elem match {
      case atomic: Atomic[_] => makeAbstract(atomic, abstraction)
      case apply: Apply1[_, _] => makeFactors(apply)(abstraction.scheme)
      case apply: Apply2[_, _, _] => makeFactors(apply)(abstraction.scheme)
      case apply: Apply3[_, _, _, _] => makeFactors(apply)(abstraction.scheme)
      // In the case of a Chain, its pragmas are inherited by the expanded result elements. The abstraction will be
      // taken into account when we generate factors for the result elements.
      case chain: Chain[_, _] => makeFactors(chain)(abstraction.scheme)
      case _ => throw new UnsupportedAlgorithmException(elem)
    }

  private def makeNonConstraintFactorsUncached[T](elem: Element[T]): List[Factor[Double]] = {
    /*
     * Don't make non-constraint factors for an element that is expanded to depth -1.
     * The element must take on the value *, so the factor is the unit.
     * Attempting to create a factor can result in problems where the element's arguments are not star, 
     * leading to the factor being zero everywhere.
     * For example, consider an Apply. If the Apply is expanded to depth -1, but its argument has already
     * been expanded and produced a set of values without *, the Apply factor would have probability zero
     * for cases where the Apply result is *. But since the Apply has only been expanded to depth -1,
     * its only possible result is *, so the factor is zero everywhere.
     */
    if (LazyValues(elem.universe).expandedDepth(elem).getOrElse(-1) != -1) {
      Abstraction.fromPragmas(elem.pragmas) match {
        case None => concreteFactors(elem)
        case Some(abstraction) => makeAbstract(elem, abstraction)
      }
    }
    else List()
  }

  private def makeConditionAndConstraintFactors[T](elem: Element[T]): List[Factor[Double]] =
    elem.allConditions.map(makeConditionFactor(elem, _)) ::: elem.allConstraints.map(makeConstraintFactor(elem, _))

  private def makeConditionFactor[T](elem: Element[T], cc: (T => Boolean, Element.Contingency)): Factor[Double] =
    makeConstraintFactor(elem, (ProbConstraintType((t: T) => if (cc._1(t)) 1.0; else 0.0), cc._2))

  private def makeConstraintFactor[T](elem: Element[T], cc: (T => Double, Element.Contingency)): Factor[Double] = {
    val (constraint, contingency) = cc
    contingency match {
      case List() => makeUncontingentConstraintFactor(elem, constraint)
      case first :: rest => makeContingentConstraintFactor(elem, constraint, first, rest)
    }
  }

  private def makeUncontingentConstraintFactor[T](elem: Element[T], constraint: T => Double): Factor[Double] = {    
    val elemVar = Variable(elem)
    val factor = new BasicFactor[Double](List(elemVar))
    for { (elemVal, index) <- elemVar.range.zipWithIndex } {
      val entry = if (elemVal.isRegular) math.exp(constraint(elemVal.value)); else 0.0
      factor.set(List(index), entry)
    }
    factor
  }

  private def makeContingentConstraintFactor[T](elem: Element[T], constraint: T => Double, firstConting: Element.ElemVal[_], restContinges: Element.Contingency): Factor[Double] = {
    val restFactor = makeConstraintFactor(elem, (constraint, restContinges))
    extendConstraintFactor(restFactor, firstConting)
  }

  private def extendConstraintFactor(restFactor: Factor[Double], firstConting: Element.ElemVal[_]): Factor[Double] = {
    // The extended factor is obtained by getting the underlying factor and expanding each row so that the row only provides its entry if the contingent variable takes
    // on the appropriate value, otherwise the entry is 1
    val Element.ElemVal(firstElem, firstValue) = firstConting
    val firstVar = Variable(firstElem)
    val firstValues = firstVar.range
    val numFirstValues = firstValues.size
    val matchingIndex: Int = firstValues.indexOf(Regular(firstValue))
    val resultFactor = new BasicFactor[Double](firstVar :: restFactor.variables)
    for { restIndices <- restFactor.allIndices } {
      val restEntry = restFactor.get(restIndices)
      for { firstIndex <- 0 until numFirstValues } {
        val resultEntry = if (firstIndex == matchingIndex) restEntry; else 1.0
        resultFactor.set(firstIndex :: restIndices, resultEntry)
      }
    }
    resultFactor
  }

  private val factorCache = scala.collection.mutable.Map[Element[_], List[Factor[Double]]]()

  def makeNonConstraintFactors(elem: Element[_]): List[Factor[Double]] = {
    factorCache.get(elem) match {
      case Some(l) => l
      case None =>
        val result = makeNonConstraintFactorsUncached(elem)
        factorCache += elem -> result
        elem.universe.register(factorCache)
        result
    }
  }
  
  
  /**
   * Create the probabilistic factors associated with an element. This method is memoized.
   */
  def make(elem: Element[_]): List[Factor[Double]] = {
    makeConditionAndConstraintFactors(elem) ::: makeNonConstraintFactors(elem)
  }
  
  /**
   * Create the probabilistic factors associated with a list.
   */
  def make[T](variables: List[Variable[_]]): Factor[T] = {
    new BasicFactor[T](variables)
  }
  
  /**
   * Remove an element from the factor cache, ensuring that factors for the element
   * are regenerated. This is important, for example,  if evidence on the variable has changed. 
   * 
   */
  def removeFactors(elem: Element[_]) { factorCache -= elem }
  
  /**
   * Clear the factor cache.
   */
  def removeFactors() { factorCache.clear }

  /**
   * Create the probabilistic factor encoding the probability of evidence in the dependent universe as a function of the
   * values of variables in the parent universe. The third argument is the the function to use for computing
   * probability of evidence in the dependent universe. It is assumed that the definition of this function will already contain the
   * right evidence.
   */
  def makeDependentFactor(parentUniverse: Universe,
    dependentUniverse: Universe,
    probEvidenceComputer: () => Double): Factor[Double] = {
    val uses = dependentUniverse.parentElements filter (_.universe == parentUniverse)
    def rule(values: List[Any]) = {
      for { (elem, value) <- uses zip values } { elem.value = value.asInstanceOf[Regular[elem.Value]].value }
      val result = probEvidenceComputer()
      result
    }
    val variables = uses map (Variable(_))
    val factor = new BasicFactor[Double](variables)
    factor.fillByRule(rule _)
    factor
  }
}