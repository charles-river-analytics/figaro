/*
 * Factory.scala
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

package com.cra.figaro.algorithm.factored.factors

import com.cra.figaro.algorithm._
import com.cra.figaro.language._
import com.cra.figaro.util._
import com.cra.figaro.algorithm.lazyfactored._
import com.cra.figaro.algorithm.factored._
import scala.collection.mutable.ListBuffer
import com.cra.figaro.algorithm.factored.factors.factory._
import com.cra.figaro.library.compound._
import com.cra.figaro.library.collection._
import com.cra.figaro.library.atomic.discrete._

/**
 * A trait for elements that are able to construct their own Factor.
 */
trait FactorMaker[T] {
  def makeFactors[T]: List[Factor[Double]]
}

/**
 * Methods for creating probabilistic factors associated with elements.
 */
object Factory {

  /**
   * The mutliplicative identity factor.
   */
  def unit[T](semiring: Semiring[T]): Factor[T] = {
    val result = new BasicFactor[T](List(), List())
    result.set(List(), semiring.one)
    result
  }

  /**
   * Create a BasicFactor from the supplied parent and children variables
   */
  def defaultFactor[T](parents: List[Variable[_]], children: List[Variable[_]]) = new BasicFactor[T](parents, children)

  private def makeFactors[T](const: Constant[T]): List[Factor[Double]] = {
    val factor = new BasicFactor[Double](List(), List(Variable(const)))
    factor.set(List(0), 1.0)
    List(factor)
  }

  private def makeDontCares[U](factor: Factor[Double],
    intermedIndex: Int,
    outcomeVar: Variable[U],
    overallVar: Variable[U]
    ): Unit = {
    // If we don't care, we assign 1.0 to all combinations of the distVar and outcomeVar
    for {
      j <- 0 until outcomeVar.size
      k <- 0 until overallVar.size
    } {
      factor.set(List(intermedIndex, j, k), 1.0)
    }
  }

  private def makeCares[U](factor: Factor[Double], intermedIndex: Int,
    outcomeVar: Variable[U], overallVar: Variable[U], choices: Set[U])(implicit mapper: PointMapper[U]): Unit = {
    // We care to match up overallVar with outcomeVar
    for {
      (outcomeVal, j) <- outcomeVar.range.zipWithIndex
      (overallVal, k) <- overallVar.range.zipWithIndex
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
    val factor = new BasicFactor[Double](List(selector, outcomeVar), List(overallVar))
    for { i <- 0 until selector.size} {
      if (i == outcomeIndex) {
        makeCares(factor, outcomeIndex, outcomeVar, overallVar, overallValues.regularValues)(mapper)
      }
      else {
        makeDontCares(factor, i, outcomeVar, overallVar)
      }
    }

    factor
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
        val dummyFactor = new BasicFactor[Double](List(), List(dummy))
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

  /**
   * Combines a set of factors into a single larger factor. This method is used when a factor has
   * been decomposed into may dependent Factors and a single Factor is required.
   */
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
       var resultFactor = unit[Double](semiring).product(factor, semiring)

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

  private def makeFactors[T](inject: Inject[T]): List[Factor[Double]] = {
    def rule(values: List[Extended[_]]) = {
      val inputXvalues :+ resultXvalue = values
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
//    val variables = resultVariable :: inputVariables
    val factor = new BasicFactor[Double](inputVariables, List(resultVariable))
    factor.fillByRule(rule _)
    List(factor)
  }

  // When we're using a parameter to compute expected sufficient statistics, we just use its expected value
  private def makeParameterFactors(param: Parameter[_]): List[Factor[Double]] = {
    // The parameter should have one possible value, which is its expected value
    assert(Variable(param).range.size == 1)
    val factor = new BasicFactor[Double](List(), List(Variable(param)))
    factor.set(List(0), 1.0)
    List(factor)
  }

  private def makeFactors[T](atomic: Atomic[T]): List[Factor[Double]] = {
    val atomicVar = Variable(atomic)
    val pbpSampler = ParticleGenerator(atomic.universe)
    // Note, don't need number of samples because values should have already been expanded on it
    // (and values will initiate the sampling)
    val samples = pbpSampler(atomic)
    if (atomicVar.range.exists(!_.isRegular)) {
      assert(atomicVar.range.size == 1) // Select's range must either be a list of regular values or {*}
      StarFactory.makeStarFactor(atomic)
    } else {
      val probs = SelectFactory.getProbs(atomic, samples)
      List(SelectFactory.makeSimpleDistribution(atomicVar, probs))
    }
  }

  /**
   * Invokes Factor constructors for a standard set of Elements. This method uses various
   * secondary factories.
   */
  def concreteFactors[T](elem: Element[T]): List[Factor[Double]] =
    elem match {
      case flip: ParameterizedFlip => DistributionFactory.makeFactors(flip)
      case pSelect: ParameterizedSelect[_] => SelectFactory.makeFactors(pSelect)
      case parameter: DoubleParameter => makeParameterFactors(parameter)
      case array: ArrayParameter => makeParameterFactors(array)
      case constant: Constant[_] => makeFactors(constant)
      case f: AtomicFlip => DistributionFactory.makeFactors(f)
      case f: CompoundFlip => DistributionFactory.makeFactors(f)
      case ab: AtomicBinomial => DistributionFactory.makeFactors(ab)
      case s: AtomicSelect[_] => SelectFactory.makeFactors(s)
      case s: CompoundSelect[_] => SelectFactory.makeFactors(s)
      case d: AtomicDist[_] => SelectFactory.makeFactors(d)
      case d: CompoundDist[_] => SelectFactory.makeFactors(d)
      case s: IntSelector => SelectFactory.makeFactors(s)
      case c: Chain[_, _] => makeFactors(c)
      case a: Apply1[_, _] => ApplyFactory.makeFactors(a)
      case a: Apply2[_, _, _] => ApplyFactory.makeFactors(a)
      case a: Apply3[_, _, _, _] => ApplyFactory.makeFactors(a)
      case a: Apply4[_, _, _, _, _] => ApplyFactory.makeFactors(a)
      case a: Apply5[_, _, _, _, _, _] => ApplyFactory.makeFactors(a)
      case i: Inject[_] => makeFactors(i)
      case r: SingleValuedReferenceElement[_] => ComplexFactory.makeFactors(r)
      case r: MultiValuedReferenceElement[_] => ComplexFactory.makeFactors(r)
      case r: Aggregate[_, _] => ComplexFactory.makeFactors(r)
      case m: MakeList[_] => ComplexFactory.makeFactors(m)
      case m: MakeArray[_] => ComplexFactory.makeFactors(m)
      case f: FoldLeft[_,_] => ComplexFactory.makeFactors(f)
      case f: FactorMaker[_] => f.makeFactors
      case a: Atomic[_] => makeFactors(a)

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
    val factor = new BasicFactor[Double](List(), List(variable))
    for { (v, i) <- values.zipWithIndex } {
      factor.set(List(i), densityMap(v))
    }
    List(factor)
  }

  private def makeAbstract[T](elem: Element[T], abstraction: Abstraction[T]): List[Factor[Double]] =
    elem match {
      case apply: Apply1[_, _] => ApplyFactory.makeFactors(apply)(abstraction.scheme)
      case apply: Apply2[_, _, _] => ApplyFactory.makeFactors(apply)(abstraction.scheme)
      case apply: Apply3[_, _, _, _] => ApplyFactory.makeFactors(apply)(abstraction.scheme)
      // In the case of a Chain, its pragmas are inherited by the expanded result elements. The abstraction will be
      // taken into account when we generate factors for the result elements.
      case chain: Chain[_, _] => makeFactors(chain)(abstraction.scheme)
      case atomic: Atomic[_] => makeAbstract(atomic, abstraction)
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
      case List() => makeUncontingentConstraintFactor(elem, constraint, false)
      case first :: rest => makeContingentConstraintFactor(elem, constraint, first, rest)
    }
  }

  private def makeUncontingentConstraintFactor[T](elem: Element[T], constraint: T => Double, upper: Boolean): Factor[Double] = {
    val elemVar = Variable(elem)
    val factor = new BasicFactor[Double](List(), List(elemVar))
    for { (elemVal, index) <- elemVar.range.zipWithIndex } {
      val entry = if (elemVal.isRegular) {
        math.exp(constraint(elemVal.value))
      } else if (upper) {
        1.0
      } else {
        0.0
      }
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
    val resultFactor = new BasicFactor[Double](firstVar :: restFactor.parents, restFactor.output)
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

  /**
   * Construct a Factor without constraints.
   */
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
   * Creates a BasicFactor from the supplied variables
   */
  def simpleMake[T](variables: List[Variable[_]]) =
    new BasicFactor[T](variables, List())

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
   * Update the factor cache.
   */
  def updateFactor[T](elem: Element[_], f: List[Factor[Double]]) { factorCache.update(elem, f) }

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
    val factor = new BasicFactor[Double](variables, List())
    factor.fillByRule(rule _)
    factor
  }
}
