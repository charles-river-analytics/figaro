/*
 * Factory.scala
 * Methods to create factors over variables.
 *
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Jan 1, 2009
 *
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.algorithm.factored.factors.factory

import com.cra.figaro.algorithm._
import com.cra.figaro.language._
import com.cra.figaro.util._
import com.cra.figaro.algorithm.lazyfactored._
import com.cra.figaro.algorithm.lazyfactored.ValueSet._
import com.cra.figaro.algorithm.factored._
import com.cra.figaro.algorithm.factored.factors._
import com.cra.figaro.library.compound._
import com.cra.figaro.library.collection._
import com.cra.figaro.library.atomic.discrete._
import com.cra.figaro.algorithm.structured._
import scala.reflect.runtime.universe.{typeTag, TypeTag}


/**
 * Methods for creating probabilistic factors associated with elements.
 */
object Factory {
  /**
   * Get the variable associated with the given element in the given component collection.
   * If the element does not exist in the component collection, an intermediate variable is created whose range is { Star }.
   */
  def getVariable[T](cc: ComponentCollection, element: Element[T]): Variable[T] = {
    if (cc.contains(element)) {
      val component = cc(element)
      component.variable
    } else {
      makeVariable(cc, withStar(Set[T]()))
    }
  }

  /**
   * Create an intermediate variable in the given component collection with the given value set.
   */
  def makeVariable[T](cc: ComponentCollection, valueSet: ValueSet[T]): Variable[T] = {
    val v = new Variable(valueSet)
    cc.intermediates += v
    v
  }

  def makeVariable[T](cc: ComponentCollection, valueSet: ValueSet[T], chain: Chain[_, _]): Variable[T] = {
    val v = new InternalChainVariable(valueSet, chain, getVariable(cc, chain))
    cc.intermediates += v
    v
  }

  /**
   * Create a new factor in which one variable is replaced with another.
   */
  def replaceVariable[T](factor: Factor[Double], oldVariable: Variable[T], newVariable: Variable[T]): Factor[Double] = {
    if (oldVariable.range != newVariable.range) throw new IllegalArgumentException("Replacing variable with variable with different range")
    if (!factor.variables.contains(oldVariable)) factor
    else {
      val newVariables = factor.variables.map(v => if (v == oldVariable) newVariable else v)
      val newFactor =
        factor match {
          case s: SparseFactor[Double] => new SparseFactor[Double](newVariables, List())
          case b: BasicFactor[Double] => new BasicFactor[Double](newVariables, List())
        }
      for { indices <- factor.getIndices } { newFactor.set(indices, factor.get(indices)) }
      newFactor
    }
  }

  /**
   * The mutliplicative identity factor.
   */
  def unit[T: TypeTag](semiring: Semiring[T]): Factor[T] = {
    val result = new BasicFactor[T](List(), List(), semiring)
    result.set(List(), semiring.one)
    result
  }

  /**
   * Given a sequence of variables, create a new variable representing the tuple of the inputs
   * and create the factor mapping the inputs to their tuple.
   * @param inputs the variables to be formed into a tuple
   */
  def makeTupleVarAndFactor(cc: ComponentCollection, chain: Option[Chain[_, _]], inputs: Variable[_]*): (Variable[List[Extended[_]]], Factor[Double]) = {
    val inputList: List[Variable[_]] = inputs.toList
    // Subtlety alert: In the tuple, we can't just map inputs with * to *. We need to remember which input was *.
    // Therefore, instead, we make the value a regular value consisting of a list of extended values.
    val tupleRangeRegular: List[List[_]] = cartesianProduct(inputList.map(_.range): _*)
    val tupleVS: ValueSet[List[Extended[_]]] = withoutStar(tupleRangeRegular.map(_.asInstanceOf[List[Extended[_]]]).toSet)
    val tupleVar: Variable[List[Extended[_]]] = if (chain.nonEmpty) Factory.makeVariable(cc, tupleVS, chain.get) else Factory.makeVariable(cc, tupleVS)
    val tupleFactor = new SparseFactor[Double](inputList, List(tupleVar))
    for { pair <- tupleVar.range.zipWithIndex } {
      val tupleVal: List[Extended[_]] = pair._1.value
      val tupleIndex = pair._2
      val inputIndices =
        for { (input, value) <- inputList.zip(tupleVal) } yield input.range.indexOf(value)
      tupleFactor.set(inputIndices ::: List(tupleIndex), 1.0)
    }
    (tupleVar, tupleFactor)
  }

  /**
   * Create a BasicFactor from the supplied parent and children variables
   */
  def defaultFactor[T: TypeTag](parents: List[Variable[_]], children: List[Variable[_]], _semiring: Semiring[T] = SumProductSemiring().asInstanceOf[Semiring[T]]) =
    new BasicFactor[T](parents, children, _semiring)

  private def makeFactors[T](cc: ComponentCollection, const: Constant[T]): List[Factor[Double]] = {
    val factor = new BasicFactor[Double](List(), List(getVariable(cc, const)))
    factor.set(List(0), 1.0)
    List(factor)
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

  def makeConditionalSelector[T, U](pairVar: Variable[List[Extended[_]]], parentXVal: Extended[T], outcomeVar: Variable[U], choices: Set[U])(implicit mapper: PointMapper[U]): Factor[Double] = {
    val factor = new ConditionalSelector[Double](List(pairVar), List(outcomeVar))
    for {
      (pairXVal, pairIndex) <- pairVar.range.zipWithIndex
      (outcomeXVal, outcomeIndex) <- outcomeVar.range.zipWithIndex
    } {
      // See makeTupleVarAndFactor: pairXVal is always regular and consists of a list of two elements: the extended parent value
      // and the extended outcome value.
      val List(selectXVal, overallXVal) = pairXVal.value
      val entry =
        if (selectXVal.isRegular && parentXVal.isRegular) {
          if (selectXVal.value == parentXVal.value) {
            if ((!overallXVal.isRegular && !outcomeXVal.isRegular) || (overallXVal.isRegular && outcomeXVal.isRegular && overallXVal.value == mapper.map(outcomeXVal.value, choices))) 1.0 else 0.0
          } else 1.0
        } else if (selectXVal.isRegular || parentXVal.isRegular) 1.0 // they are different
        else if (!overallXVal.isRegular) 1.0 // if parentXVal is *, the only possible outcomeXVal is *
        else 0.0
      factor.set(List(pairIndex, outcomeIndex), entry)
    }
    factor
  }

  private def isTemporary[_T](variable: Variable[_]): Boolean = {
    variable match {
      case e: ElementVariable[_] => e.element.isTemporary
      case _ => false
    }
  }

  private def makeFactors[T](cc: ComponentCollection, inject: Inject[T]): List[Factor[Double]] = {
    def rule(values: List[Extended[_]]) = {
      val inputXvalues :+ resultXvalue = values
      // See logic under makeCares
      if (inputXvalues.exists(!_.isRegular)) {
        if (!resultXvalue.isRegular) 1.0; else 0.0
      } else if (resultXvalue.isRegular) {
        if (resultXvalue.value.asInstanceOf[List[T]] == inputXvalues.map(_.value.asInstanceOf[T])) 1.0; else 0.0
      } else 0.0
    }
    val inputVariables = inject.args map (getVariable(cc, _))
    val resultVariable = getVariable(cc, inject)
    //    val variables = resultVariable :: inputVariables
    val factor = new BasicFactor[Double](inputVariables, List(resultVariable))
    factor.fillByRule(rule _)
    List(factor)
  }

  // When we're using a parameter to compute expected sufficient statistics, we just use its expected value
  private def makeParameterFactors(cc: ComponentCollection, param: Parameter[_]): List[Factor[Double]] = {
    // The parameter should have one possible value, which is its expected value
    val paramVar = getVariable(cc, param)
    assert(paramVar.range.size == 1)
    val factor = new BasicFactor[Double](List(), List(paramVar))
    factor.set(List(0), 1.0)
    List(factor)
  }

  private def makeFactors[T](cc: ComponentCollection, atomic: Atomic[T]): List[Factor[Double]] = {
    val atomicVar = getVariable(cc, atomic)
    val pbpSampler = ParticleGenerator(atomic.universe)
    // Note, don't need number of samples because values should have already been expanded on it
    // (and values will initiate the sampling)
    val samples = pbpSampler(atomic)
    if (atomicVar.range.exists(!_.isRegular)) {
      assert(atomicVar.range.size == 1) // Select's range must either be a list of regular values or {*}
      StarFactory.makeStarFactor(cc, atomic)
    } else {
      val probs = SelectFactory.getProbs(cc, atomic, samples)
      List(SelectFactory.makeSimpleDistribution(atomicVar, probs))
    }
  }

  /**
   * Invokes Factor constructors for a standard set of Elements. This method uses various
   * secondary factories.
   */
  def concreteFactors[T](cc: ComponentCollection, elem: Element[T], parameterized: Boolean): List[Factor[Double]] = {
    elem match {
      case flip: ParameterizedFlip => DistributionFactory.makeFactors(cc, flip, parameterized)
      case pSelect: ParameterizedSelect[_] => SelectFactory.makeFactors(cc, pSelect, parameterized)
      case pBin: ParameterizedBinomialFixedNumTrials => DistributionFactory.makeFactors(cc, pBin, parameterized)
      case parameter: DoubleParameter => makeParameterFactors(cc, parameter)
      case array: ArrayParameter => makeParameterFactors(cc, array)
      case constant: Constant[_] => makeFactors(cc, constant)
      case f: AtomicFlip => DistributionFactory.makeFactors(cc, f)
      case f: CompoundFlip => DistributionFactory.makeFactors(cc, f)
      case ab: AtomicBinomial => DistributionFactory.makeFactors(cc, ab)
      case s: AtomicSelect[_] => SelectFactory.makeFactors(cc, s)
      case s: CompoundSelect[_] => SelectFactory.makeFactors(cc, s)
      case d: AtomicDist[_] => SelectFactory.makeFactors(cc, d)
      case d: CompoundDist[_] => SelectFactory.makeFactors(cc, d)
      case s: IntSelector => SelectFactory.makeFactors(cc, s)
      case c: Chain[_, _] => ChainFactory.makeFactors(cc, c)
      case a: Apply1[_, _] => ApplyFactory.makeFactors(cc, a)
      case a: Apply2[_, _, _] => ApplyFactory.makeFactors(cc, a)
      case a: Apply3[_, _, _, _] => ApplyFactory.makeFactors(cc, a)
      case a: Apply4[_, _, _, _, _] => ApplyFactory.makeFactors(cc, a)
      case a: Apply5[_, _, _, _, _, _] => ApplyFactory.makeFactors(cc, a)
      case i: Inject[_] => makeFactors(cc, i)
      case r: SingleValuedReferenceElement[_] => ComplexFactory.makeFactors(cc, r)
      case r: MultiValuedReferenceElement[_] => ComplexFactory.makeFactors(cc, r)
      case r: Aggregate[_, _] => ComplexFactory.makeFactors(cc, r)
      //case m: MakeList[_] => ComplexFactory.makeFactors(cc, m)
      case m: MakeArray[_] => ComplexFactory.makeFactors(cc, m)
      case f: FoldLeft[_, _] => ComplexFactory.makeFactors(cc, f)
      case f: FactorMaker[_] => f.makeFactors
      case a: Atomic[_] => makeFactors(cc, a)

      case _ => throw new UnsupportedAlgorithmException(elem)
    }
  }

  private def makeAbstract[T](cc: ComponentCollection, atomic: Atomic[T], abstraction: Abstraction[T]): List[Factor[Double]] = {
    val variable = getVariable(cc, atomic)
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

  private def makeAbstract[T](cc: ComponentCollection, elem: Element[T], abstraction: Abstraction[T]): List[Factor[Double]] =
    elem match {
      case apply: Apply1[_, _] => ApplyFactory.makeFactors(cc, apply)(abstraction.scheme)
      case apply: Apply2[_, _, _] => ApplyFactory.makeFactors(cc, apply)(abstraction.scheme)
      case apply: Apply3[_, _, _, _] => ApplyFactory.makeFactors(cc, apply)(abstraction.scheme)
      // In the case of a Chain, its pragmas are inherited by the expanded result elements. The abstraction will be
      // taken into account when we generate factors for the result elements.
      case chain: Chain[_, _] => ChainFactory.makeFactors(cc, chain)(abstraction.scheme)
      case atomic: Atomic[_] => makeAbstract(cc, atomic, abstraction)
      case _ => throw new UnsupportedAlgorithmException(elem)
    }

  /**
   * Make the non-constraint factors corresponding to the given element in the component collection.
   *
   * @param parameterized If true, parameterized elements are assumed to be equal to their previously computed MAP value. If false, they are treated like any other element.
   */
  def makeFactors[T](cc: ComponentCollection, elem: Element[T], parameterized: Boolean): List[Factor[Double]] = {
    val component = cc(elem)
    if (component.range.hasStar && component.range.regularValues.isEmpty) List()
    else {
      Abstraction.fromPragmas(elem.pragmas) match {
        case None => concreteFactors(cc, elem, parameterized)
        case Some(abstraction) => makeAbstract(cc, elem, abstraction)
      }
    }
  }

  /**
   * Create the probabilistic factor encoding the probability of evidence in the dependent universe as a function of the
   * values of variables in the parent universe. The third argument is the the function to use for computing
   * probability of evidence in the dependent universe. It is assumed that the definition of this function will already contain the
   * right evidence.
   */
  def makeDependentFactor(cc: ComponentCollection, parentUniverse: Universe,
    dependentUniverse: Universe,
    probEvidenceComputer: () => Double): Factor[Double] = {
    val uses = dependentUniverse.parentElements filter (_.universe == parentUniverse)
    def rule(values: List[Any]) = {
      for { (elem, value) <- uses zip values } { elem.value = value.asInstanceOf[Regular[elem.Value]].value }
      val result = probEvidenceComputer()
      result
    }
    val variables = uses map (cc(_).variable)
    val factor = new BasicFactor[Double](variables, List())
    factor.fillByRule(rule _)
    factor
  }

  /**
   * Make factors for a particular element. This function wraps the SFI method of creating factors using component collections
   */
  def makeFactorsForElement[Value](elem: Element[_], upper: Boolean = false, parameterized: Boolean = false) = {
    Variable(elem)
    val comp = Variable.cc(elem)
    elem match {
      // If the element is a chain, we need to create subproblems for each value of the chain
      // to create factors accordingly
      case c: Chain[_, _] => {
        val chainMap = LazyValues(elem.universe).getMap(c)
        chainMap.foreach(f => {
          val subproblem = new NestedProblem(Variable.cc, f._2)
          Variable.cc.expansions += (c.chainFunction, f._1) -> subproblem
        })
      }
      // If the element is a MakeArray, we need mark that it has been expanded. Note that
      // the normal Values call will expand the MakeArray, we are just setting the max expansion here
      case ma: MakeArray[_] => {
        val maC = Variable.cc(ma)
        maC.maxExpanded = Variable.cc(ma.numItems).range.regularValues.max
      }
      // If the element is an apply, we need to populate the Apply map used by the factor creation
      case a: Apply[Value] => {
        val applyComp = comp.asInstanceOf[ApplyComponent[Value]]
        val applyMap = LazyValues(elem.universe).getMap(a)
        applyComp.setMap(applyMap)
      }
      case _ => ()
    }
    // Make the constraint and non-constraint factors for the element by calling the
    // component factor makers    
    val constraint = if (upper) {
      comp.makeConstraintFactors(Upper)
      comp.constraintFactors(Upper)
    } else {
      comp.makeConstraintFactors(Lower)
      comp.constraintFactors(Lower)
    }
    comp.makeNonConstraintFactors(parameterized)
    constraint ::: comp.nonConstraintFactors
  }
}
