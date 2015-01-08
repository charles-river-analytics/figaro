/*
 * Element.scala
 * Elements of Figaro models.
 *
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Jan 1, 2009
 *
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.language

import com.cra.figaro.library.compound._
import scala.collection.mutable.Set
import scala.language.implicitConversions

/**
 * An Element is the core component of a probabilistic model. Elements can be understood as
 * defining a probabilistic process. Elements are parameterized by the type of Value the process
 * produces.
 *
 * Each Element is a mix of a random component and a deterministic component. The random component
 * has type Randomness. The generateRandomness method generates the Randomness according to a
 * probability distribution. The generateValue method is a deterministic function that generates the
 * output Value of the Element from the Randomness. Thus, Elements can be understood as defining a
 * generative process in which first the Randomness is generated and then the output Value is
 * generated given the Randomness.
 *
 * Elements also have a current outcome, represented by the value field. Naturally, the
 * generateValue function can refer to the current value of related Elements. However, generateValue
 * is not allowed to call generateValue on another Element. We use the notation generateValue(r | w)
 * to denote the value it produces given randomness r when the current value of related Elements is
 * w.
 *
 * Elements can have hard conditions and soft constraints. A condition is a predicate on Values that
 * must be satisfied by the output of the Element. Values that violate the condition have
 * probability zero. A constraint is a function that maps Values to Doubles. The probability of a
 * Value is multiplied by the constraint, and then normalized.
 * Conditions and constraints can be contingent on other elements taking on particular values.
 * Ordinarily, these contingencies will not be specified by the user, but automatically by other Figaro code.
 * In particular, specifying named evidence on a reference can result in contingencies.
 *
 * Thus, an Element represents a conditional probability distribution over Values given the current
 * values w of related Elements. The probability of an outcome v is defined by:
 *
 *   P(v | w) is proportional to (\sum_{r: generateValue(r | w) = v} P(generateRandomness() = r)
 *                                                       * constraint(v))             if condition(v);
 *                    0                                                              otherwise
 *
 * An element has a name and belongs to an element collection that is used to find the element the name.
 *
 * Elements can be cacheable or non-cacheable, which determines what type of Chain will be created for them.
 * If you create a new Element class that you want to be cached, you should declare it to implement the Cacheable or IfArgsCacheable traits.
 *
 * @param name The name of the element
 * @param collection The element collection to which this element belongs
 */

abstract class Element[T](val name: Name[T], val collection: ElementCollection) {
  /**
   * The type of values over which the element is defined.
   */
  type Value = T

  /**
   * The type of conditions on the element. A condition is a function from a value to a Boolean.
   */
  type Condition = T => Boolean

  /**
   * The type of soft constraints on the element. A constraint is a function from a value to a Double.
   */
  type Constraint = T => Double

  /**
   * The type of randomness content of the element.
   */
  type Randomness

  /**
   * The universe in which the element is defined.
   */
  val universe = collection.universe

  override val hashCode = com.cra.figaro.util.getNextHashCode //We want this to only be called once.

  /**
   * The cacheability of the element. Chains create caches of their parent values, and it is useful to know when these values can be effectively cached and reused.
   *  In general, continuous distributions are not cacheable.
   */
  def isCachable(): Boolean = false

  /**
   * Generate the randomness content.
   */
  def generateRandomness(): Randomness

  /**
   * Generate the next randomness given the current randomness.
   * Returns three values: The next randomness, the Metropolis-Hastings proposal probability
   * ratio, which is:
   *
   * P(new -> old) / P(old -> new)
   *
   * and the model probability ratio, which is:
   *
   * P(new) / P(old)
   *
   * The default implementation is to use generateRandomness and returns ones for the
   * proposal and model probability ratios.
   *
   */
  def nextRandomness(rand: Randomness): (Randomness, Double, Double) = (generateRandomness(), 1.0, 1.0)

  /**
   * The current randomness content of the element.
   */
  var randomness: Randomness = _

  /**
   * Generate the value of the element deterministically given its randomness and the values of
   * its arguments.
   */
  def generateValue(rand: Randomness): Value

  /**
   * The current value of the element.
   */
  var value: Value = _

  /**
   * First generate the randomness, then generate the value given the randomness. Store the results
   * in randomness and value.
   */
  final def generate(): Unit = {
    if (!setFlag) { // Make sure we do not generate this element if we have already set its value
      args.foreach(arg => if (arg.value == null) arg.generate()) // make sure arguments have a valid value
      randomness = generateRandomness()
      value = generateValue(randomness)
    }
  }

  /* Complete context of this element */
  private[language] var myContext: List[Element[_]] = List()

  /** The elements on which the existence of this element depends. */
  def context = if (!active) {
    throw new NoSuchElementException
  } else myContext

  /* Stores the elements that were created in this element's context. Note this is not used
   * for chains, since they maintain their own context control.
   */
  private val myDirectContextContents: Set[Element[_]] = Set()

  /**
   * Returns the set of elements directly created in the context of this element.
   */
  def directContextContents: Set[Element[_]] = if (!active) {
    throw new NoSuchElementException
  } else myDirectContextContents

  private[language] def addContextContents(e: Element[_]): Unit = myDirectContextContents += e

  private[language] def removeContextContents(e: Element[_]): Unit = myDirectContextContents -= e

  /**
   * Returns true if this element is temporary, that is, was created in the context of another element.
   */
  def isTemporary = !myContext.isEmpty

  /**
   * Clears all the temporary elements associated with this element (all elements created in it's context).
   */
  def clearContext() = universe.deactivate(directContextContents)

  /*
   * Under the new design, conditions and constraints can be contingent on other elements taking on particular values. This correctly handles reference uncertainty where we
   * know that the element with a given name, whatever it is, satisfies a given condition, but we don't know what that element is. We apply the condition to every possible
   * element that could be referred to, but make it contingent on the reference actually referring to the variable.
   *
   * Since an element may be referred to in multiple ways, each of which can have its own contingency, we allow the conditions to contain multiple (Contingency, Condition)
   * pairs.
   */
  private[figaro]type Contingency = Element.Contingency
  private[figaro]type ElemVal[T] = Element.ElemVal[T]

  /**
   * Returns the elements that this element is contingent on. These are elements that are required to have a certain value for a condition or constraint
   * to be relevant to this element. The contingency is required because conditions and constraints can be applied to references that are
   * uncertain. Every possible element that could be pointed to by a reference must be given the condition or constraint, but the condition
   * or constraint only applies if the elements earlier in the reference have the required value.
   *
   * Figaro takes care of handling all this under the
   * hood. However, some algorithms may need to know which elements an element is contingent on. For example, sampling algorithms may need to sample
   * those other elements first. This method is supplied to support this use case.
   */
  def elementsIAmContingentOn: Set[Element[_]] = {
    val conditionElements =
      for {
        (condition, contingency) <- myConditions
        Element.ElemVal(element, value) <- contingency
      } yield element
    val constraintElements =
      for {
        (constraint, contingency) <- myConstraints
        Element.ElemVal(element, value) <- contingency
      } yield element
    Set((conditionElements ::: constraintElements): _*)
  }

  /*
   * Since a contingency is a type of use between elements, we need to add them to the uses and usedBy lists.
   * In the current implementation, they never get removed. It would be difficult to ensure that is always done correctly.
   * Not removing these elements from the relevant lists cannot affect correctness of algorithms, but it may impact their efficiency.
   * One can argue that removal of evidence is not a common use case and does not need to be optimized.
   */
  private def ensureContingency[T](elem: Element[T]) {
    universe.registerUses(this, elem)
  }

  private var myConditions: List[(Condition, Contingency)] = List()

  /** All the conditions defined on this element.*/
  def allConditions = myConditions

  /*
   * observation represents a specific observed value of this element, if the element
   * has an observation. It is essentially redundant, given that the observation will be
   * captured in a condition. However, for some algorithms, such as importance sampling,
   * it is useful to know that a condition is actually an observation of a specific value.
   * This is a common case, and to optimize it, we store the observation.
   *
   * If an element has any other condition besides this observation, we cannot use the
   * observation. However, it can have a constraint.
   */
  private[figaro] var observation: Option[T] = None

  /*
   * Testing whether a condition is satisfied can use any type of value. The condition can only be satisfied if the value has the right type and the condition returns true.
   */
  private def checkedCondition(condition: Condition, value: Any): Boolean =
    try { condition(value.asInstanceOf[Value]) } catch { case _: ClassCastException => false }

  /*
   * Determines whether a contingent condition is satisfied for a given value of this element. It is *not* satisfied only if all the contingent elements have their
   * appropriate values and the condition itself is not satisfied for the given value.
   */
  private def satisfiesContingentCondition(condition: Condition, contingency: Contingency, value: Any): Boolean = {
    val contingencySatisfied = contingency.forall((e: ElemVal[_]) => e.elem.value == e.value)
    !contingencySatisfied || checkedCondition(condition, value)
  }

  /**
   * Tests whether all the element's contingent conditions are satisfied for the given value.
   */
  def condition(value: Any) = {
    myConditions.forall((cc: (Condition, Contingency)) => satisfiesContingentCondition(cc._1, cc._2, value))
  }

  /**
   * Determines whether the condition on the element is satisfied by the current value.
   */
  def conditionSatisfied = condition(value)

  /** Add the given condition to the existing conditions of the element. By default, the contingency is empty. */
  def addCondition(condition: Condition, contingency: Contingency = List()): Unit = {
    universe.makeConditioned(this)
    contingency.foreach(ev => ensureContingency(ev.elem))
    observation = None
    myConditions ::= (condition, contingency)
  }

  /**
   * Remove all conditions associated with the given contingency. By default, the contingency is empty.
   */
  def removeConditions(contingency: Contingency = List()): Unit = {
    myConditions = myConditions.filterNot(_._2 == contingency)
    observation = None
    if (myConditions.isEmpty) universe.makeUnconditioned(this)
  }

  /**
   * Set the condition associated with the contingency. Removes previous conditions associated with the contingency.  By default, the contingency is empty.
   */
  def setCondition(newCondition: Condition, contingency: Contingency = List()): Unit = {
    removeConditions(contingency)
    addCondition(newCondition, contingency)
  }

  private var myConstraints: List[(Constraint, Contingency)] = List()

  /**
   * The current soft constraints on the element.
   */
  def allConstraints = myConstraints

  // Avoid issuing a warning every time this method is called, e.g. for every sample.
  private var constraintWarningIssued = false

  /*
   * Computes the result of the element's constraint on a given value.
   * A value of any type can be passed, but if the value is of an inappropriate type, the constraint result is negative infinity.
   * This method also issues a warning if the constraint is greater than log(1) = 0.
   */
  private def checkedConstraint(constraint: Constraint, value: Any): Double =
    try {
      val result = constraint(value.asInstanceOf[Value])
      if (result > 0 && !constraintWarningIssued) {

        //println("Warning: constraint value " + result + " is greater than 1. Algorithms that use an upper bound of 1 will be incorrect.")
        constraintWarningIssued = true
      }
      result
    } catch { case _: ClassCastException => Double.NegativeInfinity }

  /*
   * Determines the result of a contingent constraint for a given value of this element. If any of the contingent elements does not have its appropriate value, the result is 1,
   * otherwise it is the result of the constraint itself applied to the given value.
   */
  private def contingentConstraintResult(constraint: Constraint, contingency: Contingency, value: Any): Double = {
    val contingencySatisfied = contingency.forall((e: ElemVal[_]) => e.elem.value == e.value)
    if (contingencySatisfied) checkedConstraint(constraint, value); else 0.0
  }

  /**
   * Gets the result of all the element's contingent constraints for the given value.
   */
  def constraint(value: Any) = {
    val results = for { (constr, conting) <- myConstraints }
      yield contingentConstraintResult(constr, conting, value)
    (results :\ 0.0)(_ + _)
  }

  /**
   * Determines the value of the constraint on the element applied to the current value.
   */
  def constraintValue = constraint(value)

  /**
   * Compute the constraints on the new value divided by the constraints on the old value.
   */
  def score(oldValue: Value, newValue: Value): Double = constraint(newValue) - constraint(oldValue)

  /**
   * Add a contingent constraint to the element. By default, the contingency is empty.
   */
  def addConstraint(constraint: Constraint, contingency: Contingency = List()): Unit = {
    universe.makeConstrained(this)
    contingency.foreach(ev => ensureContingency(ev.elem))
    myConstraints ::= (ProbConstraintType(constraint), contingency)
  }

    /**
   * Add a log contingent constraint to the element. By default, the contingency is empty.
   */
  def addLogConstraint(constraint: Constraint, contingency: Contingency = List()): Unit = {
    universe.makeConstrained(this)
    contingency.foreach(ev => ensureContingency(ev.elem))
    myConstraints ::= (LogConstraintType(constraint), contingency)
  }

  /**
   * Remove all constraints associated with the given contingency. By default, the contingency is empty.
   */
  def removeConstraints(contingency: Contingency = List()): Unit = {
    myConstraints = myConstraints.filterNot(_._2 == contingency)
    if (myConstraints.isEmpty) universe.makeUnconstrained(this)
  }

  protected def removeConstraint(constraint: Constraint, contingency: Contingency = List()): Unit = {
    myConstraints = myConstraints.filterNot((c: (Constraint,Contingency)) => c._2 == contingency && c._1 == constraint)
    if (myConstraints.isEmpty) universe.makeUnconstrained(this)
  }


  /**
   * Set the constraint associated with the contingency. Removes previous constraints associated with the contingency.  By default, the contingency is empty.
   */
  def setConstraint(newConstraint: Constraint, contingency: Contingency = List()): Unit = {
    removeConstraints(contingency)
    addConstraint(newConstraint, contingency)
  }

    /**
   * Set the log constraint associated with the contingency. Removes previous constraints associated with the contingency.  By default, the contingency is empty.
   */
  def setLogConstraint(newConstraint: Constraint, contingency: Contingency = List()): Unit = {
    removeConstraints(contingency)
    addLogConstraint(newConstraint, contingency)
  }

  /**
   * Condition the element by observing a particular value. Propagates the effect to dependent elements and ensures that no other value for the element can be generated.
   */
  def observe(observation: Value): Unit = {
    removeConditions()
    set(observation)
    universe.makeConditioned(this)
    this.observation = Some(observation)
    myConditions ::= ((v: Value) => v == observation, List())
  }

  /**
   * Removes conditions on the element and allows different values of the element to be generated.
   */
  def unobserve(): Unit = {
    unset()
    removeConditions()
  }

  private var setFlag: Boolean = false

  /**
   * Allows different values of the element to be generated.
   */
  def unset(): Unit = {
    setFlag = false
    generate()
  }

  /**
   * Set the value of this element and propagate the effects to elements that depend on it
   * without changing their randomness. Also disallows the value of the element to change until unobserve or unset is called.
   */
  def set(newValue: Value): Unit = {
    value = newValue
    for {
      layer <- universe.layers(universe.usedBy(this))
      elem <- layer
    } {
      elem.generate()
    }
    setFlag = true
  }

  /**
   * Set the randomness of this element.
   *
   * Will generate its value using the new randomness and propagate the effects to elements that
   * depend on it without changing their randomness.
   */
  def setRandomness(newRandomness: Randomness): Unit = {
    randomness = newRandomness
    set(generateValue(randomness))
  }

  private var myPragmas: List[Pragma[Value]] = List()

  /**
   * The pragmas attached to the element.
   */
  def pragmas: List[Pragma[Value]] = myPragmas

  /**
   * Add a pragma to the element.
   */
  def addPragma(pragma: Pragma[Value]): Unit =
    myPragmas ::= pragma

  /**
   * Remove a pragma from the element.
   */
  def removePragma(pragma: Pragma[Value]): Unit =
    myPragmas = myPragmas filterNot (_ == pragma)

  /**
   * The arguments on which the element depends.
   */
  def args: List[Element[_]]

  /**
   * Flag indicating whether the element is currently active in its universe.
   */
  var active: Boolean = _

  universe.activate(this)

  /**
   * Activate the element in its universe.
   */
  def activate(): Unit = universe.activate(this)

  /**
   * Deactivate the element in its universe.
   */
  def deactivate(): Unit = universe.deactivate(this)

  /* Element self-generation on initialization was the cause of bugs. On infinite models, it can cause an infinite recursion, which could correctly be handled by
   * lazy factored inference. We have eliminated the need for self-generation on initialization. Algorithms that require elements to be generated should begin
   * by calling Universe.generateAll
   */
  //generate()

  collection.add(this)

  /**
   * The element that tests equality of this element with another element.
   */
  def ===(that: Element[Value])(implicit universe: Universe) = new Eq("", this, that, universe)

  /**
   * The element that tests whether the value of this element is equal to a particular value.
   */
  def ===(that: Value)(implicit universe: Universe) = new Apply1("", this, (v: Value) => v == that, universe)
  /**
   * The element that tests inequality of this element with another element.
   */
  def !==(that: Element[Value])(implicit universe: Universe) = new Neq("", this, that, universe)

  /**
   * A string that is the element's name, if it has a non-empty one, otherwise the result of the element's toString.
   */
  def toNameString = if (name.string != "") name.string; else toString

  def map[U](fn: Value => U)(implicit name: Name[U], collection: ElementCollection): Element[U] =
    Apply(this, fn)(name, collection)

  def flatMap[U](fn: Value => Element[U])(implicit name: Name[U], collection: ElementCollection): Element[U] =
    Chain(this, fn)(name, collection)
}

object Element {
  /**
   * Implicit conversion of an element over Booleans to a BooleanElement, allowing Boolean operators
   * to be applied to it.
   */
  implicit def toBooleanElement(elem: Element[Boolean]): BooleanElement = new BooleanElement(elem)

  /**
   * Implicit conversion of an element over Doubles to a DoubleElement, allowing Double operators
   * to be applied to it.
   */
  implicit def toDoubleElement(elem: Element[Double]): DoubleElement = new DoubleElement(elem)

  /**
   * Implicit conversion of an element over Ints to an IntElement, allowing Int operators
   * to be applied to it.
   */
  implicit def toIntElement(elem: Element[Int]): IntElement = new IntElement(elem)

  /**
   * Implicit conversion of an element over pairs to a DuoElement, allowing component extractors
   * to be applied to it.
   */
  implicit def toDuoElement[T1, T2](elem: Element[(T1, T2)]): DuoElement[T1, T2] = new DuoElement(elem)

  /**
   * Implicit conversion of an element over triples to a TrioElement, allowing component extractors
   * to be applied to it.
   */
  implicit def toTrioElement[T1, T2, T3](elem: Element[(T1, T2, T3)]): TrioElement[T1, T2, T3] = new TrioElement(elem)

  /**
   * Implicit conversion of an element over quadruples to a QuartetElement, allowing component extractors
   * to be applied to it.
   */
  implicit def toQuartetElement[T1, T2, T3, T4](elem: Element[(T1, T2, T3, T4)]): QuartetElement[T1, T2, T3, T4] =
    new QuartetElement(elem)

  /**
   * Implicit conversion of an element over quintuples to a QuintetElement, allowing component extractors
   * to be applied to it.
   */
  implicit def toQuintetElement[T1, T2, T3, T4, T5](elem: Element[(T1, T2, T3, T4, T5)]): QuintetElement[T1, T2, T3, T4, T5] =
    new QuintetElement(elem)

  /** A convenience class that pairs an element and a possible value, ensuring they have compatible types. */
  case class ElemVal[T](elem: Element[T], value: T)

  /** The type of contingencies that can hold on elements. */
  type Contingency = List[ElemVal[_]]

  /**
   * Returns the given elements and all elements on which they are contingent, closed recursively.
   * Only elements with condition.
   */
  def closeUnderContingencies(elements: scala.collection.Set[Element[_]]): scala.collection.Set[Element[_]] = {
    def findContingent(elements: scala.collection.Set[Element[_]]): scala.collection.Set[Element[_]] = {
      // Find all elements not in the input set that the input set is contingent on
      for {
        element <- elements
        contingent <- element.elementsIAmContingentOn
        if !elements.contains(contingent)
      } yield contingent
    }
    var result = elements
    var adds = findContingent(result)
    while (!adds.isEmpty) {
      result ++= adds
      adds = findContingent(result)
    }
    result
  }
}

/**
 * Elements whose values can be cached and reused.
 */
trait Cacheable[V] extends Element[V] {
  override def isCachable = true
}

/**
 * Elements whose values can be cached and reused as long as the arguments are cacheable.
 */
trait IfArgsCacheable[V] extends Element[V] {
  override def isCachable = args forall (_.isCachable)
}
