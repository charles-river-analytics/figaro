/*
 * Decision.scala
 * Element which represents decisions in a model
 * 
 * Created By:      Brian Ruttenberg (bruttenberg@cra.com)
 * Creation Date:   Oct 4, 2012
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.library.decision

import com.cra.figaro.algorithm._
import com.cra.figaro.algorithm.factored.factors._
import com.cra.figaro.algorithm.lazyfactored._
import com.cra.figaro.algorithm.decision._
import com.cra.figaro.algorithm.decision.index._
import com.cra.figaro.language._
import com.cra.figaro.library.atomic.discrete._
import scala.collection.mutable.Set

/**
 * A Decision class represents a decision that takes in a single parent element, applies a function (aka a policy)
 * on the value of the parent, and returns a decision element. A decision is essentially a chain, with the added
 * capability to change the function in the chain. If your decision has more than one parent, you can create an element
 * tuple of the parents and use the tuple as the input to the decision.
 *
 */
/*
 * 
 * Inside each decision class is a DecisionPolicy class. The DecisionPolicy class controls how the optimal
 * decision for each parent value is chosen. Generally the default policy algorithm will work for most people,
 * but they can be override if necessary. The decision class extends PolicyMaker, which controls how to create
 * a DecisionPolicy for a decision. The default implementations of decisions automatically extend
 * PolicyMaker as appropriate. If you want a different DecisionPolicy, you need to extend the trait
 * PolicyMaker and declare the function makePolicy.
 *
 * There are two types of chains. A caching version and a noncaching version. The caching should only be used
 * on domains where the parents values are discrete and have a small range, and it implements an exact policy
 * (i.e., during decision inference, it must compute a policy for every possible value of parents). The
 * noncaching uses an approximate policy algorithm (based on kNN) and should be used for continuous
 * parents or discrete parents with a very large range.
 */
trait Decision[T, U] extends Chain[T, U] with PolicyMaker[T, U] {

  /**
   * The parent type.
   */
  type PValue = this.ParentType
  
  /**
   * The decision type.
   */
  type DValue = this.Value

  var fcn: T => Element[U]
  
  /**
   * The decision function. fcn is declared as a var and can change depending on the policy.
   */
  override protected def cpd = this.fcn

  private[figaro] var policy: DecisionPolicy[T, U] = null

  /**
   * Get the decision for a specific value of the parent.
   */
  def getPolicy(p: T): Element[U] = { val result = get(p); result.generate(); result }
  
  /**
   * Get the complete policy object for this decision.
   */
  def getPolicy(): DecisionPolicy[T, U] = policy

  /**
   * Set the policy for this decision using the DecisionPolicy class stored in the Decision.
   */
  def setPolicy(): Unit = setPolicy(policy.toFcn())
  
  /**
   * Set the policy for this decision from an Algorithm run on this decision.
   */
  def setPolicy(Alg: DecisionAlgorithm[T, U]): Unit = setPolicy(Alg.getUtility())
  
  /**
   * Set the policy for this decision using a map from parent/decision tuple values to decision samples.
   */
  def setPolicy(policyMap: Map[(T, U), DecisionSample]): Unit = {
    policy = makePolicy(policyMap)
    setPolicy(policy.toFcn())
  }

  /**
   * Directly set the policy of this decision using a function from a parent value to a decision element.
   */
  def setPolicy(new_fcn: (T => Element[U])): Unit = {
    // Override the fcn variable
    fcn = new_fcn

    // Remove all factors since the old factors are now out of date
    Factory.removeFactors
    // Have to nullify the last result even if parents the same since the function changed
    clearContext
    // Have to clear the last element in the cache since clearTempory always leaves an element in the cache
    //if (cache.nonEmpty) resizeCache(cache.last._1)    
    // Have to remove the expansion of the universe since it is out of data
    LazyValues.clear(universe)
    // Must regenerate a new value since the cache should never be empty
    generateValue()
  }
}


/**
 * Abstract class for a NonCachingDecision. It is abstract because makePolicy has not been defined yet.
 */
abstract class NonCachingDecision[T, U](name: Name[U], parent: Element[T], var fcn: T => Element[U], collection: ElementCollection)
  extends NonCachingChain(name, parent, fcn, collection) with Decision[T, U] {

  override def toString = "NonCachingDecision(" + parent + ", " + this.cpd + ")"
}

/**
 * Abstract class for a CachingDecision. It is abstract because makePolicy has not been defined yet.
 */
abstract class CachingDecision[T, U](name: Name[U], parent: Element[T], var fcn: T => Element[U], collection: ElementCollection)
  extends CachingChain(name, parent, fcn, collection) with Decision[T, U] {

  override def toString = "CachingDecision(" + parent + ", " + this.cpd + ")"
}

/*
 * By default, noncaching decisions implement an approximate policy (a kNN algorithm) by extending the PolicyMaker trait and defining
 * the required makePolicy function to create a DecisionPolicyApprox. If you need to modify the parameters of the DecisionPolicyApprox
 * class or want a different class, you have to create an instance of  NonCachingDecision, extend PolicyMaker and define makePolicy.
 * See below for examples or in the example package
 */

object NonCachingDecision {

  /**
   * Create a NonCachingDecision with no parent that uses an approximate (kNN) PolicyMaker.
   */
  def apply[U](fcn: () => Element[U])(implicit name: Name[U], collection: ElementCollection, conversion: Int => Distance[Int]): Decision[Int, U] = {
    new NonCachingDecision(name, Constant(0), (i: Int) => fcn(), collection) with PolicyMaker[Int, U] {
      def makePolicy(policyMap: Map[(Int, U), DecisionSample]) = DecisionPolicyNN(policyMap)
    }
  }

  /**
   * Create a NonCachingDecision with one parent and a default function from values of the parent to Element[U].
   * Uses an approximate (kNN) PolicyMaker.
   */
  def apply[T, U](arg1: Element[T], fcn: (T) => Element[U])(implicit name: Name[U], collection: ElementCollection, conversion: T => Distance[T]): Decision[T, U] = {
    new NonCachingDecision(name, arg1, fcn, collection) with PolicyMaker[T, U] {
      def makePolicy(policyMap: Map[(T, U), DecisionSample]) = DecisionPolicyNN(policyMap)
    }
  }

  /**
   * Create a NonCachingDecision with no parent and sequence of the possible actions of the decision.
   * Uses an approximate (kNN) PolicyMaker.
   */
  def apply[U](range: Seq[U])(implicit name: Name[U], collection: ElementCollection, conversion: Int => Distance[Int]): Decision[Int, U] = {
    val defElement = rangeToElement(range)
    apply(() => defElement)(name, collection, conversion)
  }

  /**
   * Create a NonCachingDecision with one parent and sequence of the possible actions of the decision.
   * Uses an approximate (kNN) PolicyMaker.
   */
  def apply[T, U](arg1: Element[T], range: Seq[U])(implicit name: Name[U], collection: ElementCollection, conversion: T => Distance[T]): Decision[T, U] = {
    val defElement = rangeToElement(range)
    apply(arg1, (d: T) => defElement)(name, collection, conversion)
  }

  private def rangeToElement[U](range: Seq[U]): Element[U] = Uniform[U](range: _*)
}

/*
 * Caching chain by default implements an exact policy. If you want to override this, see instructions/code for the 
 * noncaching chain.
 */

object CachingDecision {
  /**
   * Create a CachingDecision with no parent that uses an exact PolicyMaker.
   */
  def apply[U](fcn: () => Element[U])(implicit name: Name[U], collection: ElementCollection): Decision[Int, U] = {
    new CachingDecision(name, Constant(0), (i: Int) => fcn(), collection) with ExactPolicyMaker[Int, U]
  }

   /**
   * Create a CachingDecision with one parent and a default function from values of the parent to Element[U].
   * Uses an exact PolicyMaker.
   */
  def apply[T, U](arg1: Element[T], fcn: (T) => Element[U])(implicit name: Name[U], collection: ElementCollection): Decision[T, U] = {
    new CachingDecision(name, arg1, fcn, collection) with ExactPolicyMaker[T, U]
  }

  /**
   * Create a CachingDecision with no parent and sequence of the possible actions of the decision.
   * Uses an exact PolicyMaker.
   */
  def apply[U](range: Seq[U])(implicit name: Name[U], collection: ElementCollection): Decision[Int, U] = {
    apply(() => rangeToElement(range))(name, collection)
  }

   /**
   * Create a CachingDecision with one parent and sequence of the possible actions of the decision.
   * Uses an exact PolicyMaker.
   */
  def apply[T, U](arg1: Element[T], range: Seq[U])(implicit name: Name[U], collection: ElementCollection): Decision[T, U] = {
    apply(arg1, (d: T) => rangeToElement(range))(name, collection)
  }

  private def rangeToElement[U](range: Seq[U]): Element[U] = Uniform[U](range: _*)
}

/**
 * By default, creating a Decision uses a Caching decision and an exact policy. See NonCachingDecision for
 * approximate policies.
 */
object Decision {
   /**
   * Create a CachingDecision with no parent that uses an exact PolicyMaker.
   */
  def apply[U](fcn: () => Element[U])(implicit name: Name[U], collection: ElementCollection): Decision[Int, U] = {
    new CachingDecision(name, Constant(0), (i: Int) => fcn(), collection) with ExactPolicyMaker[Int, U]
  }

   /**
   * Create a CachingDecision with one parent and a default function from values of the parent to Element[U].
   * Uses an exact PolicyMaker.
   */
  def apply[T, U](arg1: Element[T], fcn: (T) => Element[U])(implicit name: Name[U], collection: ElementCollection): Decision[T, U] = {
    new CachingDecision(name, arg1, fcn, collection) with ExactPolicyMaker[T, U]
  }

  /**
   * Create a CachingDecision with no parent and sequence of the possible actions of the decision.
   * Uses an exact PolicyMaker.
   */
  def apply[U](range: Seq[U])(implicit name: Name[U], collection: ElementCollection): Decision[Int, U] = {
    apply(() => rangeToElement(range))(name, collection)
  }

  /**
   * Create a CachingDecision with one parent and sequence of the possible actions of the decision.
   * Uses an exact PolicyMaker.
   */
  def apply[T, U](arg1: Element[T], range: Seq[U])(implicit name: Name[U], collection: ElementCollection): Decision[T, U] = {
    apply(arg1, (d: T) => rangeToElement(range))(name, collection)
  }

  private def rangeToElement[U](range: Seq[U]): Element[U] = Uniform[U](range: _*)
}
