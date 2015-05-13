/*
 * DecisionPolicy.scala
 * Abstract class to create decision policies
 * 
 * Created By:      Brian Ruttenberg (bruttenberg@cra.com)
 * Creation Date:   Oct 1, 2012
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.algorithm.decision

import com.cra.figaro.algorithm._
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.algorithm.decision.index._
import com.cra.figaro.language._
import com.cra.figaro.library.compound._
import com.cra.figaro.library.decision._
import com.cra.figaro.util._
import scala.collection.immutable.Map
import scala.collection.mutable.MultiMap
import com.cra.figaro.algorithm.lazyfactored.Regular

/**
 * Abstract base class for all Decision Policies. Must define two functions:
 * toFcn: T => Element[U] - this is the function that is called to
 *  compute the decision for a parent value.
 *
 * toUtility: T => Element[Double] - this returns the expected utility of the
 * decision for a parent value. Used in backward induction algorithm.
 *
 * @tparam T The parent value type
 * @tparam U The decision type
 */
trait DecisionPolicy[T, U] {
  
  /* All implemented Decision policies must define these two functions   */
  /**
   * The function that returns a decision (Element[U]) given the value of the parent T.
   */
  def toFcn(): T => Element[U]
  
  /**
   * The function that returns the expected utility (Element[Double]) given the value of the parent T.
   */
  def toUtility(): T => Element[Double]
}

object DecisionPolicy {

  private val DELTA = 1e-10

  /**
   * Computes the maximal decision from a set of (decision, utility) samples,
   * returned from a nearest neighbor algorithm. It is unweighted because the
   * samples are NOT weighted by their distance from the parent value.
   */
  def UWMAX[U](nn: List[(Double, U, DecisionSample)]): (U, Double) = {
    val decisions = scala.collection.mutable.Map[U, DecisionSample]()
    nn.foreach { v =>
      val decision = v._2
      val sample = v._3
      val curr = decisions.getOrElse(decision, DecisionSample(0.0, 0.0))
      decisions += decision -> (curr + sample)
    }
    val de = decisions.mapValues(s => s.norm).maxBy(_._2)
    de
  }
  /**
   * Computes the maximal decision from a set of (decision, utility) samples,
   * returned from the nearest neighbor algorithm. This method IS weighted. Samples
   * who's parent value closer to the query parent value are weighted more when
   * computing the expected value.
   */
  def WMAX[U](nn: List[(Double, U, DecisionSample)]): (U, Double) = {
    val decisions = scala.collection.mutable.Map[U, DecisionSample]()
    val totalDistance = nn.map(_._1 + DELTA).sum
    nn.foreach { v =>
      val distance = 1.0 - v._1 / totalDistance
      val decision = v._2
      val sample = DecisionSample(v._3.weightedUtil, v._3.weight * distance)
      val curr = decisions.getOrElse(decision, DecisionSample(0.0, 0.0))
      decisions += decision -> (curr + sample)
    }
    val de = decisions.mapValues(s => s.norm).maxBy(_._2)
    de
  }
}

/**
 * An exact decision policy. This policy is exact because every possible value of the parent must have
 * a defined policy. This makes it suitable for variable elimination algorithms or sampling algorithms
 * if the range of the parent is small and enough samples are generated.
 *
 */

class DecisionPolicyExact[T, U](policy: Map[T, (U, Double)]) extends DecisionPolicy[T, U] {
  /* Returns a Constant of the computed decision. Can be overriden to return a different element if needed */
  override def toFcn() = (p: T) => {
    if (!policy.contains(p)) throw new ParentValueNotFoundException
    Constant(policy(p)._1)
  }

  /* Returns a Constant of the computed utility. Can be overriden to return a different element if needed */
  override def toUtility() = (p: T) => {
    if (!policy.contains(p)) throw new ParentValueNotFoundException
    Constant(policy(p)._2)
  }
}

/**
 * A nearest neighbor decision policy. This policy computes an approximate decision from a sampling algorithm.
 * The input to the class is an index (which holds (parent, decision) samples) a function that will combine
 * a set of (decision, utility) samples into a single decision, and numNNSamples, the number of samples to use in 
 * a nearest neighbor algorithm. By default, this uses a VP-tree to store the samples.
 */

class DecisionPolicyNN[T <% Distance[T], U](D: Index[T, U], combineFcn: (List[(Double, U, DecisionSample)]) => (U, Double), 
  protected var numNNSamples: Double) extends DecisionPolicy[T, U] {
  
  /**
   * Set the number of nearest neighbor samples to use in policies based on nearest neighbor.
   */
  def setNumNNSamples(i: Double) = numNNSamples = i
  
  /**
   * Returns the number of nearest neighbors to use. If kNN is greater than 1, then return kNN. If kNN is less than
   * 1, then return kNN* Number of Samples.
   */
  def getNumNNSamples = if (numNNSamples >= 1) {
    numNNSamples.toInt
  } else {
    math.max(1, (numNNSamples * D.size.toDouble).toInt)
  }
  
  /* Returns a Constant of the computed decision. Can be overriden to return a different element if needed */
  override def toFcn() = (p: T) => fcnHelper(p, getNumNNSamples)
  private def fcnHelper(p: T, num: Int): Element[U] = {
    val v = combineFcn(D.getNN(p, num))
    Constant(v._1)
  }

  /* Returns a Constant of the computed utility. Can be overriden to return a different element if needed */
  override def toUtility() = (p: T) => fcnUtilityHelper(p, getNumNNSamples)
  private def fcnUtilityHelper(p: T, num: Int): Element[Double] = {
    val v = combineFcn(D.getNN(p, num))
    Constant(v._2)
  }

}


object DecisionPolicyExact {
  private def maxPolicy[T, U](values: Map[(T, U), DecisionSample]): Map[T, (U, Double)] = {
    val Strat = scala.collection.mutable.Map[T, (U, Double)]()
    values.foreach { v =>
      val parent = v._1._1
      val decision = v._1._2
      val sample = v._2
      val curr = Strat.getOrElse(parent, (decision, scala.Double.MinValue))
      Strat += parent -> { if (sample.norm > curr._2) { (decision, sample.norm) } else { curr } }
    }
    Strat.toMap
  }

  /** 
   *  Create an exact decision policy from a Map of (parent, decision) tuples to a DecisionSample.
   */
  def apply[T, U](policy: Map[(T, U), DecisionSample]) = {
    new DecisionPolicyExact(maxPolicy(policy))
  }

  /** 
   *  Create an exact decision policy from a DecisionAlgorithm.
   */
  def apply[T, U](Alg: DecisionAlgorithm[T, U]) = {
    val policy_t = Alg.getUtility()
    new DecisionPolicyExact(maxPolicy(policy_t))
  }
}

/*
 * Class to implement approximate decision policies
 * This class is mean for continuous decision parents and discrete parents with a large range. This policy will implement an index to
 * retrieve the k nearest neighbors to the parent value and compute from the maximal decision from the retrieved samples.
 *
 * By default, it implements a VP-Tree index for NN query, and uses an unweighted max function
 * to determine the optimal decision from the k NN samples.
 *
 * There are several options available:
 *
 * 1. Change the index by creating a new index and calling DecisionPolicy with your index. Can also use a Flat index
 * 	which is provided (VP-Tree only guaranteed on Metric distances). See Index Package for creating your own index
 * 2. Change the function to compute the optimal decision from samples by calling DecisionPolicy with your own function.
 * 	A weighted max function is provided as well
 * 3. Change kNN (currently default is 1% of the sample size) when instantiating a DecisionPolicyApprox. If kNN < 1, then
 *  the number of nearest  neighbors is set as kNN*#samples, otherwise if kNN >= 1, number of neighbors = kNN
 *
 * Note that any user defined class that a decision is based on must implement the Distance trait in order to be used in an
 * approximate decision policy.
 *
 */
object DecisionPolicyNN {

  /** 
   *  Create an approximate decision policy from a DecisionAlgorithm, using the supplied combination function
   *  and kNN. This uses the default index (VP-Tree).
   *  
   */
  def apply[T <% Distance[T], U](Alg: DecisionAlgorithm[T, U], 
    combineFcn: (List[(Double, U, DecisionSample)]) => (U, Double), kNN: Double) = {
    val policy = Alg.getUtility()
    val nnIndex = new VPIndex[T, U](policy, 100)
    new DecisionPolicyNN(nnIndex, combineFcn, kNN)
  }

  /** 
   *  Create an approximate decision policy from a Map of (parent, decision) tuples to a DecisionSample, 
   *  using the supplied combination function and kNN. This uses the default index (VP-Tree).
   *  
   */
  def apply[T <% Distance[T], U](policy: Map[(T, U), DecisionSample], 
    combineFcn: (List[(Double, U, DecisionSample)]) => (U, Double), kNN: Double) = {
    val nnIndex = new VPIndex[T, U](policy, 100)
    new DecisionPolicyNN(nnIndex, combineFcn, kNN)
  }

  /** 
   *  Create an approximate decision policy from an index, using the supplied combination function and kNN. 
   *  
   */
  def apply[T <% Distance[T], U](nnIndex: Index[T, U], 
    combineFcn: (List[(Double, U, DecisionSample)]) => (U, Double), kNN: Double) = {
    new DecisionPolicyNN(nnIndex, combineFcn, kNN)
  }

  /** 
   *  Create an approximate decision policy from a DecisionAlgorithm, using the supplied kNN. 
   *  This uses the default combination function (unweighted maximum) and index (VP-Tree).
   *  
   */
  def apply[T <% Distance[T], U](Alg: DecisionAlgorithm[T, U], kNN: Double): DecisionPolicy[T, U] = {
    apply(Alg, DecisionPolicy.UWMAX[U]_, kNN)
  }

  /** 
   *  Create an approximate decision policy from a Map of (parent, decision) tuples to a DecisionSample,
   *  using the supplied kNN. This uses the default combination function (unweighted maximum) and index (VP-Tree).
   *  
   */
  def apply[T <% Distance[T], U](policy: Map[(T, U), DecisionSample], kNN: Double = .01): DecisionPolicy[T, U] = {
    apply(policy, DecisionPolicy.UWMAX[U]_, kNN)
  }

  /** 
   *  Create an approximate decision policy from an index, using the supplied kNN. 
   *  This uses the default combination function (unweighted maximum).
   *  
   */
  def apply[T <% Distance[T], U](nnIndex: Index[T, U], kNN: Double): DecisionPolicy[T, U] = {
    apply(nnIndex, DecisionPolicy.UWMAX[U]_, kNN)
  }

}















