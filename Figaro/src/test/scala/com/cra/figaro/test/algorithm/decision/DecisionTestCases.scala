/*
 * DecisionTestCases.scala 
 * TBD needs description
 * 
 * Created By:      Brian Ruttenberg (bruttenberg@cra.com)
 * Creation Date:   Jan 1, 2009
 * 
 * Copyright 2017 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.test.algorithm.decision

import com.cra.figaro.algorithm._
import com.cra.figaro.algorithm.decision._
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.algorithm.factored._
import com.cra.figaro.language._
import com.cra.figaro.library.decision._
import com.cra.figaro.library.compound._
import com.cra.figaro.library.atomic.discrete.Uniform
import com.cra.figaro.library.atomic.continuous.Normal
import scala.collection.mutable.Map
import scala.math._

object DecisionTestCases {

  /* ppp = -2 -> E[true] = -5, E[false] = 6 -> choose false
   * ppp = 2 -> E[true] = 1, E[false] = -2 -> choose true
   * ppp = 0 -> E[true] = .25*-7+.75*1 = -1, E[false] = .25*6+.75*-2 = 0 -> choose false
   */
  def DecisionDiscrete(algCreator: (List[Element[Double]], Decision[Int, Boolean]) => ProbQueryAlgorithm): (Decision[Int, Boolean], DecisionAlgorithm[Int, Boolean]) = {
    Universe.createNew()
    val p = Select(0.25 -> 0, 0.75 -> 2)
    val pp = Uniform(-2, 0)
    //val ppp = CachingChain(p, pp, (a: Int, b: Int) => Constant[Int](a + b))
    val ppp = Apply(p, pp, (a: Int, b: Int) => a + b)
    val d = Decision(ppp, List(true, false))
    def u_fcn(b: Boolean, i: Int): Double = {
      i match {
        case 0 => if (b) -7.0 else 6.0
        case 2 => if (b) 1.0 else -2.0
      }
    }
    val u = Apply(d, p, u_fcn)

    val alg = algCreator(List(u), d)
    alg.start()
    alg.asInstanceOf[DecisionAlgorithm[Int, Boolean]].setPolicy(d)
    (d, alg.asInstanceOf[DecisionAlgorithm[Int, Boolean]])
  }

  /* Closely replicates the discrete test. Should be pretty close to the same decisions, 
   * but some tests may fail (inconsistently)
   * 
   * Expected values should be nearly the same as the discrete tests, but may need a larger tolerance
   * 
   */
  def DecisionContinuous(algCreator: (List[Element[Double]], Decision[Double, Boolean]) => ProbQueryAlgorithm,
    sim: Boolean): (Decision[Double, Boolean], DecisionAlgorithm[Double, Boolean], Double, Double) = {
    Universe.createNew()
    val p = Select(0.25 -> 0, 0.75 -> 2)
    val pp = Uniform(-2, 0)
    val ppp = Chain(p, pp, (a: Int, b: Int) => Normal((a + b).toDouble, 0.01))

    val d = new NonCachingDecision("", ppp, (p: Double) => Uniform(true, false), Universe.universe) with PolicyMaker[Double, Boolean] {
      def makePolicy(policyMap: scala.collection.immutable.Map[(Double, Boolean), DecisionSample]) = DecisionPolicyNN(policyMap, 10000)
    }

    def u_fcn(b: Boolean, i: Int): Double = {
      i match {
        case 0 => if (b) -7.0 else 6.0
        case 2 => if (b) 1.0 else -2.0
      }
    }
    val u = Apply(d, p, u_fcn)

    val Exp_before = if (sim) {
      val mh_before = Importance(5000, u)
      mh_before.run()
      mh_before.computeExpectation(u, (t: Double) => t)
    } else {
      0.0
    }

    val alg = algCreator(List(u), d)
    alg.start()
    alg.asInstanceOf[DecisionAlgorithm[Double, Boolean]].setPolicy(d)

    val Exp_after = if (sim) {
      val mh_after = Importance(5000, u)
      mh_after.run()
      mh_after.computeExpectation(u, (t: Double) => t)
    } else {
      0.0
    }
    (d, alg.asInstanceOf[DecisionAlgorithm[Double, Boolean]], Exp_before, Exp_after)
  }

  /* Discrete decision test with posted evidence. p has a larger range, but we set a condition
   * that p must equal 0 or 2. This will make it equivalent to the normal discrete test 
   */
  def DecisionDiscreteEvidence(algCreator: (List[Element[Double]], Decision[Int, Boolean]) => ProbQueryAlgorithm): (Decision[Int, Boolean], DecisionAlgorithm[Int, Boolean]) = {
    Universe.createNew()
    val p = Select(0.2 -> -1, 0.1 -> 0, 0.3 -> 2, 0.4 -> 8)
    val pp = Uniform(-2, 0)
    val ppp = CachingChain(p, pp, (a: Int, b: Int) => Constant[Int](a + b))
    val d = new NonCachingDecision("", ppp, (i: Int) => Uniform(true, false), Universe.universe) with ExactPolicyMaker[Int, Boolean]
    val u = RichCPD[Boolean, Int, Double](d, p,
      (OneOf(true), OneOf(0)) -> Constant(-7.0), (OneOf(true), OneOf(2)) -> Constant(1.0),
      (OneOf(false), OneOf(0)) -> Constant(6.0), (OneOf(false), OneOf(2)) -> Constant(-2.0),
      (*, OneOf(-1)) -> Constant(0.0), (*, OneOf(-3)) -> Constant(0.0),
      (*, OneOf(6)) -> Constant(0.0), (*, OneOf(8)) -> Constant(0.0))

    p.addCondition((i: Int) => (i == 0) || (i == 2))

    val alg = algCreator(List(u), d)
    alg.start()
    alg.asInstanceOf[DecisionAlgorithm[Int, Boolean]].setPolicy(d)
    (d, alg.asInstanceOf[DecisionAlgorithm[Int, Boolean]])
  }

  def DecisionContinuousEvidence(algCreator: (List[Element[Double]], Decision[Double, Boolean]) => ProbQueryAlgorithm,
    sim: Boolean): (Decision[Double, Boolean], DecisionAlgorithm[Double, Boolean], Double, Double) = {
    Universe.createNew()
    val p = Select(0.2 -> -1, 0.1 -> 0, 0.3 -> 2, 0.4 -> 8)
    val pp = Uniform(-2, 0)
    val ppp = Chain(p, pp, (a: Int, b: Int) => Normal((a + b).toDouble, 0.01))

    val d = new NonCachingDecision("", ppp, (p: Double) => Uniform(true, false), Universe.universe) with PolicyMaker[Double, Boolean] {
      def makePolicy(policyMap: scala.collection.immutable.Map[(Double, Boolean), DecisionSample]) = DecisionPolicyNN(policyMap, 10000)
    }
    def u_fcn(b: Boolean, i: Int): Double = {
      i match {
        case 0 => if (b) -7.0 else 6.0
        case 2 => if (b) 1.0 else -2.0
        case _ => 0.0
      }
    }
    val u = Apply(d, p, u_fcn)

    val Exp_before = if (sim) {
      val mh_before = Importance(5000, u)
      mh_before.run()
      mh_before.computeExpectation(u, (t: Double) => t)
    } else {
      0.0
    }

    p.addCondition((i: Int) => (i == 0) || (i == 2))

    val alg = algCreator(List(u), d)
    alg.start()
    alg.asInstanceOf[DecisionAlgorithm[Double, Boolean]].setPolicy(d)

    val Exp_after = if (sim) {
      val mh_after = Importance(5000, u)
      mh_after.run()
      mh_after.computeExpectation(u, (t: Double) => t)
    } else {
      0.0
    }
    (d, alg.asInstanceOf[DecisionAlgorithm[Double, Boolean]], Exp_before, Exp_after)
  }

  def MultiDecisionDiscrete(algCreator: (List[Element[Double]], List[Decision[Boolean, Double]]) => MultiDecisionAlgorithm,
    sim: Boolean): (MultiDecisionAlgorithm, List[Decision[Boolean, Double]], Double, Double) = {
    // optimal decisions for this scenario:    
    // d1: true->0.1, false->0.5
    // d2: true->0.5, false->0.1

    /* For d2:
     * F2 == True => E[.1] = 0, E[.2] = .1, E[.3] = .2, E[.4] = .3, E[.5] = .4
     * F2 == False => E[.1] = 0, E[.2] = -.1, E[.3] = -.2, E[.4] = -.3, E[.5] = -.4
     * 
     * For d1:
     * F1 == True => E[.1] = 0 + .1*.4 + .9*0 = .04
     * 				 E[.2] = -.1 + .2*.4 + .8*0 = -.02
     * 				 E[.3] = -.2 + .3*.4 + .7*0 = -.08
     * 			     E[.4] = -.3 + .4*.4 + .6*0 = -.14
     * 				 E[.5] = -.4 + .5*.4 + .5*0 = -.20
     * 
     * F1 == False => E[.1] = .0 + .1*.4 + .9*0 = .04
     * 				  E[.2] = .1 + .2*.4 + .8*0 = .18
     * 				  E[.3] = .2 + .3*.4 + .7*0 = .32
     * 			      E[.4] = .3 + .4*.4 + .6*0 = .46
     * 				  E[.5] = .4 + .5*.4 + .5*0 = .60
     */

    Universe.createNew()
    val f1 = Flip(0.5)
    val d1 = Decision(f1, 0.1 to 0.5 by 0.1)("d1", Universe.universe)
    val u1 = Apply(f1, d1, (f: Boolean, p: Double) => (if (f == false) p - .1 else .1 - p))
    val f2 = Flip(d1)
    val d2 = Decision(f2, 0.1 to 0.5 by 0.1)("d2", Universe.universe)
    val u2 = Apply(f2, d2, (f: Boolean, p: Double) => (if (f == true) p - .1 else .1 - p))

    val Exp_before = if (sim) {
      val ve_before = MetropolisHastings(20000, ProposalScheme.default, 1000, u1, u2)
      ve_before.start()
      List(u1, u2).map(u => ve_before.computeExpectation(u, (t: Double) => t)).sum
    } else {
      0.0
    }

    val ve = algCreator(List(u1, u2), List(d1, d2))
    ve.start()

    val Exp_after = if (sim) {
      val ve_after = MetropolisHastings(20000, ProposalScheme.default, 1000, u1, u2)
      ve_after.start()
      List(u1, u2).map(u => ve_after.computeExpectation(u, (t: Double) => t)).sum
    } else {
      0.0
    }

    (ve, List(d1, d2), Exp_before, Exp_after)
  }

  def MultiDecisionContinuous(algCreator: (List[Element[Double]], List[Decision[Double, Double]]) => MultiDecisionAlgorithm,
    sim: Boolean): (MultiDecisionAlgorithm, List[Decision[Double, Double]], Double, Double) = {
    // optimal decisions for this scenario:  
    // d1: true->0.1, false->0.5
    // d2: true->0.5, false->0.1

    Universe.createNew()
    val f1 = If(Flip(0.5), Normal(1.0, 0.01), Normal(0.0, 0.01))
    val d1 = new NonCachingDecision("", f1, (p: Double) => Uniform((0.1 to 0.5 by 0.1): _*), Universe.universe) with PolicyMaker[Double, Double] {
      def makePolicy(policyMap: scala.collection.immutable.Map[(Double, Double), DecisionSample]) = DecisionPolicyNN(policyMap, 1000)
    }
    val u1 = Apply(f1, d1, (f: Double, p: Double) => (math.signum(0.5 - f) * (p - .1)))
    val f2 = If(Flip(d1), Normal(1.0, 0.01), Normal(0.0, 0.01))
    val d2 = new NonCachingDecision("", f2, (p: Double) => Uniform((0.1 to 0.5 by 0.1): _*), Universe.universe) with PolicyMaker[Double, Double] {
      def makePolicy(policyMap: scala.collection.immutable.Map[(Double, Double), DecisionSample]) = DecisionPolicyNN(policyMap, 1000)
    }
    val u2 = Apply(f2, d2, (f: Double, p: Double) => (math.signum(f - 0.5) * (p - .1)))

    val Exp_before = if (sim) {
      val ve_before = MetropolisHastings(20000, ProposalScheme.default, 1000, u1, u2)
      ve_before.start()
      List(u1, u2).map(u => ve_before.computeExpectation(u, (t: Double) => t)).sum
    } else {
      0.0
    }

    val ve = algCreator(List(u1, u2), List(d1, d2))
    ve.start()

    val Exp_after = if (sim) {
      val ve_after = MetropolisHastings(20000, ProposalScheme.default, 1000, u1, u2)
      ve_after.start()
      List(u1, u2).map(u => ve_after.computeExpectation(u, (t: Double) => t)).sum
    } else {
      0.0
    }

    (ve, List(d1, d2), Exp_before, Exp_after)
  }

}
