/*
 * MHTest.scala  
 * Metropolis-Hastings tests.
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Jan 1, 2009
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.test.algorithm.sampling

import org.scalatest.Matchers
import org.scalatest.{ WordSpec, PrivateMethodTester }
import com.cra.figaro.algorithm._
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.algorithm.sampling.MetropolisHastings._
import com.cra.figaro.language._
import com.cra.figaro.library.atomic.continuous._
import com.cra.figaro.library.compound._
import com.cra.figaro.test._
import scala.collection.mutable.Map
import scala.language.existentials
import com.cra.figaro.test.tags.Performance
import com.cra.figaro.test.tags.NonDeterministic

class MHTest extends WordSpec with Matchers with PrivateMethodTester {
  "Anytime MetropolisHastings" should {
    "for a constant produce the constant with probability 1" in {
      Universe.createNew()
      val c = Constant(1)
      // we need to introduce a stochastic element to avoid calling select on empty set
      val f = Flip(0.3)
      anytimeMHTest(c, (i: Int) => i == 1, 1.0, 0.0001)
    }

    "for a uniform produce an interval with probability equal to the fraction of the range" in {
      Universe.createNew()
      val u = Uniform(0.2, 1.0)
      anytimeMHTest(u, (d: Double) => 0.3 <= d && d < 0.5, 0.25, 0.01)
    }
    "for a simple flip produce true with the given probability" in {
      Universe.createNew()
      val f = Flip(0.3)
      anytimeMHTest(f, (b: Boolean) => b, 0.3, 0.01)
    }

    "for a complex flip produce true with probability equal to the expectation over its argument" in {
      Universe.createNew()
      val f = Flip(Uniform(0.2, 1.0))
      anytimeMHTest(f, (b: Boolean) => b, 0.6, 0.01)
    }

    "for a simple select produce an outcome with the given probability" in {
      Universe.createNew()
      val s = Select(0.2 -> 1, 0.3 -> 2, 0.5 -> 3)
      anytimeMHTest(s, (i: Int) => i > 1, 0.8, 0.01)
    }

    "for a complex select produce an outcome with the expecation over the arguments" in {
      Universe.createNew()
      val s = Select(Select(0.4 -> 0.2, 0.6 -> 0.8) -> 1, Constant(0.4) -> 2)
      anytimeMHTest(s, (i: Int) => i == 1, 0.4 * 1.0 / 3 + 0.6 * 2.0 / 3, 0.01)
    }

    "for a simple dist produce an outcome with the probability of choosing the branch times " +
      "the probability the branch produces the outcome" in {
        Universe.createNew()
        val d = Dist(0.2 -> Constant(1), 0.8 -> Select(0.3 -> 1, 0.7 -> 2))
        anytimeMHTest(d, (i: Int) => i == 1, 0.2 + 0.8 * 0.3, 0.01)
      }

    "for a complex dist produce an outcome with the expectation of choosing the branch times " +
      "the probability the branch produces the outcome" in {
        Universe.createNew()
        val d = Dist(Select(0.4 -> 0.2, 0.6 -> 0.8) -> Constant(1), Constant(0.4) -> Select(0.3 -> 1, 0.7 -> 2))
        anytimeMHTest(d, (i: Int) => i == 1, 0.4 * (1.0 / 3 + 2.0 / 3 * 0.3) + 0.6 * (2.0 / 3 + 1.0 / 3 * 0.3), 0.01)
      }

    "for an apply produce an outcome with the probability of its inverse image" in {
      Universe.createNew()
      val a = Apply(Select(0.2 -> 1, 0.3 -> 2, 0.5 -> 3), (i: Int) => i % 2 == 0)
      anytimeMHTest(a, (b: Boolean) => !b, 0.7, 0.01)
    }

    "for an Inject produce an outcome with the probability of its inverse image" in {
      Universe.createNew()
      val i = Inject(Select(0.2 -> 1, 0.3 -> 2, 0.5 -> 3), Constant(4))
      anytimeMHTest(i, (xs: Seq[Int]) => xs.length == 2 && xs(0) == 1 && xs(1) == 4, 0.2, 0.01)
    }

    "for a caching chain produce an outcome with the expectation over the parent " +
      "of the probability of choosing the outcome" in {
        Universe.createNew()
        val c = CachingChain(Flip(0.6), (b: Boolean) => if (b) Select(0.2 -> 1, 0.8 -> 2); else Constant(1))
        anytimeMHTest(c, (i: Int) => i == 1, 0.6 * 0.2 + 0.4, 0.01)
      }

    "for an uncaching chain produce an outcome with the expectation over the parent " +
      "of the probability of choosing the outcome" in {
        Universe.createNew()
        val c = NonCachingChain(Uniform(0.2, 1.0), (d: Double) => Flip(d))
        anytimeMHTest(c, (b: Boolean) => b, 0.6, 0.04)
      }

    //Explanation:
    //The 'anytimeMHTest' method takes a element, an outcome and the correct probability of that outcome 
    //from the given element. In the multichain below, '1' is selected with probability .20 when either B1 or B2 is true, 
    //or with probability 1 when both B1 and B2 are false. So in total, the probability of selecting '1' isP(B1, -B2) + P(-B1,B2) + P(B1,B2))*0.20 + P(-B1,-B2).
    //-Michael

    "for a caching two parent chain produce an outcome with the expectation over the parent " +
      "of the probability of choosing the outcome" in {
        Universe.createNew()
        val c = CachingChain(Flip(0.6), Flip(0.5), (b1: Boolean, b2: Boolean) => if (b1 || b2) Select(0.2 -> 1, 0.8 -> 2); else Constant(1))
        anytimeMHTest(c, (i: Int) => i == 1, (0.6 * 0.5 + 0.6 * 0.5 + 0.4 * 0.5) * 0.2 + 0.4 * 0.5, 0.01)
      }

    //The expected value for the uniform distribution (lower, upper) is (upper + lower)/2.
    //So (1.0 + 0.2)/2 = 0.6, and (0.8 - 0.0)/2 = 0.4, and (d1 + d2)/2 = 0.5.
    "for an uncaching two parent chain produce an outcome with the expectation over the parent " +
      "of the probability of choosing the outcome" in {
        Universe.createNew()
        val c = NonCachingChain(Uniform(0.2, 1.0), Uniform(0.0, 0.8), (d1: Double, d2: Double) => Flip((d1 + d2) / 2))
        anytimeMHTest(c, (b: Boolean) => b, 0.5, 0.04)
      }

    "with an element that is used multiple times use the same value each time" in {
      Universe.createNew()
      val f = Flip(0.7)
      val e = f === f
      anytimeMHTest(e, (b: Boolean) => b, 1.0, 0.00001)
    }

    "with an element shared between the parent of a chain and its result use the same value each time" in {
      Universe.createNew()
      val f = Uniform(0.2, 1.0)
      val c = Chain(f, (d: Double) => if (d < 0.4) f; else Constant(0.8))
      //The condition d > 0.3 means that we are testing whether or not the value of c is greater than 0.3.
      //From the definition of c above, there are are two ways that this can happen
      //A) If (d < 0.4), c goes to 'f'. This happens with probability 0.25. Then, if d < 0.4, then p(f > 0.3) = 0.5.
      //B) If (d > 0.4), c goes to Constant(0.8). This happens with probability 0.75. Then, with probability 1, c is greater than 0.3.
      anytimeMHTest(c, (d: Double) => ((d > 0.3) == true), 0.25 * 0.5 + 0.75 * 1, 0.01) // if d < 0.4, P(f < 0.3) = 0.5
    }

    "with an element shared between the parents of a two parent chain and its result use the same value each time" in {
      Universe.createNew()
      val f1 = Uniform(0.2, 1.0)
      val f2 = Uniform(0.2, 1.0)
      val c = Chain(f1, f2, (d1: Double, d2: Double) => if (d1 < 0.4 && d2 < 0.6) f2; else Constant(0.8))
      //This time
      //(True and True)*(E[(f2, given f2 <0.6)] + [P(false, true) + P(true, false) + P(false, false) ]* 1
      anytimeMHTest(c, (d: Double) => d > 0.3, (0.25 * 0.5) * 0.75 + (0.75 * 0.5 + .25 * .5 + .75 * .5) * 1, 0.01)
    }

    "with a constraint on an element factor in the constraint" in {
      Universe.createNew()
      val f = Flip(0.4)
      f.setConstraint((b: Boolean) => if (b) 1.0; else 2.0)
      anytimeMHTest(f, (b: Boolean) => b, 0.4 / (0.4 + 0.6 * 2), 0.01)
    }

    "with a constraint on a dependent element factor in the constraint" in {
      Universe.createNew()
      val f = Flip(0.4)
      val a = If(f, 1, 2)
      a.setConstraint((i: Int) => if (i == 1) 1.0; else 2.0)
      anytimeMHTest(f, (b: Boolean) => b, 0.4 / (0.4 + 0.6 * 2), 0.01)
    }

    "with a condition on an element factor in the condition" in {
      Universe.createNew()
      val f = Flip(0.4)
      f.setCondition((b: Boolean) => b)
      anytimeMHTest(f, (b: Boolean) => b, 1.0, 0.01)
    }

    "with a condition on a dependent element factor in the condition" in {
      Universe.createNew()
      val f = Flip(0.4)
      val a = If(f, 1, 2)
      a.setCondition((i: Int) => i == 1)
      anytimeMHTest(f, (b: Boolean) => b, 1.0, 0.01)
    }
  }

  "OneTime MetropolisHastings" should {
    "with a condition on the result of a caching chain should condition the argument" in {
      Universe.createNew()
      val f = Flip(0.3)
      val c = CachingChain(f, (b: Boolean) => if (b) Flip(0.8); else Flip(0.4))
      c.setCondition((b: Boolean) => b)
      oneTimeMHTest(f, (b: Boolean) => b, 0.3 * 0.8 / (0.3 * 0.8 + 0.7 * 0.4), 200000)
    }

    "with a condition on the result of a caching chain with shared models should condition the argument" in {
      Universe.createNew()
      val f = Flip(0.3)
      val c = CachingChain(f, (b: Boolean) => if (b) f; else Flip(0.4))
      c.setCondition((b: Boolean) => b) //The condition states that 'b' must be true.
      oneTimeMHTest(f, (b: Boolean) => b, 0.3 * 1.0 / (0.3 * 1.0 + 0.7 * 0.4), 200000)
    }

    //I am not sure how to test a multi-chain in this scenario.
    "with a condition on the result of a two parent caching chain should condition the argument" in {
      Universe.createNew()
      val f1 = Flip(0.3)
      val f2 = Flip(0.4)
      val c = CachingChain(f1, f2, (b1: Boolean, b2: Boolean) => if ((b1 == true) || (b2 == false)) Flip(0.8); else Flip(0.4))
      c.setCondition((b: Boolean) => (b == true))
      //(B1 == T,B2 == F), (B1 == T, B2 == T), 
      oneTimeMHTest(f1, (b: Boolean) => b, ((0.3 * 0.8 * 0.6) + (0.3 * 0.8 * 0.4)) / ((0.3 * 0.8 * 0.6) + (0.3 * 0.8 * 0.4) + (0.7 * 0.8 * 0.6) + (0.7 * 0.4 * 0.4)), 200000)
    }

    "with a condition on the result of a two parent caching chain with shared models should condition the argument" in {
      Universe.createNew()
      val f1 = Flip(0.3)
      val f2 = Flip(0.4)
      val c = CachingChain(f1, f2, (b1: Boolean, b2: Boolean) => if ((b1 == true) || (b2 == false)) f1; else Flip(0.4))
      c.setCondition((b: Boolean) => (b == true))
      //[ (T,F) + (T,T) + (F,F) ] / [ (T,F) + (F,F) + (T,T) + (F,T) ]
      oneTimeMHTest(f1, (b: Boolean) => (b == true),
        ((0.3 * 1.0 * 0.6) + (0.3 * 1.0 * 0.4)) /
          ((0.3 * 1.0 * 0.6) + (0.3 * 1.0 * 0.4) + (0.7 * 0.4 * 0.4)),
        200000)
    }

    "with two conditions produce the correct result" in {
      val numSamples = 200000
      val tolerance = 0.01
      Universe.createNew()
      val f1 = Flip(0.3)
      val f2 = Flip(0.8)
      val f3 = Flip(0.4)
      f3.setCondition((b: Boolean) => b)
      val c = CachingChain(f1, (b: Boolean) => if (b) f2; else f3)
      val f4 = Flip(0.6)
      val eq = c === f4
      eq.setCondition((b: Boolean) => b)
      val p1 = 0.3 * (0.6 * 0.8 + 0.4 * 0.2)
      val p2 = 0.7 * (0.6 * 1 + 0.4 * 0)
      /*
       * The default proposal scheme does not work here. If it gets to the state:
       * f1: T, f2: T, f3: F, f4: F, it cannot change state in a way that satisfies both conditions by a single
       * proposal.
       */
      val u = Universe.universe
      val scheme = UntypedScheme(() => u.randomStochasticElement(), Some(FinalScheme(() => f4)))
      val mh = MetropolisHastings(numSamples, scheme, f1)
      try {
        mh.start()
        mh.stop()
        mh.probability(f1, (b: Boolean) => b) should be(p1 / (p1 + p2) +- tolerance)
      } finally {
        mh.kill()
      }
    }

    "with a condition on the result of an If in which both consequences are random, correctly condition the test" taggedAs(NonDeterministic) in {
      val numSamples = 100000
      val tolerance = 0.01
      Universe.createNew()
      val elem1 = Flip(0.2)
      val elem2 = If(elem1, Flip(0.7), Flip(0.4))
      elem2.observe(true)
      val alg = MetropolisHastings(numSamples, ProposalScheme.default, elem1)
      try {
        alg.start()
        alg.stop()
        val p1 = 0.2 * 0.7
        val p2 = 0.8 * 0.4
        alg.probability(elem1, (b: Boolean) => b) should be(p1 / (p1 + p2) +- tolerance)
      } finally {
        alg.kill()
      }
    }

    "with an observation on an element correctly set the value of dependent elements" in {
      val numSamples = 10000
      val tolerance = 0.01
      Universe.createNew()
      val elem1 = Uniform(0.0, 1.0)
      val elem2 = Apply(elem1, (d: Double) => d + 1)
      elem1.observe(0.5)
      val alg = MetropolisHastings(numSamples, ProposalScheme.default, elem2)
      try {
        alg.start()
        println(elem1.value)
        println(elem2.value)
        alg.stop()
        alg.probability(elem2, (d: Double) => 1.4 < d && d < 1.6) should be(1.0 +- tolerance)
      } finally {
        alg.kill()
      }
    }

    "not suffer from memory leaks" taggedAs (Performance) in {
      Universe.createNew()
      val c = NonCachingChain(Uniform(0.2, 1.0), (d: Double) => Constant(d))
      val mh = MetropolisHastings(1000000, ProposalScheme.default, c)
      val time1 = System.nanoTime
      mh.start()
      val time2 = System.nanoTime
      println(((time2 - time1).toDouble / 1000000000))
    }

    "not suffer from memory leaks with chain2" taggedAs (Performance) in {
      Universe.createNew()
      val c = NonCachingChain(Uniform(0.2, 1.0), Uniform(0.1, 0.9), (d1: Double, d2: Double) => Constant(d1 + d2))
      val mh = MetropolisHastings(1000000, ProposalScheme.default, c)
      val time1 = System.nanoTime
      mh.start()
      val time2 = System.nanoTime
      println(((time2 - time1).toDouble / 1000000000))
    }

    "produce the correct answer each time when run twice with different conditions" in {
      Universe.createNew()
      val s = Select(0.5 -> 0.3, 0.5 -> 0.6)
      val f = Flip(s)
      val i = MetropolisHastings(20000, ProposalScheme.default, f)
      s.observe(0.3)
      i.start()
      i.probability(f, true) should be(0.3 +- 0.01)
      i.kill()
      s.observe(0.6)
      i.start()
      i.probability(f, true) should be(0.6 +- 0.01)
      i.kill()
    }
  }

  "Testing a Metropolis-Hastings algorithm" should {
    "produce values satisfying a predicate the correct amount of the time without conditions or constraints" in {
      Universe.createNew()
      val s1 = Select(0.4 -> 1, 0.6 -> 4)
      val s2 = Select(0.2 -> 2, 0.8 -> 3)
      val p1 = Predicate(s1, (i: Int) => i == 4)
      val alg = MetropolisHastings(ProposalScheme.default)
      s1.value = 1
      val (_, scores, _) = alg.test(10000, List(p1), List())
      // probability of s1 changing to 4 is 0.5 (prob. s1 proposed) * 0.6 (prob 4 is chosen if s1 proposed)
      scores(p1) should be(0.3 +- 0.01)
    }

    "produce values satisfying a predicate the correct amount of the time with constraints" in {
      Universe.createNew()
      val s1 = Select(0.4 -> 1, 0.6 -> 4)
      val s2 = Select(0.2 -> 2, 0.8 -> 3)
      s1.setConstraint((i: Int) => if (i == 1) 1; else 0.25)
      val p1 = Predicate(s1, (i: Int) => i == 4)
      val alg = MetropolisHastings(ProposalScheme.default)
      s1.value = 1
      val (_, scores, _) = alg.test(10000, List(p1), List())
      // probability of s1 changing to 4 is 0.5 (prob. s1 proposed) * 0.6 (prob 4 is chosen if s1 proposed) * 0.25
      scores(p1) should be(0.075 +- 0.01)
    }

    "returns the percentage of time an element is proposed" in {
      Universe.createNew()
      val s1 = Select(0.4 -> 1, 0.6 -> 4)
      val s2 = Select(0.2 -> 2, 0.8 -> 3)
      s1.setConstraint((i: Int) => if (i == 1) 1; else 0.25)
      val alg = MetropolisHastings(ProposalScheme.default)
      val (_, _, counts) = alg.test(10000, List(), List(s1, s2))
      counts(s1) should be(0.5 +- 0.01)
      counts(s2) should be(0.5 +- 0.01)
    }
  }

  "Fixed bugs" should {
    "propose temporary elements when non-temporary elements are non-stochastic" taggedAs(NonDeterministic) in {
      Universe.createNew()
      val c1 = NonCachingChain(Constant(0), (i: Int) => Flip(0.7))
      val a1 = If(c1, Constant(1), Constant(0))
      Universe.universe.clearTemporaries()
      val alg = MetropolisHastings(10000, ProposalScheme.default, a1)
      alg.start()
      alg.probability(a1, 1) should be(0.7 +- 0.01)
    }
  }

  def newState(mh: MetropolisHastings): State = {
    val cls = mh.getClass().getSuperclass()
    val method = cls.getDeclaredMethod("newState")
    method.setAccessible(true)
    method.invoke(mh).asInstanceOf[State]
  }

  def getDissatisfied(mh: MetropolisHastings): Set[Element[_]] = {
    val cls = mh.getClass().getSuperclass()
    val method = cls.getDeclaredMethod("getDissatisfied")
    method.setAccessible(true)
    method.invoke(mh).asInstanceOf[Set[Element[_]]]
  }

  def propose(mh: MetropolisHastings, state: State, elem: Element[_]): State = {
    val cls = mh.getClass().getSuperclass()
    val method = cls.getDeclaredMethod("propose", classOf[State], classOf[Element[_]])
    method.setAccessible(true)
    method.invoke(mh, state, elem).asInstanceOf[State]
  }

  def runScheme(mh: MetropolisHastings): State = {
    val cls = mh.getClass().getSuperclass()
    val method = cls.getDeclaredMethod("runScheme")
    method.setAccessible(true)
    method.invoke(mh).asInstanceOf[State]
  }

  def updateOne(mh: MetropolisHastings, state: State, elem: Element[_]): State = {
    val cls = mh.getClass().getSuperclass()
    val method = cls.getDeclaredMethod("updateOne", classOf[State], classOf[Element[_]])
    method.setAccessible(true)
    method.invoke(mh, state, elem).asInstanceOf[State]
  }

  def update(mh: MetropolisHastings, state: State, elem: Element[_]): State = {
    val cls = mh.getClass().getSuperclass()
    val method = cls.getDeclaredMethod("update", classOf[State], classOf[Element[_]])
    method.setAccessible(true)
    method.invoke(mh, state, elem).asInstanceOf[State]
  }

  def proposeAndUpdate(mh: MetropolisHastings): State = {
    val cls = mh.getClass().getSuperclass()
    val method = cls.getDeclaredMethod("proposeAndUpdate")
    method.setAccessible(true)
    method.invoke(mh).asInstanceOf[State]
  }

  def accept(mh: MetropolisHastings, state: State): Unit = {
    val cls = mh.getClass().getSuperclass()
    val method = cls.getDeclaredMethod("accept", classOf[State])
    method.setAccessible(true)
    method.invoke(mh, state).asInstanceOf[Unit]
  }

  def undo(mh: MetropolisHastings, state: State): Unit = {
    val cls = mh.getClass().getSuperclass()
    val method = cls.getDeclaredMethod("undo", classOf[State])
    method.setAccessible(true)
    method.invoke(mh, state).asInstanceOf[Unit]
  }

  def decideToAccept(mh: MetropolisHastings, state: State): Boolean = {
    val cls = mh.getClass().getSuperclass()
    val method = cls.getDeclaredMethod("decideToAccept", classOf[State])
    method.setAccessible(true)
    method.invoke(mh, state).asInstanceOf[Boolean]
  }

  def anytimeMHTest[T](target: Element[T], predicate: T => Boolean, prob: Double, tolerance: Double): Unit = {
    val mh = MetropolisHastings(ProposalScheme.default, target)
    try {
      mh.start()
      Thread.sleep(1000)
      mh.stop()
      println(mh.getSampleCount.toString + " samples")
      mh.probability(target, predicate) should be(prob +- tolerance)
    } finally {
      mh.kill()
    }
  }

  def oneTimeMHTest[T](target: Element[T], predicate: T => Boolean, prob: Double, numSamples: Int): Unit = {
    val tolerance = 0.01
    val mh = MetropolisHastings(numSamples, ProposalScheme.default, target)
    try {
      mh.start()
      mh.stop()
      mh.probability(target, predicate) should be(prob +- tolerance)
    } finally {
      mh.kill()
    }
  }
}
