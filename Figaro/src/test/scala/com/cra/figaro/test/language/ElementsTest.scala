/*
 * ElementsTest.scala  
 * Tests of basic functionality of Figaro elements.
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Jan 1, 2009
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.test.language

import org.scalatest.Matchers
import org.scalatest.WordSpec
import com.cra.figaro.language._
import com.cra.figaro.library.atomic.continuous._
import com.cra.figaro.library.compound._
import com.cra.figaro.library.atomic.discrete.Uniform
import scala.math.log

class ElementsTest extends WordSpec with Matchers {
  "A Constant" should {
    "have value equal to the given constant" in {
      Universe.createNew()
      val e = Constant(5)
      e.generate()
      e.value should equal(5)
    }

    "convert to the correct string" in {
      Universe.createNew()
      Constant(5).toString should equal("Constant(5)")
    }
  }

  "A Flip with constant weight" should {
    "have value true with probability equal to the weight" in {
      Universe.createNew()
      sampleTest(Flip(0.7), (b: Boolean) => b == true, 0.7)
    }

    "convert to the correct string" in {
      Universe.createNew()
      Flip(0.7).toString should equal("Flip(0.7)")
    }
  }

  "A Flip with a random weight" should {
    "have value true with probability equal to the expectation of the weight" in {
      Universe.createNew()
      val u = Uniform(0.0, 0.5)
      val f = Flip(u)
      u.value = 0.25
      sampleTest(f, (b: Boolean) => b == true, 0.25)
    }

    "convert to the correct string" in {
      Universe.createNew()
      val u = Uniform(0.0, 0.5)
      Flip(u).toString should equal("Flip(" + u + ")")
    }
  }

  "A Chain" when {
    "called" should {
      "have value equal to the value of its function applied to its argument's value" in {
        Universe.createNew()
        val f1 = Flip(0.7)
        val f2 = Flip(0.4)
        val f3 = Flip(0.9)
        val c = NonCachingChain(f1, (b: Boolean) => if (b) f2; else f3)
        f1.value = false
        f2.value = true
        f3.value = false
        c.generate()
        c.value should equal(false)
        f1.value = true
        c.generate()
        c.value should equal(true)
      }

      "convert to the correct string" in {
        Universe.createNew()
        val f1 = Flip(0.7)
        val f2 = Flip(0.4)
        val f3 = Flip(0.9)
        val fn = (b: Boolean) => if (b) f2; else f3
        NonCachingChain(f1, fn).toString should equal("Chain(" + f1 + ", " + fn + ")")
      }

      // No more caching, removed
      /*
      "return a cached result" in {
        var sum = 0
        def fn(b: Boolean) = {
          sum += 1
          Constant(b)
        }
        val c = CachingChain(Flip(0.5), fn)
        c.get(true)
        c.get(false)
        c.get(true)
        sum should equal(2)
      }
      * 
      */

      // No more caching, removed
      /*
      "call the CPD when the cache is full" in {
        Universe.createNew()
        var sum = 0
        def fn(b: Int) = {
          sum += 1
          Constant(b)
        }
        val f1 = Uniform(0, 1, 2)
        val c = NonCachingChain(f1, fn _)
        sum = 0
        c.get(0)
        c.get(1)
        c.get(2)
        c.get(0)
        // either 3 or 4 depending on whether the value on initialization is true or false
        // see implementation of NonCachingChain and use of oldParentValue
        sum should be > (3)
      }
      * 
      */
      "call the CPD for each Chain access" in {
        Universe.createNew()
        var sum = 0
        def fn(b: Int) = {
          sum += 1
          Constant(b)
        }
        val f1 = Uniform(0, 1, 2)
        val c = NonCachingChain(f1, fn _)
        sum = 0
        c.get(0)
        c.get(1)
        c.get(2)
        c.get(0)
        sum should equal(4)
      }
    }

    // No more local context of chains
    /*
    "managing the context" should {      
      "store new elements in the correct subContext" in {
        Universe.createNew()
        val c = Chain(Flip(0.5), (b: Boolean) => if (b) Constant(0) else Constant(1))
        c.get(true)
        c.get(false)
        c.myMappedContextContents(true).size should equal(1)
        c.elemInContext(c.myMappedContextContents(true).head) should equal(true)
        c.myMappedContextContents(false).size should equal(1)
        c.elemInContext(c.myMappedContextContents(false).head) should equal(false)
      }
     
      "remove deactivated elements from context when resizing the cache" in {
        Universe.createNew()
        val c = NonCachingChain(Uniform(0, 1, 2), (b: Int) => Constant(b))
        c.get(0)
        c.get(1)
        c.get(2)
        c.directContextContents.size should equal(2)
        c.elemInContext.size should equal(2)
      }
    
      "remove deactivated elements from context when removing temporaries" in {
        Universe.createNew()
        val c = CachingChain(com.cra.figaro.library.atomic.discrete.Uniform(0, 10), (b: Int) => Constant(b))
        c.get(0)
        c.get(1)
        c.get(2)
        c.clearContext
        c.directContextContents.size should equal(1)
        c.elemInContext.size should equal(1)
      }
    
      "only remove elements defined in subContext" in {
        Universe.createNew()
        def fcn(b: Int) = {
          val t = Constant(b)
          Chain(t, (i: Int) => Constant(i))
        }
        val u = com.cra.figaro.library.atomic.discrete.Uniform(0, 10)
        u.set(0)
        val c = new Chain("", com.cra.figaro.library.atomic.discrete.Uniform(0, 10), (b: Int) => fcn(b), 3, Universe.universe)
        c.get(0)
        c.get(1)
        c.get(2)
        val lastE = c.cache.last
        val elemsInContext = Universe.universe.uses(lastE._2) + lastE._2
        c.get(3)
        elemsInContext forall (!_.active) should equal(true)
        Universe.universe.contextContents(c) forall (_.active) should equal(true)
      }
    }
    * 
    */
  }

  "A chain with two parents" when {
    "non-caching" should {
      "have value equal to the value of its function applied to its argument's values" in {
        Universe.createNew()
        val f1 = Flip(0.4)
        val f2 = Flip(0.7)
        val f3 = Flip(0.9)
        val f4 = Flip(0.235)
        val c = NonCachingChain(f1, f2, (b1: Boolean, b2: Boolean) => if (b1 == true && b2 == true) f3; else f4)
        f1.set(false)
        f2.set(true)
        f3.set(false)
        f4.set(true)
        c.generate()
        c.value should equal(true)

        f1.set(true)
        c.generate()
        c.value should equal(false)

      }

      "convert to the correct string" in {
        Universe.createNew()
        val f1 = Flip(0.7)
        val f2 = Flip(0.4)
        val f3 = Flip(0.9)
        val f4 = Flip(0.235)
        val fn = (b1: Boolean, b2: Boolean) => if (b1 && b2) f3; else f4
        NonCachingChain(f1, f2, fn).toString should equal("Chain(Apply(" + f1 + ", " + f2 + ", " + fn + "), <function1>)")
      }

      "evaluate the CPD each time get is called" in {
        Universe.createNew()
        var sum = 0
        def fn(b1: Boolean, b2: Boolean): Element[Boolean] = {
          sum += 1
          Constant(b1 && b2)
        }
        val f1 = Flip(0.5)
        val f2 = Flip(0.5)
        f1.set(true)
        f2.set(false)
        val c = Chain(f1, f2, fn)        
        c.get(true, true)
        c.get(false, false)
        c.get(true, true)
        sum should equal(3)
      }
    }

    "caching" should {
      "have value equal to the value of its function applied to its argument's value" in {
        Universe.createNew()
        val f1 = Flip(0.7)
        val f2 = Flip(0.4)
        val f3 = Flip(0.9)
        val f4 = Flip(0.235)
        f1.set(false)
        f2.set(true)
        f3.set(false)
        f4.set(true)
        val c = CachingChain(f1, f2, (b1: Boolean, b2: Boolean) => if (b1 == true && b2 == true) f3; else f4)

        c.generate()
        c.value should equal(true)
        f1.set(true)
        c.generate()
        c.value should equal(false)
      }

      "convert to the correct string" in {
        Universe.createNew()
        val f1 = Flip(0.7)
        val f2 = Flip(0.4)
        val f3 = Flip(0.9)
        val f4 = Flip(0.235)
        val fn = (b1: Boolean, b2: Boolean) => if (b1 && b2) f3; else f4
        //      "Chain(Apply(Flip(0.7), Flip(0.4), <function2>), <function1>)"

        CachingChain(f1, f2, fn).toString should equal("Chain(Apply(" + f1 + ", " + f2 + ", " + fn + "), <function1>)")
      }
      //((((((((((80 * .50) * 1.08) + 82.4 * .50) * 1.08) + 84.8 * .50) * 1.08) + 87.3 * .50) * 1.08) + 89.9 * .50) * 1.08)

    }
  }

  "An Apply with one argument" should {
    "have value equal to its function applied to its argument" in {
      Universe.createNew()
      val u = Uniform(0.0, 2.0)
      val a = Apply(u, (d: Double) => d + 1.0)
      u.value = 1.3
      a.generate()
      a.value should equal(2.3)
    }

    "convert to the correct string" in {
      Universe.createNew()
      val u = Uniform(0.0, 2.0)
      val f = (d: Double) => d + 1.0
      Apply(u, f).toString should equal("Apply(" + u + ", " + f + ")")
    }
  }

  "An Apply with two arguments" should {
    "have value equal to its function applied to its arguments" in {
      Universe.createNew()
      val u = Uniform(0.0, 2.0)
      val v = Constant(1.0)
      val a = Apply(u, v, (d1: Double, d2: Double) => d1 + d2 + 1.0)
      u.value = 1.3
      v.value = 1.0
      a.generate()
      a.value should equal(3.3)
    }

    "convert to the correct string" in {
      val u = Uniform(0.0, 2.0)
      val v = Constant(1.0)
      val f = (d1: Double, d2: Double) => d1 + d2 + 1.0
      Apply(u, v, f).toString should equal("Apply(" + u + ", " + v + ", " + f + ")")
    }
  }

  "An Apply with three arguments" should {
    "have value equal to its function applied to its arguments" in {
      Universe.createNew()
      val u = Uniform(0.0, 2.0)
      val v = Constant(1.0)
      val w = Select(0.5 -> 0.0, 0.5 -> 5.0)
      val a = Apply(u, v, w, (d1: Double, d2: Double, d3: Double) => d1 + d2 + d3 + 1.0)
      u.value = 1.3
      v.value = 1.0
      w.value = 5.0
      a.generate()
      a.value should equal(8.3)
    }

    "convert to the correct string" in {
      val u = Uniform(0.0, 2.0)
      val v = Constant(1.0)
      val w = Select(0.5 -> 0.0, 0.5 -> 5.0)
      val f = (d1: Double, d2: Double, d3: Double) => d1 + d2 + d3 + 1.0
      Apply(u, v, w, f).toString should equal(
        "Apply(" + u + ", " + v + ", " + w + ", " + f + ")")
    }
  }

  "An Apply with four arguments" should {
    "have value equal to its function applied to its arguments" in {
      Universe.createNew()
      val u = Uniform(0.0, 2.0)
      val v = Constant(1.0)
      val w = Select(0.5 -> 0.0, 0.5 -> 5.0)
      val x = Constant(-2.0)
      val a = Apply(u, v, w, x, (d1: Double, d2: Double, d3: Double, d4: Double) => d1 + d2 + d3 + d4 + 1.0)
      u.value = 1.3
      v.value = 1.0
      w.value = 5.0
      x.value = -2.0
      a.generate()
      a.value should equal(6.3)
    }

    "convert to the correct string" in {
      val u = Uniform(0.0, 2.0)
      val v = Constant(1.0)
      val w = Select(0.5 -> 0.0, 0.5 -> 5.0)
      val x = Constant(-2.0)
      val f = (d1: Double, d2: Double, d3: Double, d4: Double) => d1 + d2 + d3 + d4 + 1.0
      Apply(u, v, w, x, f).toString should equal(
        "Apply(" + u + ", " + v + ", " + w + ", " + x + ", " + f + ")")
    }
  }

  "An Apply with five arguments" should {
    "have value equal to its function applied to its arguments" in {
      Universe.createNew()
      val u = Uniform(0.0, 2.0)
      val v = Constant(1.0)
      val w = Select(0.5 -> 0.0, 0.5 -> 5.0)
      val x = Constant(-2.0)
      val y = Constant(0.5)
      val a =
        Apply(u, v, w, x, y, (d1: Double, d2: Double, d3: Double, d4: Double, d5: Double) =>
          d1 + d2 + d3 + d4 + d5 + 1.0)
      u.value = 1.3
      v.value = 1.0
      w.value = 5.0
      x.value = -2.0
      y.value = 0.5
      a.generate()
      a.value should equal(6.8)
    }

    "convert to the correct string" in {
      val u = Uniform(0.0, 2.0)
      val v = Constant(1.0)
      val w = Select(0.5 -> 0.0, 0.5 -> 5.0)
      val x = Constant(-2.0)
      val y = Constant(0.5)
      val f =
        (d1: Double, d2: Double, d3: Double, d4: Double, d5: Double) => d1 + d2 + d3 + d4 + d5 + 1.0
      Apply(u, v, w, x, y, f).toString should equal(
        "Apply(" + u + ", " + v + ", " + w + ", " + x + ", " + y + ", " + f + ")")
    }
  }

  "An Inject" should {
    "have value equal to the sequence of values of its arguments" in {
      Universe.createNew()
      val u1 = Uniform(0.0, 2.0)
      val u2 = Constant(1.5)
      val i = Inject(u1, u2)
      u1.value = 1.3
      u2.value = 1.5
      i.generate()
      i.value.toList should equal(List(1.3, 1.5))
    }

    "convert to the correct string" in {
      Universe.createNew()
      val u1 = Uniform(0.0, 2.0)
      val u2 = Constant(1.5)
      val i = Inject(u1, u2)
      Inject(u1, u2).toString should equal("Inject(" + u1 + ", " + u2 + ")")
    }
  }

  "A Select with constant probabilities" should {
    "have a value with its associated probability" in {
      Universe.createNew()
      sampleTest(Select(0.2 -> 1, 0.3 -> 2, 0.5 -> 3), (i: Int) => i > 1, 0.8)
    }

    "convert to the correct string" in {
      Universe.createNew()
      val d = Select(0.2 -> 1, 0.3 -> 2, 0.5 -> 3)
      d.toString should equal("Select(0.2 -> 1, 0.3 -> 2, 0.5 -> 3)")
    }
  }

  "A Select with variable probabilities" should {
    "have a value with probability proportional to its associated probability element's value" in {
      Universe.createNew()
      val c1 = Constant(0.2)
      val c2 = Constant(0.4)
      val d = Select(c1 -> false, c2 -> true)
      sampleTest(d, (b: Boolean) => b, 0.4 / 0.6)
    }

    "convert to the correct string" in {
      Universe.createNew()
      val fn = (b: Boolean) => if (b) Constant(0.2); else Constant(0.8)
      val con = Constant(0.4)
      val chn = Chain(Flip(0.75), fn)
      val d = Select(con -> false, chn -> true)
      d.toString should equal("Select(" + con + " -> false, " + chn + " -> true)")
    }
  }

  "A Dist with constant probabilities" should {
    "have the value of an outcome with its associated probability" in {
      Universe.createNew()
      val f = Flip(0.6)
      val c = Constant(true)
      val d = Dist(0.3 -> f, 0.7 -> c)
      f.value = false
      sampleTest(d, (b: Boolean) => b, 0.7)
    }

    "convert to the correct string" in {
      Universe.createNew()
      val fl = Flip(0.6)
      val con = Constant(true)
      val d = Dist(0.3 -> fl, 0.7 -> con)
      d.toString should equal("Dist(0.3 -> " + fl + ", 0.7 -> " + con + ")")
    }
  }

  "A Dist with variable probabilities" should {
    "have the value of an outcome with probability proportional to its associated probability element's value" in {
      Universe.createNew()
      val f2 = Flip(0.5)
      val c1 = Constant(0.2)
      val c2 = Constant(0.4)
      val c3 = Constant(true)
      val d = Dist(c1 -> f2, c2 -> c3)
      f2.value = false
      sampleTest(d, (b: Boolean) => b, 0.4 / 0.6)
    }

    "convert to the correct string" in {
      Universe.createNew()
      val fn = (b: Boolean) => if (b) Constant(0.2); else Constant(0.8)
      val con1 = Constant(0.4)
      val fl = Flip(0.5)
      val chn = Chain(Flip(0.75), fn)
      val con2 = Constant(true)
      val d = Dist(con1 -> fl, chn -> con2)
      d.toString should equal(
        "Dist(" + con1 + " -> " + fl + ", " + chn + " -> " + con2 + ")")
    }
  }

  "An element" when {
    "used as a monad with map" should {
      "have value equal to its function applied to its argument" in {
        Universe.createNew()
        val u = Uniform(0.0, 2.0)
        val a = u.map(_ + 1.0)
        u.value = 1.3
        a.generate()
        a.value should equal(2.3)
      }
    }

    "used as a monad with flatMap" should {
      "have value equal to the value of its function applied to its argument's value" in {
        Universe.createNew()
        val u = Uniform(0.0, 2.0)
        val a = u.flatMap(x => Constant(x + 2.0))
        u.value = 1.3
        a.generate()
        a.value should equal(3.3)
      }
    }

    "a condition is added" should {
      "have both the original conditions and the added condition" in {
        val universe = Universe.createNew()
        val s = Select(0.2 -> 2, 0.3 -> 3, 0.5 -> 4)
        s.addCondition((i: Int) => i % 2 == 0)
        s.addCondition((i: Int) => i > 2)
        assert(universe.conditionedElements.contains(s))
        s.condition(2) should equal(false)
        s.condition(3) should equal(false)
        s.condition(4) should equal(true)
      }
    }

    "a constraint is added" should {
      "have both the original constraints and the added constraint" in {
        val universe = Universe.createNew()
        val s = Select(0.2 -> 2, 0.3 -> 3, 0.5 -> 4)
        s.addConstraint((i: Int) => i.toDouble)
        s.addConstraint((i: Int) => i.toDouble)
        assert(universe.constrainedElements.contains(s))
        s.constraint(2) should equal(log(4.0))
        s.constraint(3) should equal(log(9.0))
        s.constraint(4) should equal(log(16.0))
      }
    }

    "a contingent condition is added" should {
      "have the condition be true whenever the contingency is false, and dependent on the value of the element when the contingency is true" in {
        Universe.createNew()
        val x = Flip(0.3)
        val y = Flip(0.6)
        val z = If(x, y, Constant(true))
        val contingency1 = List(Element.ElemVal(x, false))
        val contingency2 = List(Element.ElemVal(y, false))
        x.value = true
        y.value = false
        z.addCondition((b: Boolean) => b, contingency1)
        z.condition(true) should equal(true)
        z.condition(false) should equal(true)
        z.addCondition((b: Boolean) => b, contingency2)
        z.condition(true) should equal(true)
        z.condition(false) should equal(false)
      }
    }

    "a contingent constraint is added" should {
      "have the condition be true whenever the contingency is false, and dependent on the value of the element when the contingency is true" in {
        Universe.createNew()
        val x = Flip(0.3)
        val y = Flip(0.6)
        val z = If(x, y, Constant(true))
        val contingency1 = List(Element.ElemVal(x, false))
        val contingency2 = List(Element.ElemVal(y, false))
        x.value = true
        y.value = false
        z.addConstraint((b: Boolean) => if (b) 2.0; else 3.0, contingency1)
        z.constraint(true) should equal(math.log(1.0))
        z.constraint(false) should equal(math.log(1.0))
        z.addConstraint((b: Boolean) => if (b) 2.0; else 3.0, contingency2)
        z.constraint(true) should equal(math.log(2.0))
        z.constraint(false) should equal(math.log(3.0))
      }
    }
    
    "correctly set the observation" should {
      Universe.createNew()
      val x = Flip(0.3)
      x.observation should equal (None)
      x.addConstraint((b: Boolean) => if (b) 1.0; else 2.0)
      x.observation should equal (None)
      x.addCondition((b: Boolean) => b)
      x.observation should equal (None)
      x.observe(true)
      x.observation should equal (Some(true))
      x.addConstraint((b: Boolean) => if (b) 1.0; else 2.0)
      x.observation should equal (Some(true))
      x.addCondition((b: Boolean) => b)
      x.observation should equal (None)
    }
  }

  def sampleTest[T](targetElem: Element[T], predicate: T => Boolean, prob: Double) = {
    val numTrials = 100000
    val tolerance = 0.01
    var successes = 0
    for { i <- 1 to numTrials } {
      targetElem.generate()
      if (predicate(targetElem.value)) successes += 1
    }
    (successes.toDouble / numTrials) should be(prob +- tolerance)

  }
}
