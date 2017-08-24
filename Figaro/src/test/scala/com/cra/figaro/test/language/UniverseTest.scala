/*
 * UniverseTest.scala    
 * Universe test. 
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Jan 1, 2009
 * 
 * Copyright 2017 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.test.language

import org.scalatest.Matchers
import org.scalatest.WordSpec
import com.cra.figaro.language._
import com.cra.figaro.language.Universe._
import com.cra.figaro.library.atomic.continuous._
import com.cra.figaro.library.compound._
import scala.collection.mutable._
import scala.collection.mutable.Map
import com.cra.figaro.algorithm.factored.VariableElimination

class UniverseTest extends WordSpec with Matchers {
  "A Universe" when {
    "having activated and deactivated some elements" should {
      
     "activate parent elements" in {
        createNew()
        val e1 = Constant(8)
        val e2 = Constant(6)
        val e3 = Chain(e1,(i:Int) => e2)
        
        e1.deactivate()
        e3.deactivate()
        e3.activate()
        e1.active should equal(true)
      }
      
      "have all elements whose last activation was later than their last deactivation be active and no others" in {
        createNew()
        val e1 = Constant(8)
        val e2 = Constant(7)
        val e3 = Constant(6)
        e2.deactivate()
        e3.deactivate()
        e2.activate()
        assert(universe.activeElements contains e1)
        assert(universe.activeElements contains e2)
        universe.activeElements.size should equal(2)
      }

      "have all active stochastic elements be in the set of stochastic elements and no others" in {
        createNew()
        val e1 = Uniform(0.0, 1.0)
        val e2 = Select(0.3 -> 1, 0.5 -> 2)
        val e3 = Constant(4)
        val e4 = Flip(0.2)
        e2.deactivate()
        e4.deactivate()
        e2.activate()
        assert(universe.stochasticElements contains e1)
        assert(universe.stochasticElements contains e2)
        universe.stochasticElements.size should equal(2)
      }
    }

    "having pushed and popped some elements" should {
      "have the context stack contain elements in reverse order of their pushing if they are not popped" in {
        createNew()
        val e1 = Constant(8)
        universe.pushContext(e1)
        val e2 = Constant(7)
        universe.pushContext(e2)
        val e3 = Constant(6)
        universe.pushContext(e3)
        universe.contextStack should equal(List(e3, e2, e1))
      }

      "have the context stack not contain elements pushed after a popped element or the popped element itself" in {
        createNew()
        val e1 = Constant(8)
        universe.pushContext(e1)
        val e2 = Constant(7)
        universe.pushContext(e2)
        val e3 = Constant(6)
        universe.pushContext(e3)
        universe.popContext(e2)
        universe.contextStack should equal(List(e1))
      }

      "have the context be the context stack at time of activation" in {
        createNew()
        val e1 = Constant(8)
        universe.context(e1) should be('empty)
        universe.pushContext(e1)
        val e2 = Constant(7)
        universe.context(e2) should equal(List(e1))
      }

      "say that elements activated with an empty context stack are not temporary" in {
        createNew()
        val e = Constant(8)
        e.isTemporary should equal(false)
      }

      "say that elements activated with a non-empty context stack are temporary" in {
        createNew()
        val e1 = Constant(8)
        universe.pushContext(e1)
        val e2 = Constant(7)
        e2.isTemporary should equal(true)
      }

      "retrieve elements in context" in {
        Universe.createNew()
        val c = CachingChain(com.cra.figaro.library.atomic.discrete.Uniform(0, 10), (b: Int) => Constant(b))
        val e1 = c.get(0)
        val e2 = c.get(1)
        val e3 = c.get(2)
        Universe.universe.inContext(e1, c) should equal (true)
        Universe.universe.inContext(e2, c) should equal (true)
        Universe.universe.inContext(e3, c) should equal (true)
        val e4 = Constant(4)
        Universe.universe.inContext(e4, c) should equal (false)
      }
  
    }

    "calling contextContents on an element" should {
      "return all elements activated while the element was in the context stack" in {
        createNew()
        val e1 = Constant(8)
        universe.pushContext(e1)
        val e2 = Constant(7)
        universe.pushContext(e2)
        val e3 = Constant(6)
        val l = universe.contextContents(e1)
        assert(l contains e2)
        assert(l contains e3)
        l.size should equal(2)
      }
    }

    "having deactivated an element" should {
      "have the element be removed from the context stack" in {
        createNew()
        val e1 = Constant(8)
        universe.pushContext(e1)
        e1.deactivate()
        universe.contextStack should be('empty)
      }

      "have the element not be included in the Universe.universe.contextContents of another element" in {
        createNew()
        val e1 = Constant(8)
        universe.pushContext(e1)
        val e2 = Constant(7)
        e2.deactivate()
        universe.contextContents(e1) should be('empty)
      }

      "have all elements in its context contents be deactivated" in {
        createNew()
        val e1 = Constant(8)
        universe.pushContext(e1)
        val e2 = Constant(7)
        e1.deactivate()
        universe.activeElements should not contain (e2)
      }

      "have the element be removed from a registerd map" in {
        createNew()
        val e1 = Constant(8)
        universe.pushContext(e1)
        val e2 = Constant(7)
        universe.pushContext(e2)
        val m: Map[Element[_], String] = Map(e1 -> "a", e2 -> "b")
        universe.register(m)
        e2.deactivate()
        m should equal(Map(e1 -> "a"))
      }
    }

    "having cleared" should {
      "have no active elements" in {
        createNew()
        val e = Constant(8)
        createNew()
        universe.activeElements should be('empty)
      }

      "have empty context stack" in {
        createNew()
        val e = Constant(8)
        universe.pushContext(e)
        createNew()
        universe.contextStack should be('empty)
      }
    }

    "having cleared temporaries" should {
      "have empty context stack" in {
        createNew()
        val e = Constant(8)
        universe.pushContext(e)
        universe.clearTemporaries()
        universe.contextStack should be('empty)
      }

      "have temporary elements be inactive but permanent elements active" in {
        createNew()
        val e1 = Constant(8)
        universe.pushContext(e1)
        val e2 = Constant(7)
        universe.clearTemporaries()
        universe.activeElements should equal(List(e1))
      }

      "maintain consistent variable uses" in {
        val P1 = Flip(0.3)
        val P2 = Flip(0.4)
        val C1 = NonCachingChain(P1, P2, (b1: Boolean, b2: Boolean) => Constant(0))
        //val C1 = NonCachingChain(P1, (b1: Boolean) => Constant(0))
        val X1 = Apply(C1, (i: Int) => i)

        P1.generate
        P2.generate
        C1.generate
        X1.generate

        val usesBefore = Universe.universe.uses(X1).clone

        Universe.universe.clearTemporaries
        val usesAfter = Universe.universe.uses(X1)
        usesAfter.contains(P2) should equal(true)
        //usesBefore should equal (usesAfter)

      }

      "maintain existing usedBy if multiple paths to an element" in {
        createNew()
        val a1 = Flip(.2)
        val a2 = Flip(.6)
        val f1 = Flip(0.5)
        val c1 = CachingChain(f1, (b: Boolean) => if (b) If(a1, 0, 1) else If(a2, 3, 4))
        val x1 = Apply(a1, a2, c1, (i1: Boolean, i2: Boolean, i3: Int) => 0)

        f1.value = true
        c1.generate
        f1.value = false
        c1.generate

        Universe.universe.clearTemporaries
        val usedByAfter = Universe.universe.usedBy(a1)
        usedByAfter.contains(x1) should equal(true)
      }

    }

    "having cleared context" should {
      "have all elements in the context be deactivated and no others" in {
        createNew()
        val e1 = Constant(8)
        val e2 = Flip(0.5)
        val e3 = NonCachingChain(e2, (b: Boolean) => if (b) e1; else Constant(9))
        universe.clearContext(e3)
        universe.activeElements.size should equal(3)
        assert(universe.activeElements contains e1)
        assert(universe.activeElements contains e2)
        assert(universe.activeElements contains e3)
      }
    }

    "attempting to activate an active element" should {
      "throw IllegalArgumentException" in {
        createNew()
        val e = Constant(8)
        an [IllegalArgumentException] should be thrownBy { e.activate() } 
      }
    }

    "attempting to deactivate an inactive element" should {
      "throw IllegalArgumentException" in {
        createNew()
        val e = Constant(8)
        e.deactivate()
        an [IllegalArgumentException] should be thrownBy { e.deactivate() } 
      }
    }

    "attempting to get the context of a deactivated element" should {
      "throw NoSuchElementException" in {
        createNew()
        val e1 = Constant(8)
        e1.deactivate()
        an [NoSuchElementException] should be thrownBy { universe.context(e1) } 
      }
    }

    "attempting to get the context contents of a deactivated element" should {
      "throw NoSuchElementException" in {
        createNew()
        val e1 = Constant(8)
        e1.deactivate()
        an [NoSuchElementException] should be thrownBy  { universe.contextContents(e1) } 
      }
    }
  }

  "Calling Universe.usedBy on an element" should {
    "return all elements that use it recursively" in {
      createNew()
      val e1 = Uniform(0.0, 1.0)
      val e2 = Flip(e1)
      val e3 = If(e2, Flip(0.8), Flip(0.2))
      universe.usedBy(e1) should equal(Set(e2, e3))
    }
    "return a set that contains an element that uses it as an Apply1 argument" in {
      createNew()
      val f = Flip(0.5)
      val a = Apply(f, (b: Boolean) => b)
      universe.usedBy(f) should equal(Set(a))
      universe.usedBy(a) should equal(Set())
    }

    "return a set that contains an element that uses it as an Apply2 argument" in {
      Universe.createNew()
      val f1 = Flip(0.5)
      val f2 = Flip(0.6)
      val a = Apply(f1, f2, (b1: Boolean, b2: Boolean) => b1 && b2)
      universe.usedBy(f1) should equal(Set(a))
      universe.usedBy(f2) should equal(Set(a))
      universe.usedBy(a) should equal(Set())
    }

    "return a set that contains an element that uses it as an Apply3 argument" in {
      createNew()
      val f1 = Flip(0.5)
      val c2 = Constant(1)
      val c3 = Constant(2)
      val a = Apply(f1, c2, c3, (b1: Boolean, i2: Int, i3: Int) => if (b1) i2; else i3)
      universe.usedBy(f1) should equal(Set(a))
      universe.usedBy(c2) should equal(Set(a))
      universe.usedBy(c3) should equal(Set(a))
      universe.usedBy(a) should equal(Set())
    }

    "return a set that contains an element that uses it as a CachingChain argument" in {
      createNew()
      val f = Flip(0.5)
      def fn(b: Boolean) = if (b) Constant(1); else Constant(2)
      val c = CachingChain(f, fn)
      universe.usedBy(f) should equal(Set(c))
      universe.usedBy(c) should equal(Set())
    }

    "return a set that contains an element that uses it as a NonCachingChain argument" in {
      createNew()
      val f = Flip(0.5)
      def fn(b: Boolean) = if (b) Constant(1); else Constant(2)
      val c = NonCachingChain(f, fn)
      universe.usedBy(f) should equal(Set(c))
      universe.usedBy(c) should equal(Set())
    }

    "return a set that contains an element that uses it as a Select probability" in {
      createNew()
      val u1 = Uniform(0.0, 1.0)
      val u2 = Uniform(0.2, 0.8)
      val s = Select(u1 -> 1, u2 -> 2)
      universe.usedBy(u1) should equal(Set(s))
      universe.usedBy(u2) should equal(Set(s))
      universe.usedBy(s) should equal(Set())
    }

    "return a set that contains an element that uses it as a Dist probability or outcome" in {
      createNew()
      val c1 = Constant(1)
      val c2 = Constant(2)
      val u1 = Uniform(0.0, 1.0)
      val u2 = Uniform(0.2, 0.8)
      val d1 = Dist(0.3 -> c1, 0.7 -> c2)
      val d2 = Dist(u1 -> c1, u2 -> c2)
      universe.usedBy(c1) should equal(Set(d1, d2))
      universe.usedBy(c2) should equal(Set(d1, d2))
      universe.usedBy(u1) should equal(Set(d2))
      universe.usedBy(u2) should equal(Set(d2))
      universe.usedBy(d1) should equal(Set())
      universe.usedBy(d2) should equal(Set())
    }
  }

  "Calling Universe.usedBy on an element created by a chain" should {
    "contain the chain that created it if the chain is caching" in {
      createNew()
      val f = Flip(0.5)
      def fn(b: Boolean) = if (b) Constant(1); else Constant(2)
      val c = CachingChain(f, fn)
      val x = c.get(true)
      universe.usedBy(x) should equal(Set(c))
      // other things shouldn't change
      universe.usedBy(f) should equal(Set(c))
      universe.usedBy(c) should equal(Set())
    }

    "contain the chain that created it if the chain is non-caching" in {
      createNew()
      val f = Flip(0.5)
      def fn(b: Boolean) = if (b) Constant(1); else Constant(2)
      val c = NonCachingChain(f, fn)
      val x = c.get(true)
      universe.usedBy(x) should equal(Set(c))
      // other things shouldn't change
      universe.usedBy(f) should equal(Set(c))
      universe.usedBy(c) should equal(Set())
    }
  }


  
  "clearing unnamed elements" should {
    "remove only elements without names" in {
      val u = Universe.createNew()
      val e1 = Flip(0.1)("e1",u)
      val e2 = Flip(0.2)("e2",u)
      val e3 = Flip(0.3)
      u.activeElements.size should equal(3)
      u.clearUnnamed()
      u.activeElements.size should equal(2)
    }
  }
  
  "Computing independent elements" should {
    "given a set produce the independent and dependent elements" in {
      createNew()
      val e1 = Flip(0.1)
      val e2 = Flip(0.2)
      val e3 = Flip(0.3)
      val e4 = If(e1, e2, e3)
      val e5 = e4 === Flip(0.6)
      universe.independentElements(Set(e1, e2, e5)) should equal((Set(e1, e2), Set(e5)))
    }
  }

  "Computing layers of elements" should {
    "given a set produce layers of independent elements given the previous layers" in {
      createNew()
      val e1 = Flip(0.1)
      val e2 = Flip(0.2)
      val e3 = Flip(0.3)
      val e4 = If(e1, e2, e3)
      val e5 = e4 === Flip(0.6)
      val layers = universe.layers(Set(e1, e2, e3, e4, e5))
      layers.length should equal(3)
      layers(0).length should equal(3)
      layers(0).toSet should equal(Set(e1, e2, e3))
      layers(1) should equal(List(e4))
      layers(2) should equal(List(e5))
    }
    
  }

  "Setting a new universe as the default universe" should {
    "cause elements that are created to be in that universe" in {
      val u = new Universe
      universe = u
      val f = Flip(0.5)
      f.universe should equal(u)
    }
  }
  
   "Registering and deregistering algorithms" should {
    "add algorithms to registered list" in {
      val u = new Universe
      val f = Flip(0.50)
      val alg = VariableElimination(f)
      u.registerAlgorithm(alg)
    }
    
    "remove algorithms from registered list" in {
      val u = new Universe
      val f = Flip(0.50)
      val alg = VariableElimination(f)
      u.deregisterAlgorithm(alg)
    }
  }
}
