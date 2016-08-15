package com.cra.figaro.test.extensions



import com.cra.figaro.extensions.{CachingChain, Chain, NonCachingChain}
import com.cra.figaro.language.{Chain => _, CachingChain => _, NonCachingChain => _, _}
import com.cra.figaro.library.atomic.discrete.Uniform
import org.scalatest.Matchers
import org.scalatest.WordSpec

class ChainTest extends WordSpec with Matchers {

  "A Chain" when {
    "called" should {
      "have value equal to the value of its function applied to its argument's value" in {
        Universe.createNew()
        val f1 = Flip(0.7)
        val f2 = Flip(0.4)
        val f3 = Flip(0.9)
        val c = NonCachingChain(f1)(b => if(b) f2; else f3)
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
        NonCachingChain(f1)(fn).toString should equal("Chain(" + f1 + ", " + fn + ")")
      }

      "call the CPD for each Chain access" in {
        Universe.createNew()
        var sum = 0
        def fn(b: Int) = {
          sum += 1
          Constant(b)
        }
        val f1 = Uniform(0, 1, 2)
        val c = NonCachingChain(f1)(fn)
        sum = 0
        c.get(0)
        c.get(1)
        c.get(2)
        c.get(0)
        sum should equal(4)
      }
    }

  }

  "A chain with two parents" when {
    "non-caching" should {
      "have value equal to the value of its function applied to its argument's values" in {
        Universe.createNew()
        val f1 = Flip(0.4)
        val f2 = Flip(0.7)
        val f3 = Flip(0.9)
        val f4 = Flip(0.235)
        val c = NonCachingChain(f1, f2)((b1, b2) => if (b1 && b2 ) f3; else f4)
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
        NonCachingChain(f1, f2)(fn).toString should equal("Chain(Apply(" + f1 + ", " + f2 + ", " + fn + "), <function1>)")
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
        val c = Chain(f1, f2)(fn)
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
        val c = CachingChain(f1, f2)((b1, b2) => if (b1  && b2 ) f3; else f4)

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

        CachingChain(f1, f2)(fn).toString should equal("Chain(Apply(" + f1 + ", " + f2 + ", " + fn + "), <function1>)")
      }
      //((((((((((80 * .50) * 1.08) + 82.4 * .50) * 1.08) + 84.8 * .50) * 1.08) + 87.3 * .50) * 1.08) + 89.9 * .50) * 1.08)

    }
  }
}