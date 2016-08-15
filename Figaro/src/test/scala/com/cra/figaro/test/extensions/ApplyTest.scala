package com.cra.figaro.test.extensions


import com.cra.figaro.extensions.Apply
import com.cra.figaro.language.{Select, Constant, Universe}
import com.cra.figaro.library.atomic.discrete.Uniform
import org.scalatest.Matchers
import org.scalatest.WordSpec

class ApplyTest extends WordSpec with Matchers {


  "An extension Apply with one argument" should {
    "have value equal to its function applied to its argument" in {
      Universe.createNew()
      val u = Uniform(0.0, 2.0)
      val a = Apply(u)(_ + 1.0)
      u.value = 1.3
      a.generate()
      a.value should equal(2.3)
    }

    "convert to the correct string" in {
      Universe.createNew()
      val u = Uniform(0.0, 2.0)
      val f = (d: Double) => d + 1.0
      Apply(u)(f).toString should equal("Apply(" + u + ", " + f + ")")
    }

  }

  "An extension Apply with two arguments" should {
    "have value equal to its function applied to its arguments" in {
      Universe.createNew()
      val u = Uniform(0.0, 2.0)
      val v = Constant(1.0)
      val a = Apply(u, v)( _ + _ + 1.0)
      u.value = 1.3
      v.value = 1.0
      a.generate()
      a.value should equal(3.3)
    }

    "convert to the correct string" in {
      val u = Uniform(0.0, 2.0)
      val v = Constant(1.0)
      val f = (d1: Double, d2: Double) => d1 + d2 + 1.0
      Apply(u, v)(f).toString should equal("Apply(" + u + ", " + v + ", " + f + ")")
    }
  }

  "An extension Apply with three arguments" should {
    "have value equal to its function applied to its arguments" in {
      Universe.createNew()
      val u = Uniform(0.0, 2.0)
      val v = Constant(1.0)
      val w = Select(0.5 -> 0.0, 0.5 -> 5.0)
      val a = Apply(u, v, w)(_ + _ + _ + 1.0)
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
      Apply(u, v, w)(f).toString should equal(
        "Apply(" + u + ", " + v + ", " + w + ", " + f + ")")
    }
  }

  "An extension Apply with four arguments" should {
    "have value equal to its function applied to its arguments" in {
      Universe.createNew()
      val u = Uniform(0.0, 2.0)
      val v = Constant(1.0)
      val w = Select(0.5 -> 0.0, 0.5 -> 5.0)
      val x = Constant(-2.0)
      val a = Apply(u, v, w, x)(_ + _ + _ + _ + 1.0)
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
      Apply(u, v, w, x)(f).toString should equal(
        "Apply(" + u + ", " + v + ", " + w + ", " + x + ", " + f + ")")
    }
  }
}