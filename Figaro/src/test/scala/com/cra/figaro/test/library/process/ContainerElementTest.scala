package com.cra.figaro.test.library.process

import org.scalatest.WordSpec
import org.scalatest.Matchers
import com.cra.figaro.library.process._
import com.cra.figaro.language._
import com.cra.figaro.util._
import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.algorithm.sampling.{Importance, Forward}
import com.cra.figaro.library.compound._

class ContainerElementTest extends WordSpec with Matchers {
  "A container element" should {
    "create elements in the right universes" in {
      val u1 = Universe.createNew()
      val contElem = create()
      val u2 = Universe.createNew()
      contElem(0).universe should equal (u1)
      contElem.get(2).universe should equal (u1)
      contElem.map(!_)(0).universe should equal (u1)
      contElem.chain(if (_) Flip(0.6) else Flip(0.9))(0).universe should equal (u1)
      def and(b1: Boolean, b2: Boolean) = b1 && b2
      contElem.reduce(and).universe should equal (u1)
      contElem.foldLeft(true)(and).universe should equal (u1)
      contElem.foldRight(true)(and).universe should equal (u1)
      contElem.aggregate(true)((b1: Boolean, b2: Boolean) => !b1 || b2, (b1: Boolean, b2: Boolean) => b1 && b2).universe should equal (u1)
      contElem.count((b: Boolean) => b).universe should equal (u1)
      contElem.exists((b: Boolean) => b).universe should equal (u1)
      contElem.forall((b: Boolean) => b).universe should equal (u1)
      contElem.length.universe should equal (u1)
      val contElem2 = new ContainerElement(Constant(Container(Flip(0.6))))
      contElem.concat(contElem2).element.universe should equal (u1)
    }

    "get the right element using apply" in {
      Universe.createNew()
      val contElem = create()
      VariableElimination.probability(contElem(0), true) should be ((0.5 * 0.1 + 0.5 * 0.3) +- 0.0000000001)
    }

    "get the right optional element using get" in {
      Universe.createNew()
      val contElem = create()
      VariableElimination.probability(contElem.get(2), Some(true)) should be ((0.5 * 0.5) +- 0.000000000001)
    }

    "map a function through all possible values correctly" in {
      Universe.createNew()
      val contElem = create()
      VariableElimination.probability(contElem.map(!_)(0), false) should be ((0.5 * 0.1 + 0.5 * 0.3) +- 0.0000000001)
    }

    "chain a function through all possible values correctly" in {
      Universe.createNew()
      val contElem = create()
      val p1 = 0.5 * 0.1 + 0.5 * 0.3
      val p2 = 1 - p1
      val answer = p1 * 0.6 + p2 * 0.9
      VariableElimination.probability(contElem.chain(if (_) Flip(0.6) else Flip(0.9))(0), true) should be (answer +- 0.0000001)
    }

    "correctly fold through elements" in {
      Universe.createNew()
      val contElem = create()
      val p1 = 0.1 * 0.2
      val p2 = 0.3 * 0.4 * 0.5
      val answer = 0.5 * p1 + 0.5 * p2
      def and(b1: Boolean, b2: Boolean) = b1 && b2
      val e1 = contElem.reduce(and)
      val e2 = contElem.foldLeft(true)(and)
      val e3 = contElem.foldRight(true)(and)
      val e4 = contElem.aggregate(true)((b1: Boolean, b2: Boolean) => !b1 || b2, (b1: Boolean, b2: Boolean) => b1 && b2)
      val alg = VariableElimination(e1, e2, e3, e4)
      alg.start()
      alg.probability(e1, true) should be (answer +- 0.0000001)
      alg.probability(e2, true) should be (answer +- 0.0000001)
      alg.probability(e3, true) should be (answer +- 0.0000001)
      alg.probability(e4, true) should be (answer +- 0.0000001)
      alg.kill()
    }

    "when quantifying over elements satisfying a predicate, produce the right answer" in {
      Universe.createNew()
      val contElem = create()
      val elem1 = contElem.count((b: Boolean) => b)
      val elem2 = contElem.exists((b: Boolean) => b)
      val elem3 = contElem.forall((b: Boolean) => b)
      val alg = VariableElimination(elem1, elem2, elem3)
      alg.start()
      val p10 = 0.9 * 0.8
      val p20 = 0.7 * 0.6 * 0.5
      val p11 = 0.9 * 0.2 + 0.1 * 0.8
      val p21 = 0.7 * 0.6 * 0.5 + 0.7 * 0.4 * 0.5 + 0.3 * 0.6 * 0.5
      val p12 = 0.1 * 0.2
      val p22 = 0.7 * 0.4 * 0.5 + 0.3 * 0.6 * 0.5 + 0.3 * 0.4 * 0.5
      val p23 = 0.3 * 0.4 * 0.5
      alg.probability(elem1, 0) should be ((0.5 * p10 + 0.5 * p20) +- 0.0000001)
      alg.probability(elem1, 1) should be ((0.5 * p11 + 0.5 * p21) +- 0.0000001)
      alg.probability(elem1, 2) should be ((0.5 * p12 + 0.5 * p22) +- 0.0000001)
      alg.probability(elem2, true) should be ((1 - (0.5 * p10 + 0.5 * p20)) +- 0.0000001)
      alg.probability(elem3, true) should be ((0.5 * p12 + 0.5 * p23) +- 0.0000001)
      alg.kill()
    }

    "correctly produce an element over the length of the container" in {
      Universe.createNew()
      val contElem = create()
      val len = contElem.length
      val alg = VariableElimination(len)
      val answer = 0.5 * 2 + 0.5 * 3
      alg.start()
      alg.expectation(len, (i: Int) => i.toDouble) should be (answer +- 0.0000001)
      alg.kill()
    }

    "when concatenating, have all the elements of both processes, with the second process following the first" in {
      Universe.createNew()
      val contElem1 = create()
      val contElem2 = new ContainerElement(Constant(Container(Flip(0.6))))
      val contElem = contElem1.concat(contElem2)
      val all = contElem.forall((b: Boolean) => b)
      val p1 = 0.1 * 0.2 * 0.6 // probability first choice for contElem1 are all true and contElem2 is all true
      val p2 = 0.3 * 0.4 * 0.5 * 0.6 // probability second choice for contElem1 are all true and contElem2 is all true
      val answer = 0.5 * p1 + 0.5 * p2
      Importance.probability(all, true) should be (answer +- 0.01)
    }

    "should allow concatenation with a container using implicit conversions" in {
      Universe.createNew()
      val contElem1 = create()
      val cont2 = Container(Flip(0.6))
      val contElem = contElem1.concat(cont2)
      val all = contElem.forall((b: Boolean) => b)
      val p1 = 0.1 * 0.2 * 0.6 // probability first choice for contElem1 are all true and contElem2 is all true
      val p2 = 0.3 * 0.4 * 0.5 * 0.6 // probability second choice for contElem1 are all true and contElem2 is all true
      val answer = 0.5 * p1 + 0.5 * p2
      Importance.probability(all, true) should be (answer +- 0.01)
    }
  }

  def create() = {
    val elem1 = Flip(0.1)
    val elem2 = Flip(0.2)
    val elem3 = Flip(0.3)
    val elem4 = Flip(0.4)
    val elem5 = Flip(0.5)
    val container1: Container[Int, Boolean] = Container(elem1, elem2)
    val container2: Container[Int, Boolean] = Container(elem3, elem4, elem5)
    val containerChooser = Select(0.5 -> container1, 0.5 -> container2)
    new ContainerElement(containerChooser)
  }
}
