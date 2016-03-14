/*
 * ReferenceTest.scala   
 * Name and reference tests.
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
import org.scalatest.{ PrivateMethodTester, WordSpec }
import com.cra.figaro.algorithm._
import com.cra.figaro.algorithm.factored._
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.language._
import com.cra.figaro.language.Universe._
import com.cra.figaro.library.atomic.continuous._
import com.cra.figaro.library.atomic.discrete
import com.cra.figaro.util._
import scala.math.log
import com.cra.figaro.algorithm.factored.beliefpropagation.BeliefPropagation

class ReferenceTest extends WordSpec with PrivateMethodTester with Matchers {
  "A Reference" when {

    "created from a string with no periods" should {
      "create a Name with the string" in {
        val r: Reference[Int] = "abc"
        r should equal(Name("abc"))
      }
    }

    "created from a string with a period" should {
      "create the Indirect Reference in which the head is the first part of the string and the tail is the second" in {
        val r: Reference[Int] = "abc.def"
        r should equal(Indirect(Name("abc"), Name("def")))
      }
    }

    "created from a string with only periods" should {
      "throw IllegalArgumentException" in {
        an [IllegalArgumentException] should be thrownBy { val r: Reference[Int] = "..." } 
      }
    }
  }

  "An ElementCollection" when {
        "getting the element associated with a name" should {
      "return the reference element associated with the name" in {
        createNew()
        val u = Uniform(0.0, 1.0)("u", universe)
        universe.getElementByReference[Double]("u") should equal(u)
      }
    }

    "getting the element associated with an indirect reference" should {
      "correctly resolve the reference" in {
        createNew()
        val ec = new ElementCollection {}
        val u = Uniform(0.0, 1.0)("u", ec)
        val c = Constant(ec)("g", universe)
        universe.getElementByReference[Double]("g.u") should equal(u)
      }

      "throw NoSuchElementException when given a reference in which a name does not exist" in {
        createNew()
        an [NoSuchElementException] should be thrownBy { universe.getElementByReference("g.u") } 
      }

      "throw NoSuchElementException when given a reference in which an intermediate name does not refer " +
        "to an element collection" in {
          createNew()
          val u = Uniform(0.0, 1.0)
          an [NoSuchElementException] should be thrownBy  { universe.getElementByReference("u.f") }
        }
    }
    
    "determining if a reference is resolvable" should {
      "return the resolvable nature of the reference" in {
        createNew()
        class Test extends ElementCollection {
          val t = Constant(0)("t", this)
        }
        val u = Constant(new Test)("u", universe)
        universe.hasRef("u.t") should be (true)
        universe.hasRef("u.z") should be (false)
      }
    }


    "getting the elements associated with a multi-valued reference" should {
      "return the set consisting of all elements referred to" in {
        createNew()
        val ec1 = new ElementCollection {}
        val ec2 = new ElementCollection {}
        val ec3 = new ElementCollection {}
        val ec4 = new ElementCollection {}
        val ec5 = new ElementCollection {}
        val z3 = Flip(0.1)("z", ec3)
        val z4 = Flip(0.2)("z", ec4)
        val z5 = Flip(0.3)("z", ec5)
        val y1 = Constant(List(ec3, ec4))("y", ec1)
        val y2 = Constant(List(ec4, ec5))("y", ec2)
        val x = Constant(List(ec1, ec2))("x", universe)
        universe.getManyElementsByReference[Boolean]("x.y.z") should equal(Set(z3, z4, z5))
      }
    }

    "getting all possibilities associated with a reference" should {
      "given a single name produce the single element associated with the name" in {
        createNew()
        val f = Flip(0.2)("f", universe)
        val poss = universe.allPossibleResolutions("f")
        poss should equal(Set((f, List())))
      }

      "given an indirect reference of length two produce all the possibilities" in {
        createNew()
        class EC extends ElementCollection {
          val u = Uniform(0.0, 1.0)("u", this)
        }
        val ec1 = new EC
        val ec2 = new EC
        val choice = Select(0.2 -> ec1, 0.8 -> ec2)("g", universe)
        val poss = universe.allPossibleResolutions("g.u")
        poss should equal(Set((ec1.u, List(Element.ElemVal(choice, ec1))), (ec2.u, List(Element.ElemVal(choice, ec2)))))
      }

      "given an indirect reference of length three produce all the possibilities" in {
        createNew()
        class ECOut extends ElementCollection {
          class ECIn extends ElementCollection {
            val u = Uniform(0.0, 1.0)("u", this)
          }
          val ecIn1 = new ECIn
          val ecIn2 = new ECIn
          val choiceIn = Select(0.5 -> ecIn1, 0.5 -> ecIn2)("c", this)
        }
        val ecOut1 = new ECOut
        val ecOut2 = new ECOut
        val choiceOut = Select(0.2 -> ecOut1, 0.8 -> ecOut2)("g", universe)
        val poss = universe.allPossibleResolutions("g.c.u")
        poss should equal(Set(
          (ecOut1.ecIn1.u, List[Element.ElemVal[_]](Element.ElemVal(choiceOut, ecOut1), Element.ElemVal(ecOut1.choiceIn, ecOut1.ecIn1))),
          (ecOut1.ecIn2.u, List[Element.ElemVal[_]](Element.ElemVal(choiceOut, ecOut1), Element.ElemVal(ecOut1.choiceIn, ecOut1.ecIn2))),
          (ecOut2.ecIn1.u, List[Element.ElemVal[_]](Element.ElemVal(choiceOut, ecOut2), Element.ElemVal(ecOut2.choiceIn, ecOut2.ecIn1))),
          (ecOut2.ecIn2.u, List[Element.ElemVal[_]](Element.ElemVal(choiceOut, ecOut2), Element.ElemVal(ecOut2.choiceIn, ecOut2.ecIn2)))))
      }
    }

    "computing the arguments of a reference with a reference" should {
      "given a single name produce the arguments of the element associated with the name " +
        "as well as the element itself" in {
          createNew()
          val u = Uniform(0.0, 1.0)
          val f = Flip(u)("f", universe)
          val refelem = universe.get("f")
          refelem.args.toSet should equal(Set(u, f))
        }

      "given an indirect reference of length two produce the arguments associated with the possibilities as well as" +
        "the selector element of the head and the possibilities themselves" in {
          createNew()
          class EC extends ElementCollection {
            val u = Uniform(0.0, 1.0)
            val f = Flip(u)("f", this)
          }
          val ec1 = new EC
          val ec2 = new EC
          val choice = Select(0.2 -> ec1, 0.8 -> ec2)("g", universe)
          val refelem = universe.get("g.f")
          refelem.args.toSet should equal(Set(ec1.u, ec1.f, ec2.u, ec2.f, choice))
        }

      "given an indirect reference of length three produce the arguments associated with all the possibilities" +
        "as well as all selectors along the path and the possibilities themselves" in {
          createNew()
          class ECOut extends ElementCollection {
            class ECIn extends ElementCollection {
              val u = Uniform(0.0, 1.0)
              val f = Flip(u)("f", this)
            }
            val ecIn1 = new ECIn
            val ecIn2 = new ECIn
            val choiceIn = Select(0.5 -> ecIn1, 0.5 -> ecIn2)("c", this)
          }
          val ecOut1 = new ECOut
          val ecOut2 = new ECOut
          val choiceOut = Select(0.2 -> ecOut1, 0.8 -> ecOut2)("g", universe)
          val refelem = universe.get("g.c.f")
          refelem.args.toSet should equal(Set(ecOut1.ecIn1.u, ecOut1.ecIn1.f, ecOut1.ecIn2.u, ecOut1.ecIn2.f,
            ecOut2.ecIn1.u, ecOut2.ecIn1.f, ecOut2.ecIn2.u, ecOut2.ecIn2.f,
            ecOut1.choiceIn, ecOut2.choiceIn, choiceOut))
        }
    }

    "asserting evidence" should {

      "have simple evidence exist in an existing element" in {
        createNew()
        class EC1 extends ElementCollection {
          Flip(0.3)("f1", this)
        }
        val ec1 = new EC1
        ec1.assertEvidence("f1", Observation(true))
        val f1 = ec1.getElementByReference[Boolean]("f1")
        f1.condition(true) should equal(true)
        f1.condition(false) should equal(false)
      }

      "have indirect evidence exist in a single existing embedded element" in {
        createNew()
        class EC1 extends ElementCollection {
          Flip(0.3)("f1", this)
        }
        class EC2 extends ElementCollection {
          Constant(new EC1)("x1", this)
        }
        val ec2 = new EC2
        ec2.assertEvidence("x1.f1", Observation(true))
        val f1 = ec2.getElementByReference[Boolean]("x1.f1")
        f1.condition(true) should equal(true)
        f1.condition(false) should equal(false)
      }

      "have indirect evidence exist in multiple existing embedded elements" in {
        createNew()
        class EC1 extends ElementCollection {
          Flip(0.3)("f1", this)
        }
        class EC2 extends ElementCollection {
          Constant(Set(new EC1, new EC1))("x1", this)
        }
        val ec2 = new EC2
        ec2.assertEvidence("x1.f1", Observation(true))
        val f1s = ec2.getManyElementsByReference[Boolean]("x1.f1")
        f1s.size should equal(2)
        for { f1 <- f1s } {
          f1.condition(true) should equal(true)
          f1.condition(false) should equal(false)
        }
      }

      "have simple evidence exist in newly created element with appropriate name" in {
        createNew()
        class EC1 extends ElementCollection
        val ec1 = new EC1
        ec1.assertEvidence("f1", Observation(true))
        Flip(0.3)("f1", ec1)
        val f1 = ec1.getElementByReference[Boolean]("f1")
        f1.condition(true) should equal(true)
        f1.condition(false) should equal(false)
      }

      "have indirect evidence exist in newly created element of embedded collection that already exists" in {
        createNew()
        class EC1 extends ElementCollection
        val ec1 = new EC1
        class EC2 extends ElementCollection {
          Constant(ec1)("x1", this)
        }
        val ec2 = new EC2
        ec2.assertEvidence("x1.f1", Observation(true))
        Flip(0.3)("f1", ec1)
        val f1 = ec2.getElementByReference[Boolean]("x1.f1")
        f1.condition(true) should equal(true)
        f1.condition(false) should equal(false)
      }

      "have indirect evidence exist in existing element of newly created embedded collection" in {
        createNew()
        class EC1 extends ElementCollection
        val ec1 = new EC1
        class EC2 extends ElementCollection
        val ec2 = new EC2
        ec2.assertEvidence("x1.f1", Observation(true))
        Flip(0.3)("f1", ec1)
        Constant(ec1)("x1", ec2)
        val f1 = ec2.getElementByReference[Boolean]("x1.f1")
        f1.condition(true) should equal(true)
        f1.condition(false) should equal(false)
      }

      "have indirect evidence exist in newly created element of newly created embedded collection" in {
        createNew()
        class EC1 extends ElementCollection
        val ec1 = new EC1
        class EC2 extends ElementCollection
        val ec2 = new EC2
        ec2.assertEvidence("x1.f1", Observation(true))
        Constant(ec1)("x1", ec2)
        Flip(0.3)("f1", ec1)
        val f1 = ec2.getElementByReference[Boolean]("x1.f1")
        f1.condition(true) should equal(true)
        f1.condition(false) should equal(false)
      }

      "have indirect evidence exist in correct values of previously created element, depending on contingencies" in {
        createNew()
        class EC1 extends ElementCollection {
          val f = Flip(0.3)("f", this)
        }
        val ec11 = new EC1
        val ec12 = new EC1
        class EC2 extends ElementCollection {
          val x = Select(0.5 -> ec11, 0.5 -> ec12)("x", this)
        }
        val ec2 = new EC2
        ec2.assertEvidence("x.f", Observation(true))
        val f1 = ec11.f
        val f2 = ec12.f
        ec2.x.value = ec11
        f1.condition(true) should equal(true)
        f2.condition(true) should equal(true)
        f1.condition(false) should equal(false)
        f2.condition(false) should equal(true)
        ec2.x.value = ec12
        f1.condition(true) should equal(true)
        f2.condition(true) should equal(true)
        f1.condition(false) should equal(true)
        f2.condition(false) should equal(false)
      }

      "have indirect evidence exist in correct values of newly created element, depending on contingencies" in {
        createNew()
        class EC1 extends ElementCollection {
          val f = Flip(0.3)("f", this)
        }
        val ec11 = new EC1
        val ec12 = new EC1
        class EC2 extends ElementCollection {
        }
        val ec2 = new EC2
        ec2.assertEvidence("x.f", Observation(true))
        val x = Select(0.5 -> ec11, 0.5 -> ec12)("x", ec2)
        val f1 = ec11.f
        val f2 = ec12.f
        x.value = ec11
        f1.condition(true) should equal(true)
        f2.condition(true) should equal(true)
        f1.condition(false) should equal(false)
        f2.condition(false) should equal(true)
        x.value = ec12
        f1.condition(true) should equal(true)
        f2.condition(true) should equal(true)
        f1.condition(false) should equal(true)
        f2.condition(false) should equal(false)

      }

      "allow setting of evidence on lazy val element" in {
        createNew()
        class EC1 extends ElementCollection {
          lazy val f = Flip(0.3)("f", this)
        }
        val ec1 = new EC1
        ec1.assertEvidence("f", Observation(true))
        val f1 = ec1.f
        f1.condition(true) should equal(true)
        f1.condition(false) should equal(false)
      }

      "should assert that contigent elements only take on values that are resolvable in the reference" in {
        createNew()
        class EC extends ElementCollection
        class EC1 extends ElementCollection {
          val f = Flip(0.3)("f", this)
        }
        class EC2 extends ElementCollection {
          val g = Flip(0.3)("g", this)
        }
        val ec1 = new EC1
        val ec2 = new EC2
        val ec = new EC
        val x = Select(0.5 -> ec1, 0.5 -> ec2)("x", ec)
        ec.assertEvidence("x.f", Observation(true))
        x.condition(ec1) should equal(true)
        x.condition(ec2) should equal(false)
      }

      "should remove contingent assertions when evidence is removed on a reference" in {
        createNew()
        class EC extends ElementCollection
        class EC1 extends ElementCollection {
          val f = Flip(0.3)("f", this)
        }
        class EC2 extends ElementCollection {
          val g = Flip(0.3)("g", this)
        }
        val ec1 = new EC1
        val ec2 = new EC2
        val ec = new EC
        val x = Select(0.5 -> ec1, 0.5 -> ec2)("x", ec)
        ec.assertEvidence("x.f", Observation(true))
        ec.removeEvidence("x.f")
        x.condition(ec1) should equal(true)
        x.condition(ec2) should equal(true)
      }
    }
  }

  "A Universe" when {
    "having activated an element with empty name" should {
      "allow activating another element with empty name" in {
        createNew()
        Flip(0.2)
        Constant(1)
      }
    }

    "having activated an element with a name" should {
      "allow getting the associated element" in {
        createNew()
        val f = Flip(0.2)("f", universe)
        universe.getElementByReference[Boolean]("f") should equal(f)
      }

      "allow activating another element with the same name, and get the most recent" in {
        createNew()
        val f = Flip(0.2)("f", universe)
        val c = Constant(1)("f", universe)
        universe.getElementByReference[Int]("f") should equal(c)
        assert(universe.activeElements.contains(f))
        assert(universe.activeElements.contains(c))
      }
    }

    "having deactivated an element with a name" should {
      "allow activating another element with the same name" in {
        createNew()
        val f = Flip(0.2)("f", universe)
        f.deactivate()
        Constant(1)("f", universe)
      }
    }

    "asserting evidence" should {
      "given a name and a condition set the condition of the associated element" in {
        val u = new Universe
        val e = Select(0.2 -> 1, 0.3 -> 2, 0.5 -> 3)("s", u)
        val evidence = NamedEvidence[Int]("s", Condition((i: Int) => i > 1))
        u.assertEvidence(List(evidence))
        e.condition(1) should equal(false)
        e.condition(2) should equal(true)
        e.condition(3) should equal(true)
      }

      "given a name and a constraint set the constraint of the associated element" in {
        val u = new Universe
        val e = Select(0.2 -> 1, 0.3 -> 2, 0.5 -> 3)("s", u)
        val evidence = NamedEvidence[Int]("s", Constraint((i: Int) => i.toDouble))
        u.assertEvidence(List(evidence))
        //e.constraint(1) should equal(log(1.0))
        e.constraint(2) should equal(log(2.0))
        e.constraint(3) should equal(log(3.0))
      }

      "given a name and an observation set the condition of the associated element" in {
        val u = new Universe
        val e = Select(0.2 -> 1, 0.3 -> 2, 0.5 -> 3)("s", u)
        val evidence = NamedEvidence[Int]("s", Observation(3))
        u.assertEvidence(List(evidence))
        e.condition(1) should equal(false)
        e.condition(2) should equal(false)
        e.condition(3) should equal(true)
      }
    }
  }

  "Computing values" should {
    "produce the correct set of values for a simple single-valued reference" in {
      val u = Universe.createNew()
      val s = Select(0.2 -> 1, 0.3 -> 2, 0.5 -> 3)("s", u)
      val r = u.getElementByReference[Int]("s")
      Values()(r) should equal(Set(1, 2, 3))
    }

    "produce the correct set of values for an indirect single-valued reference" in {
      val u = Universe.createNew()
      class C1 extends ElementCollection {
        val s = Select(0.2 -> 1, 0.3 -> 2, 0.5 -> 3)("s", this)
      }
      class C2 extends C1 {
        override val s = Select(0.5 -> 4, 0.5 -> 2)("s", this)
      }
      val x = Select(0.2 -> new C1, 0.8 -> new C2)("x", u)
      val r = u.get[Int]("x.s")
      Values()(r) should equal(Set(1, 2, 3, 4))
    }

    "produce the correct set of values for an aggregate" in {
      val u = Universe.createNew()
      class C1 extends ElementCollection {
        val s = Select(0.2 -> 1, 0.3 -> 2, 0.5 -> 3)("s", this)
      }
      class C2 extends C1 {
        override val s = Select(0.5 -> 7, 0.5 -> 3)("s", this)
      }
      val x = Select(0.2 -> HashMultiSet(new C1, new C1, new C1), 0.8 -> HashMultiSet(new C2, new C2))("x", universe)
      val a = u.getAggregate((s: MultiSet[Int]) => (0 /: s)(_ + _))("x.s")
      Values()(a) should equal(Set(3, 4, 5, 6, 7, 8, 9, 10, 14))
    }

    "produce the correct set of values for a multi-step aggregate" in {
      val u = Universe.createNew()
      class C1 extends ElementCollection {
        val s = Select(0.2 -> 1, 0.3 -> 2, 0.5 -> 3)("s", this)
      }
      class C2 extends C1 {
        override val s = Select(0.5 -> 7, 0.5 -> 3)("s", this)
      }
      class C3(n: Int) extends ElementCollection {
        val x = Select(0.2 -> HashMultiSet[C1](List.tabulate(n + 1)(i => new C1): _*), 0.8 -> HashMultiSet[C1](List.tabulate(n)(i => new C2): _*))("x", this)
      }
      class C4 extends ElementCollection {
        val y = Select(0.6 -> new C3(1), 0.4 -> new C3(2))("y", this)
      }
      val w = Select(0.5 -> List(new C4), 0.5 -> List(new C4, new C4))("w", u)
      val a = u.getAggregate((s: MultiSet[Int]) => (0 /: s)(_ + _))("w.y.x.s")
      // A single y could be (C1, C1), (C1, C1, C1), (C2), or (C2, C2)
      // w is 1 or 2 of these in any combination. The possibilities are (C1, C1), (C1, C1, C1), (C2), (C2, C2), (C1, C1, C1, C1), (C1, C1, C1, C1, C1),
      // (C1, C1, C2), (C1, C1, C2, C2), (C1, C1, C1, C1, C1, C1), (C1, C1, C1, C2), (C1, C1, C1, C2, C2), (C2, C2, C2), or (C2, C2, C2, C2)
      // Values of these possibilities are (2,3,4,5,6), (3,4,5,6,7,8,9), (3,7), (6,10,14), (4,5,6,7,8,9,10,11,12), (5,6,7,8,9,10,11,12,13,14,15),
      // (5,6,7,8,9,10,11,12,13), (8,9,10,11,12,13,14,15,16,17,18,19,20), (6,7,8,9,10,11,12,13,14,15,16,17,18), (6,7,8,9,10,11,12,13,14,15,16),
      // (9,10,11,12,13,14,15,16,17,18,19,20,21,22,23), (9,13,17,21), (12,16,20,24,28)
      // POssible values are 2-24 and 28.
      Values()(a) should equal(Set((2 to 24): _*) + 28)
    }
  }

  "Running variable elimination" should {
    "produce the correct result for a simple single-valued reference" in {
      val u = Universe.createNew()
      val s = Select(0.2 -> 1, 0.3 -> 2, 0.5 -> 3)("s", u)
      val r = u.getElementByReference[Int]("s")
      val alg = VariableElimination(r)
      alg.start()
      alg.probability(r, 1) should equal(0.2)
    }

    "produce the correct set of values for an indirect single-valued reference" in {
      val u = Universe.createNew()
      class C1 extends ElementCollection {
        val s = Select(0.2 -> 1, 0.3 -> 2, 0.5 -> 3)("s", this)
      }
      class C2 extends C1 {
        override val s = Select(0.5 -> 4, 0.5 -> 2)("s", this)
      }
      val x = Select(0.2 -> new C1, 0.8 -> new C2)("x", u)
      val r = u.get[Int]("x.s")
      val alg = VariableElimination(r)
      alg.start()
      val correctProb = 0.2 * 0.3 + 0.8 * 0.5
      alg.probability(r, 2) should be(correctProb +- 0.00000001)
    }

    "produce the correct set of values for an aggregate" in {
      val u = Universe.createNew()
      class C1 extends ElementCollection {
        val s = Select(0.2 -> 1, 0.3 -> 2, 0.5 -> 3)("s", this)
      }
      class C2 extends C1 {
        override val s = Select(0.5 -> 7, 0.5 -> 3)("s", this)
      }
      val x = Select(0.2 -> HashMultiSet(new C1, new C1, new C1), 0.8 -> HashMultiSet(new C2, new C2))("x", universe)
      val a = u.getAggregate((s: MultiSet[Int]) => (0 /: s)(_ + _))("x.s")
      val alg = VariableElimination(a)
      alg.start()
      // 6 can result in the following ways: Using C1: (1,2,3), (1,3,2), (2,1,3), (2,3,1), (3,1,2), (3,2,1), (2,2,2); Using C2: (3,3)
      val correctProb = 0.2 * (6 * 0.2 * 0.3 * 0.5 + 0.3 * 0.3 * 0.3) + 0.8 * 0.5 * 0.5
      alg.probability(a, 6) should be(correctProb +- 0.00000001)
    }

    "produce the correct set of values for a multi-step aggregate" in {
      val u = Universe.createNew()
      class C1 extends ElementCollection {
        val s = Select(0.2 -> 1, 0.3 -> 2, 0.5 -> 3)("s", this)
      }
      class C2 extends C1 {
        override val s = Select(0.25 -> 7, 0.75 -> 3)("s", this)
      }
      class C3(n: Int) extends ElementCollection {
        val x = Select(0.2 -> HashMultiSet[C1](List.tabulate(n + 1)(i => new C1): _*), 0.8 -> HashMultiSet[C1](List.tabulate(n)(i => new C2): _*))("x", this)
      }
      class C4 extends ElementCollection {
        val y = Select(0.6 -> new C3(0), 0.4 -> new C3(1))("y", this)
      }
      val w = Select(0.1 -> List(new C4), 0.9 -> List(new C4, new C4))("w", u)
      val a = u.getAggregate((s: MultiSet[Int]) => (0 /: s)(_ + _))("w.y.x.s")
      val alg = BeliefPropagation(100, a) //VariableElimination.debugged(a)
      alg.start()
      // C3(0) could be (1) (prob 0.2 * 0.2), (2) (prob 0.2 * 0.3), (3) (prob 0.2 * 0.5), () (prob 0.8)
      // C3(1) could be (1,1) (prob 0.2 * 0.2 * 0.2), (1,2) or (2,1) (prob 0.2 * 0.2 * 0.3 * 2), (1,3) or (3,1) (prob 0.2 * 0.2 * 0.5 * 2),
      //   (2,2) (prob 0.2 * 0.3 * 0.3), (2,3) or (3,2) (prob 0.2 * 0.3 * 0.5 * 2), (3,3) (prob 0.2 * 0.5 * 0.5), (3) (prob 0.8 * 0.75), (7) (prob 0.8 * 0.25)
      // w is: C3(0) (prob 0.1 * 0.6), C3(1) (prob 0.1 * 0.4), C3(0) + C3(0) (prob 0.9 * 0.6 * 0.6), C3(0) + C3(1) or C3(1) + C3(0) (prob 0.9 * 0.6 * 0.4 * 2) 
      //   C3(1) + C3(1) (prob 0.9 * 0.4 * 0.4)
      // C3(0) comes to 3 with probability 0.2 * 0.5
      // C3(1) comes to 3 with probability 0.2 * 0.2 * 0.3 * 2 + 0.8 * 0.75
      // C3(0) + C3(0) comes to 3 (1 + 2 or 2 + 1 or 0 + 3 or 3 + 0) with probability 0.2 * 0.2 * 0.2 * 0.3 * 2 + 0.2 * 0.5 * 0.8 * 2
      // C3(0) + C3(1) comes to 3 (0 + 3 or 1 + 2) with probability 0.8 * (0.2 * 0.2 * 0.3 * 2 + 0.8 * 0.75) + 0.2 * 0.2 * 0.2 * 0.2 * 0.2
      // CS(1) + CS(1) cannot come to 3
      val probw0 = 0.1 * 0.6
      val probw1 = 0.1 * 0.4
      val probw00 = 0.9 * 0.6 * 0.6
      val probw01 = 0.9 * 0.6 * 0.4 * 2
      val probw11 = 0.9 * 0.4 * 0.4
      assert(math.abs(probw0 + probw1 + probw00 + probw01 + probw11 - 1.0) < 0.0000001)
      val prob3Ifw0 = 0.2 * 0.5
      val prob3Ifw1 = 0.2 * 0.2 * 0.3 * 2 + 0.8 * 0.75
      val prob3Ifw00 = 0.2 * 0.2 * 0.2 * 0.3 * 2 + 0.2 * 0.5 * 0.8 * 2
      val prob3Ifw01 = 0.8 * (0.2 * 0.2 * 0.3 * 2 + 0.8 * 0.75) + 0.2 * 0.2 * 0.2 * 0.2 * 0.2
      val prob3Ifw11 = 0
      val prob3 = probw0 * prob3Ifw0 + probw1 * prob3Ifw1 + probw00 * prob3Ifw00 + probw01 * prob3Ifw01 + probw11 * prob3Ifw11
      alg.probability(a, 3) should be(prob3 +- 0.00000001)
    }

    "produce the correct result with a class hierarchy with no evidence" in {
      Universe.createNew()
      val myVehicle = Vehicle.generate("v1")
      val i1 = Apply(myVehicle, (v: Vehicle) => v.isInstanceOf[Pickup])
      val alg = VariableElimination(i1)
      alg.start()
      alg.probability(i1, true) should be((0.4 * 0.3) +- 0.01)
    }

    "produce the correct result with a class hierarchy with evidence" in {
      Universe.createNew()
      val myVehicle = Vehicle.generate("v1")
      universe.assertEvidence(List(NamedEvidence("v1.size", Observation('medium))))
      val i1 = Apply(myVehicle, (v: Vehicle) => v.isInstanceOf[Pickup])
      val alg = VariableElimination(i1)
      alg.start()
      alg.probability(i1, true) should be(((0.3 * 1.0) / (0.3 * 1.0 + 0.6 * 0.25)) +- 0.01)
    }
  }

  "Running importance sampling" should {
    "produce the correct result with a class hierarchy with no evidence" in {
      Universe.createNew()
      val myVehicle = Vehicle.generate("v1")
      val i1 = Apply(myVehicle, (v: Vehicle) => v.isInstanceOf[Pickup])
      val alg = Importance(10000, i1)
      alg.start()
      alg.probability(i1, true) should be((0.4 * 0.3) +- 0.01)
    }

    "produce the correct result with a class hierarchy with evidence" in {
      Universe.createNew()
      val myVehicle = Vehicle.generate("v1")
      universe.assertEvidence(List(NamedEvidence("v1.size", Observation('medium))))
      val i1 = Apply(myVehicle, (v: Vehicle) => v.isInstanceOf[Pickup])
      val alg = Importance(10000, i1)
      alg.start()
      alg.probability(i1, true) should be(((0.3 * 1.0) / (0.3 * 1.0 + 0.6 * 0.25)) +- 0.01)
    }
  }

  "Running Metropolis-Hastings" should {
    "produce the correct result with a class hierarchy with no evidence" in {
      Universe.createNew()
      val myVehicle = Vehicle.generate("v1")
      val i1 = Apply(myVehicle, (v: Vehicle) => v.isInstanceOf[Pickup])
      val alg = MetropolisHastings(1000000, ProposalScheme.default, i1)
      alg.start()
      alg.probability(i1, true) should be((0.4 * 0.3) +- 0.01)
    }

    "produce the correct result with a class hierarchy with evidence" in {
      Universe.createNew()
      val myVehicle = Vehicle.generate("v1")
      universe.assertEvidence(List(NamedEvidence("v1.size", Observation('medium))))
      val i1 = Apply(myVehicle, (v: Vehicle) => v.isInstanceOf[Pickup])
      val alg = MetropolisHastings(1000000, ProposalScheme.default, i1)
      alg.start()
      alg.probability(i1, true) should be(((0.3 * 1.0) / (0.3 * 1.0 + 0.6 * 0.25)) +- 0.01)
    }
  }

  abstract class Vehicle extends ElementCollection {
    val size: Element[Symbol]
  }

  class Truck extends Vehicle {
    val size: Element[Symbol] = Select(0.25 -> 'medium, 0.75 -> 'big)("size", this)
    lazy val capacity: Element[Int] = Chain(size, (s: Symbol) => if (s == 'big) Select(0.5 -> 1000, 0.5 -> 2000); else Constant(100))("capacity", this)
  }

  class Pickup extends Truck {
    override val size: Element[Symbol] = Constant('medium)("size", this)
    val color: Element[Symbol] = discrete.Uniform('blue, 'red)
  }

  class TwentyWheeler extends Truck {
    override val size: Element[Symbol] = Constant('huge)("size", this)
    override lazy val capacity = Constant(5000)("capacity", this)
  }

  class Car extends Vehicle {
    val size = Constant('small)("size", this)
  }

  object Vehicle {
    def generate(name: String): Element[Vehicle] = 
      Dist(0.6 -> Car.generate, 0.4 -> Truck.generate)(name, Universe.universe)
  }

  object Truck {
    def generate: Element[Vehicle] = Dist(0.1 -> TwentyWheeler.generate, 0.3 -> Pickup.generate, 0.6 -> Constant[Vehicle](new Truck))
  }

  object Pickup {
    def generate: Element[Vehicle] = Constant(new Pickup)
  }

  object TwentyWheeler {
    def generate: Element[Vehicle] = Constant(new TwentyWheeler)
  }

  object Car {
    def generate: Element[Vehicle] = Constant(new Car)
  }
}
