/*
 * OnlineEMTest.scala 
 * TBD needs description
 * 
 * Created By:      Michael Howard (mhoward@cra.com)
 * Creation Date:   Mar 30, 2015
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */
package com.cra.figaro.test.algorithm.learning
import org.scalatest.Matchers
import org.scalatest.{ PrivateMethodTester, WordSpec }
import com.cra.figaro.algorithm._
import com.cra.figaro.algorithm.online.Online
import com.cra.figaro.algorithm.factored._
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.algorithm.learning._
import com.cra.figaro.library.atomic.continuous._
import com.cra.figaro.library.atomic.discrete.Binomial
import com.cra.figaro.library.compound._
import com.cra.figaro.language._
import com.cra.figaro.language.Universe._
import com.cra.figaro.util._
import com.cra.figaro.util.random
import scala.math.abs
import java.io._
import com.cra.figaro.test.tags.NonDeterministic

class OnlineEMTest extends WordSpec with PrivateMethodTester with Matchers {
  
    "Online expectation maximization with BP" when
    {

      "used to estimate a Beta parameter" should
        {

          "detect bias after a large enough number of trials" in
            {
              val initial = Universe.createNew
              val b = Beta(2, 2)
              def transition = () => {
                val next = Universe.createNew
                val f = Flip(b)("f",next)
                next
              }
              val algorithm = EMWithBP.online(transition, b)(initial)
              algorithm.start()
              for (i <- 1 to 7) {
                algorithm.update(List(NamedEvidence("f", Observation(true))))
              }

              for (i <- 1 to 3) {
                algorithm.update(List(NamedEvidence("f", Observation(false))))
              }


              val result = b.MAPValue
              algorithm.kill
              result should be(0.6666 +- 0.01)

            }

          "take the prior concentration parameters into account" in
            {
              val initial = Universe.createNew
              val b = Beta(3.0, 7.0)
              def transition = () => {
                val next = Universe.createNew
                val f = Flip(b)("f",next)
                next
              }
              val algorithm = EMWithBP.online(transition, b)(initial)
              algorithm.start()
              for (i <- 1 to 7) {
                algorithm.update(List(NamedEvidence("f", Observation(true))))
              }
              for (i <- 1 to 3) {
                algorithm.update(List(NamedEvidence("f", Observation(false))))
              }
              val result = b.MAPValue
              algorithm.kill
              result should be(0.50 +- 0.01)
            }

          "learn the bias from observations of binomial elements" in {
              val initial = Universe.createNew
              val b = Beta(2, 2)
              def transition = () => {
                val next = Universe.createNew
                val f = Binomial(5,b)("binomial",next)
                next
              }
              val algorithm = EMWithBP.online(transition, b)(initial)
              algorithm.start()
              algorithm.update(List(NamedEvidence("binomial", Observation(4))))
              algorithm.update(List(NamedEvidence("binomial", Observation(3))))
              val result = b.MAPValue
              algorithm.kill
              result should be(0.6666 +- 0.01)
          }

          "correctly use a uniform prior" in {
              val initial = Universe.createNew
              val b = Beta(1, 1)
              def transition = () => {
                val next = Universe.createNew
                val f = Binomial(5,b)("binomial",next)
                next
              }
              val algorithm = EMWithBP.online(transition, b)(initial)
              algorithm.start()
              algorithm.update(List(NamedEvidence("binomial", Observation(4))))
              algorithm.update(List(NamedEvidence("binomial", Observation(3))))
              val result = b.MAPValue
              algorithm.kill
              result should be(0.7 +- 0.01)
          }

        }

      "used to estimate a Dirichlet parameter with two concentration parameters" should
        {

          "detect bias after a large enough number of trials" in
            {
              val initial = Universe.createNew
              val d = Dirichlet(2, 2)
              def transition = () => {
                val next = Universe.createNew
                val s = Select(d, true, false)("s",next)
                next
              }
              val algorithm = EMWithBP.online(transition, d)(initial)
              algorithm.start()
              for (i <- 1 to 7) {
                algorithm.update(List(NamedEvidence("s", Observation(true))))
              }
              for (i <- 1 to 3) {
                algorithm.update(List(NamedEvidence("s", Observation(false))))
              }
              val result = d.MAPValue
              algorithm.kill
              result(0) should be(0.6666 +- 0.01)

            }

          "take the prior concentration parameters into account" in
            {
              val initial = Universe.createNew
              val d = Dirichlet(3, 7)
              def transition = () => {
                val next = Universe.createNew
                val s = Select(d, true, false)("s",next)
                next
              }
              val algorithm = EMWithBP.online(transition, d)(initial)
                         algorithm.start()
              for (i <- 1 to 7) {
                algorithm.update(List(NamedEvidence("s", Observation(true))))
              }
              for (i <- 1 to 3) {
                algorithm.update(List(NamedEvidence("s", Observation(false))))
              }
              val result = d.MAPValue
              algorithm.kill
              result(0) should be(0.50 +- 0.01)

            }

        }        
          
          "used to estimate multiple parameters" should
            {

              "leave parameters having no observations unchanged" in
                {
                  val initial = Universe.createNew
                  val d = Dirichlet(2.0, 4.0, 2.0)
                  val b = Beta(2.0, 2.0)
                  val outcomes = List(1, 2, 3)

                  def transition = () => {
                    val next = Universe.createNew
                    val s = Select(d, outcomes:_*)("s",next)
                    next
                  }
                  val algorithm = EMWithBP.online(transition,d,b)(initial)
                             algorithm.start()
                  for (i <- 1 to 4) {
                    algorithm.update(List(NamedEvidence("s", Observation(1))))
                  }

                  for (i <- 1 to 2) {
                    algorithm.update(List(NamedEvidence("s", Observation(2))))
                  }

                  for (i <- 1 to 4) {
                    algorithm.update(List(NamedEvidence("s", Observation(3))))
                  }

               

                  val result = d.MAPValue
         
                  result(0) should be(0.33 +- 0.01)
                  result(1) should be(0.33 +- 0.01)
                  result(2) should be(0.33 +- 0.01)

                  val betaResult = b.MAPValue
                  betaResult should be(0.5)
                  algorithm.kill
                }

              "correctly estimate all parameters with observations" in
                {
                  val initial = Universe.createNew
                  val d = Dirichlet(2.0, 3.0, 2.0)
                  val b = Beta(3.0, 7.0)
                  val outcomes = List(1, 2, 3)
                  
                  def transition = () => {
                    val next = Universe.createNew
                    val s = Select(d, outcomes:_*)("s",next)
                    val f = Flip(b)("f",next)
                    next
                  }
                  val algorithm = EMWithBP.online(transition,d,b)(initial)
                             algorithm.start()
                  for (i <- 1 to 4) {
                    algorithm.update(List(NamedEvidence("s", Observation(1))))
                  }

                  for (i <- 1 to 2) {
                    algorithm.update(List(NamedEvidence("s", Observation(2))))
                  }

                  for (i <- 1 to 4) {
                    algorithm.update(List(NamedEvidence("s", Observation(3))))
                  }
                  
                  for (i <- 1 to 7) {
                    algorithm.update(List(NamedEvidence("f", Observation(true))))
                  }
                  
                  for (i <- 1 to 3) {
                    algorithm.update(List(NamedEvidence("f", Observation(false))))
                  }

                  val result = d.MAPValue

                  result(0) should be(0.35 +- 0.01)
                  result(1) should be(0.28 +- 0.01)
                  result(2) should be(0.36 +- 0.01)

                  val betaResult = b.MAPValue
                  betaResult should be(0.5 +- 0.01)
                 algorithm.kill
                }
            }
    }
  
  "Online expectation maximization" when
    {

      "used to estimate a Beta parameter" should
        {

          "detect bias after a large enough number of trials" in
            {
              val initial = Universe.createNew
              val b = Beta(2, 2)
              def transition = () => {
                val next = Universe.createNew
                val f = Flip(b)("f",next)
                next
              }
              val algorithm = EMWithVE.online(transition, b)(initial)
              algorithm.start()
              for (i <- 1 to 7) {
                algorithm.update(List(NamedEvidence("f", Observation(true))))
              }

              for (i <- 1 to 3) {
                algorithm.update(List(NamedEvidence("f", Observation(false))))
              }


              val result = b.MAPValue
              algorithm.kill
              result should be(0.6666 +- 0.01)

            }

          "take the prior concentration parameters into account" in
            {
              val initial = Universe.createNew
              val b = Beta(3.0, 7.0)
              def transition = () => {
                val next = Universe.createNew
                val f = Flip(b)("f",next)
                next
              }
              val algorithm = EMWithVE.online(transition, b)(initial)
              algorithm.start()
              for (i <- 1 to 7) {
                algorithm.update(List(NamedEvidence("f", Observation(true))))
              }
              for (i <- 1 to 3) {
                algorithm.update(List(NamedEvidence("f", Observation(false))))
              }
              val result = b.MAPValue
              algorithm.kill
              result should be(0.50 +- 0.01)
            }

          "learn the bias from observations of binomial elements" in {
              val initial = Universe.createNew
              val b = Beta(2, 2)
              def transition = () => {
                val next = Universe.createNew
                val f = Binomial(5,b)("binomial",next)
                next
              }
              val algorithm = EMWithVE.online(transition, b)(initial)
              algorithm.start()
              algorithm.update(List(NamedEvidence("binomial", Observation(4))))
              algorithm.update(List(NamedEvidence("binomial", Observation(3))))
              val result = b.MAPValue
              algorithm.kill
              result should be(0.6666 +- 0.01)
          }

          "correctly use a uniform prior" in {
              val initial = Universe.createNew
              val b = Beta(1, 1)
              def transition = () => {
                val next = Universe.createNew
                val f = Binomial(5,b)("binomial",next)
                next
              }
              val algorithm = EMWithVE.online(transition, b)(initial)
              algorithm.start()
              algorithm.update(List(NamedEvidence("binomial", Observation(4))))
              algorithm.update(List(NamedEvidence("binomial", Observation(3))))
              val result = b.MAPValue
              algorithm.kill
              result should be(0.7 +- 0.01)
          }

        }

      "used to estimate a Dirichlet parameter with two concentration parameters" should
        {

          "detect bias after a large enough number of trials" in
            {
              val initial = Universe.createNew
              val d = Dirichlet(2, 2)
              def transition = () => {
                val next = Universe.createNew
                val s = Select(d, true, false)("s",next)
                next
              }
              val algorithm = EMWithVE.online(transition, d)(initial)
              algorithm.start()
              for (i <- 1 to 7) {
                algorithm.update(List(NamedEvidence("s", Observation(true))))
              }
              for (i <- 1 to 3) {
                algorithm.update(List(NamedEvidence("s", Observation(false))))
              }
              val result = d.MAPValue
              algorithm.kill
              result(0) should be(0.6666 +- 0.01)

            }

          "take the prior concentration parameters into account" in
            {
              val initial = Universe.createNew
              val d = Dirichlet(3, 7)
              def transition = () => {
                val next = Universe.createNew
                val s = Select(d, true, false)("s",next)
                next
              }
              val algorithm = EMWithVE.online(transition, d)(initial)
                         algorithm.start()
              for (i <- 1 to 7) {
                algorithm.update(List(NamedEvidence("s", Observation(true))))
              }
              for (i <- 1 to 3) {
                algorithm.update(List(NamedEvidence("s", Observation(false))))
              }
              val result = d.MAPValue
              algorithm.kill
              result(0) should be(0.50 +- 0.01)

            }

        }        
          
          "used to estimate multiple parameters" should
            {

              "leave parameters having no observations unchanged" in
                {
                  val initial = Universe.createNew
                  val d = Dirichlet(2.0, 4.0, 2.0)
                  val b = Beta(2.0, 2.0)
                  val outcomes = List(1, 2, 3)

                  def transition = () => {
                    val next = Universe.createNew
                    val s = Select(d, outcomes:_*)("s",next)
                    next
                  }
                  val algorithm = EMWithVE.online(transition,d,b)(initial)
                             algorithm.start()
                  for (i <- 1 to 4) {
                    algorithm.update(List(NamedEvidence("s", Observation(1))))
                  }

                  for (i <- 1 to 2) {
                    algorithm.update(List(NamedEvidence("s", Observation(2))))
                  }

                  for (i <- 1 to 4) {
                    algorithm.update(List(NamedEvidence("s", Observation(3))))
                  }

               

                  val result = d.MAPValue
         
                  result(0) should be(0.33 +- 0.01)
                  result(1) should be(0.33 +- 0.01)
                  result(2) should be(0.33 +- 0.01)

                  val betaResult = b.MAPValue
                  betaResult should be(0.5)
                  algorithm.kill
                }

              "correctly estimate all parameters with observations" in
                {
                  val initial = Universe.createNew
                  val d = Dirichlet(2.0, 3.0, 2.0)
                  val b = Beta(3.0, 7.0)
                  val outcomes = List(1, 2, 3)
                  
                  def transition = () => {
                    val next = Universe.createNew
                    val s = Select(d, outcomes:_*)("s",next)
                    val f = Flip(b)("f",next)
                    next
                  }
                  val algorithm = EMWithVE.online(transition,d,b)(initial)
                             algorithm.start()
                  for (i <- 1 to 4) {
                    algorithm.update(List(NamedEvidence("s", Observation(1))))
                  }

                  for (i <- 1 to 2) {
                    algorithm.update(List(NamedEvidence("s", Observation(2))))
                  }

                  for (i <- 1 to 4) {
                    algorithm.update(List(NamedEvidence("s", Observation(3))))
                  }
                  
                  for (i <- 1 to 7) {
                    algorithm.update(List(NamedEvidence("f", Observation(true))))
                  }
                  
                  for (i <- 1 to 3) {
                    algorithm.update(List(NamedEvidence("f", Observation(false))))
                  }

                  val result = d.MAPValue

                  result(0) should be(0.35 +- 0.01)
                  result(1) should be(0.28 +- 0.01)
                  result(2) should be(0.36 +- 0.01)

                  val betaResult = b.MAPValue
                  betaResult should be(0.5 +- 0.01)
                 algorithm.kill
                }
            }
    }
}