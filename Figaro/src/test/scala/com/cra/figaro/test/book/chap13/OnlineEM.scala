/*
 * OnlineEM.scala 
 * Book example unit test.
 * 
 * Created By:      Michael Reposa (mreposa@cra.com), Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Feb 26, 2016
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.test.book.chap13

import com.cra.figaro.algorithm.learning.EMWithVE
import com.cra.figaro.patterns.learning.{ModelParameters, ParameterCollection}
import com.cra.figaro.language._
import com.cra.figaro.library.atomic.continuous.Dirichlet
import com.cra.figaro.algorithm.factored.VariableElimination
import org.scalatest.Matchers
import org.scalatest.WordSpec
import com.cra.figaro.test.tags.BookExample

object OnlineEM {
  def main(args: Array[String]) {
    val parameters = ModelParameters()
    val d = Dirichlet(2.0,2.0,2.0)("d",parameters)

    class Model(parameters: ParameterCollection, modelUniverse: Universe) {
      val s = Select(parameters.get("d"), 1, 2, 3)("s", modelUniverse)
    }

    def f = () => {
      val modelUniverse = new Universe
      new Model(parameters.priorParameters, modelUniverse)
      modelUniverse
    }

    val em = EMWithVE.online(f, parameters)
    em.start()

    for (i <- 1 to 100) {
      val evidence = List(NamedEvidence("s", Observation(1)))
      em.update(evidence)
    }

    val futureUniverse1 = Universe.createNew()
    val futureModel1 = new Model(parameters.posteriorParameters, futureUniverse1)
    println(VariableElimination.probability(futureModel1.s, 1))

    for (i <- 101 to 200) {
      val evidence = List(NamedEvidence("s", Observation(2)))
      em.update(evidence)
    }

    val futureUniverse2 = Universe.createNew()
    val futureModel2 = new Model(parameters.posteriorParameters, futureUniverse2)
    println(VariableElimination.probability(futureModel2.s, 1))
  }
}

class OnlineEMTest extends WordSpec with Matchers {
    val parameters = ModelParameters()
    val d = Dirichlet(2.0,2.0,2.0)("d",parameters)

    class Model(parameters: ParameterCollection, modelUniverse: Universe) {
      val s = Select(parameters.get("d"), 1, 2, 3)("s", modelUniverse)
    }

    def f = () => {
      val modelUniverse = new Universe
      new Model(parameters.priorParameters, modelUniverse)
      modelUniverse
    }

    val em = EMWithVE.online(f, parameters)
    em.start()

    for (i <- 1 to 100) {
      val evidence = List(NamedEvidence("s", Observation(1)))
      em.update(evidence)
    }

    val futureUniverse1 = Universe.createNew()
    val futureModel1 = new Model(parameters.posteriorParameters, futureUniverse1)
    val result1 = VariableElimination.probability(futureModel1.s, 1)

    for (i <- 101 to 200) {
      val evidence = List(NamedEvidence("s", Observation(2)))
      em.update(evidence)
    }

    val futureUniverse2 = Universe.createNew()
    val futureModel2 = new Model(parameters.posteriorParameters, futureUniverse2)
    val result2 = VariableElimination.probability(futureModel2.s, 1)
  
    "Online EM" should {
      "produce a probability for future model 1 = 0.9805825242718447" taggedAs (BookExample) in {
        result1 should be (0.9805825242718447)
      }
      "produce a probability for future model 2 = 0.4975369458128079" taggedAs (BookExample) in {
        result2 should be (0.4975369458128079)
      }
    }
}
