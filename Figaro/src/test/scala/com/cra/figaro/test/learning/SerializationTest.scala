/*
 * SerializationTest.scala 
 * TBD needs description
 * 
 * Created By:      Michael Howard (mhoward@cra.com)
 * Creation Date:   Mar 5, 2015
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */
package com.cra.figaro.test.learning

import org.scalatest.WordSpec
import org.scalatest.Matchers
import com.cra.figaro.algorithm.learning._
import com.cra.figaro.language._
import com.cra.figaro.library.atomic.continuous._
import com.cra.figaro.library.compound._
import argonaut._, Argonaut._
import com.cra.figaro.library.atomic.continuous.Dirichlet
import com.cra.figaro.library.atomic.continuous.Beta
import com.cra.figaro.patterns.learning._
class SerializationTest extends WordSpec with Matchers {
  
  "A Beta parameter" should {
    "serialize its name and alpha and beta values" in {
      val u = Universe.createNew()
      val b = Beta(1,5)("b", u)
      val jsonBeta = b.asJson
      println(jsonBeta.spaces2)
      val name = jsonBeta.field("name").get.as[String].value.get
      val alpha = jsonBeta.field("aValue").get.as[Double].value.get
      val beta = jsonBeta.field("bValue").get.as[Double].value.get

      name should be("b")
      alpha should be(1.0)
      beta should be(5.0)
    }

    "deserialize its name, alpha and beta values from JSON" in{
      val jsonBeta = ("name" := "b") ->: ("aValue" := 1.0) ->: ("bValue" := 5.0) ->: jEmptyObject
      val b = jsonBeta.asJson.as[AtomicBeta].value.get
      b.name.string should be("b")
      b.aValue should be(1)
      b.bValue should be(5.0)
    }

  }

  "A Dirichlet parameter" should {
    "serialize its name and concentration parameters" in{
      val u = Universe.createNew()
     
      val d = Dirichlet(1,2,3,4,5)("d", u)
      val jsonDirichlet = d.asJson
      println(jsonDirichlet.spaces2)
      val name = jsonDirichlet.field("name").get.as[String].value.get
      val concentrationParameters = jsonDirichlet.field("alphaValues").get.as[List[Double]].value.get

      name should be("d")
      concentrationParameters(0) should be(1.0)
      concentrationParameters(1) should be(2.0)
      concentrationParameters(2) should be(3.0)
      concentrationParameters(3) should be(4.0)
      concentrationParameters(4) should be(5.0)
    }

    "deserialize its name and concentration parameters from JSON" in{
      val jsonDirichlet = ("name" := "d") ->: ("alphaValues" := List(1.0,2.0,3.0,4.0,5.0)) ->: jEmptyObject
      val d = jsonDirichlet.as[AtomicDirichlet].value.get
      d.name.string should be("d")
      d.concentrationParameters(0) should be(1.0)
      d.concentrationParameters(1) should be(2.0)
      d.concentrationParameters(2) should be(3.0)
      d.concentrationParameters(3) should be(4.0)
      d.concentrationParameters(4) should be(5.0)
    }

  }

  "A parameter collection" should {
    "serialize and deserialize set of parameters" in{
      val m = ModelParameters()
      val b1 = Beta(1,5)("b1",m)
      val d1 = Dirichlet(3,2,4,1)("d1",m)
      val b2 = Beta(2,2)("b2",m)
      val d2 = Dirichlet(1,2,3,4,5,6)("d2",m)
      val jsonParameters = m.asJson
      
      val u = Universe.createNew()
      val m2 = jsonParameters.as[ModelParameters].value.get
      
      //Note: This was intended to conceal any matching/casting behind the scenes
      //But it really makes accessing the values directly kind of clunky.
      //A better method would be a 'toDouble' or 'toArray' method
      //which throws an exception if the parameter matching the name is the wrong type
      //it would remove a lot of this ugliness.
      val b1MAP = m2.posteriorParameters.get("b1") match {
        case p: PrimitiveDouble => p.d
        case default => throw new IllegalArgumentException("Cannot MAP value from parameter collection.")
      }

      val d1MAP = m2.posteriorParameters.get("d1") match {
        case p: PrimitiveArray => p.a
        case default => throw new IllegalArgumentException("Cannot MAP value from parameter collection.")
      }
      val b2MAP = m2.posteriorParameters.get("b2") match {
        case p: PrimitiveDouble => p.d
        case default => throw new IllegalArgumentException("Cannot MAP value from parameter collection.")
      }
      val d2MAP = m2.posteriorParameters.get("d2") match {
        case p: PrimitiveArray => p.a
        case default => throw new IllegalArgumentException("Cannot MAP value from parameter collection.")
      }
      
      b1MAP should be(b1.MAPValue)
      d1MAP.length should be(d1.MAPValue.length)
      d1MAP(0) should be(d1.MAPValue(0))
      d1MAP(1) should be(d1.MAPValue(1))
      d1MAP(2) should be(d1.MAPValue(2))
      d1MAP(3) should be(d1.MAPValue(3))
      b2MAP should be(b2.MAPValue)
      d2MAP.length should be(d2.MAPValue.length)
      d2MAP(0) should be(d2.MAPValue(0))
      d2MAP(1) should be(d2.MAPValue(1))
      d2MAP(2) should be(d2.MAPValue(2))
      d2MAP(3) should be(d2.MAPValue(3))
      d2MAP(4) should be(d2.MAPValue(4))
      d2MAP(5) should be(d2.MAPValue(5))
    }

    
  }

}
