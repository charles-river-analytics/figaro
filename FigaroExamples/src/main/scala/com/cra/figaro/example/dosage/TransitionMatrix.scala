/*
 * TransitionMatrix.scala
 * A transition matrix.
 * 
 * Created By:      Brian Ruttenberg (bruttenberg@cra.com)
 * Creation Date:   Oct 1, 2012
 * 
 * Copyright 2017 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.example.dosage

/**
 * A transition matrix.
 */
object TransitionMatrix {

  val bases = List("A", "C", "U", "G")

  def JC69(expChanges: Double): Map[String, List[(Double, String)]] = {
    var M = Map[String, List[(Double, String)]]()

    bases.foreach { b1 =>
      val l = bases.map { b2 =>
        if (b2 == b1)
          0.25 + 0.75 * math.pow(math.E, -4.0 * expChanges / 3)
        else
          0.25 - 0.25 * math.pow(math.E, -4.0 * expChanges / 3)
      }
      M += (b1 -> l.zip(bases))
    }
    M
  }

  def F81(freq: Map[String, Double], expChanges: Double): Map[String, List[(Double, String)]] = {
    var M = Map[String, List[(Double, String)]]()

    val beta = 1.0 / (1.0 - freq.values.map(math.pow(_, 2.0)).sum)

    bases.foreach { b1 =>
      val l = bases.map { b2 =>
        if (b2 == b1)
          freq(b1) + (1.0 - freq(b1)) * math.pow(math.E, -1.0 * beta * expChanges)
        else
          freq(b2) * (1.0 - math.pow(math.E, -1.0 * beta * expChanges))
      }
      M += (b1 -> l.zip(bases))
    }
    M
  }

}
