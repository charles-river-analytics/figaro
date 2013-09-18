/*
 * ScoringMatrix.scala
 * A scoring matrix.
 * 
 * Created By:      Brian Ruttenberg (bruttenberg@cra.com)
 * Creation Date:   Oct 1, 2012
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 */

package com.cra.figaro.example.dosage

import scala.io.Source

/**
 * A scoring matrix.
 */
object ScoringMatrix {

  val blosum62 = {
    var M = Map[Char, Map[Char, Int]]()
    val lines = for (line <- Source.fromFile("BLOSUM62.txt").getLines()) yield line
    val (aa_line, mtx_lines) = lines.toList.splitAt(1)
    val aa = aa_line(0).split(" +").toList.tail

    mtx_lines foreach { l =>
      var m1 = Map[Char, Int]()
      val line = l.split(" +").toList
      val scores = line.tail.zip(aa)
      scores foreach (s => m1 += (s._2(0) -> s._1.toInt))
      M += (line.head(0) -> m1)
    }

    // invert scoring matrix to make a minimization problem as NN algorithm works on min distance
    val maxScore = M.map(_._2.values.max).max
    M.foreach { s => M += (s._1 -> s._2.mapValues(i => -1 * i + maxScore + 1)) }
    M
  }

}