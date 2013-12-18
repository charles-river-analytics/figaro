/*
 * Conversion.scala
 * AminoAcidSequence conversion methods
 * 
 * Created By:      Brian Ruttenberg (bruttenberg@cra.com)
 * Creation Date:   Oct 1, 2012
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.example.dosage

import com.cra.figaro.util._
import scala.language.implicitConversions

/**
 * A library of AminoAcidSequence conversion methods.
 */

object Conversion {

  val symbolToN = Map[String, List[String]](
    ("A" -> List("A")),
    ("C" -> List("C")),
    ("G" -> List("G")),
    ("U" -> List("U")),
    ("W" -> List("A", "U")),
    ("S" -> List("C", "G")),
    ("M" -> List("A", "C")),
    ("K" -> List("G", "U")),
    ("R" -> List("A", "G")),
    ("Y" -> List("C", "U")),
    ("B" -> List("C", "G", "U")),
    ("D" -> List("A", "G", "U")),
    ("H" -> List("A", "C", "U")),
    ("V" -> List("A", "C", "G")),
    ("N" -> List("A", "C", "G", "U")),
    ("*" -> List("A", "C", "G", "U")))

  val compToAA = Map[String, String](
    ("GCN" -> "A"), ("CGN" -> "R"), ("MGR" -> "R"), ("AAY" -> "N"),
    ("GAY" -> "D"), ("UGY" -> "C"), ("CAR" -> "Q"), ("GAR" -> "E"),
    ("GGN" -> "G"), ("CAY" -> "H"), ("AUH" -> "I"), ("YUR" -> "L"),
    ("CUN" -> "L"), ("AAR" -> "K"), ("AUG" -> "M"), ("UUY" -> "F"),
    ("CCN" -> "P"), ("UCN" -> "S"), ("AGY" -> "S"), ("ACN" -> "T"),
    ("UGG" -> "W"), ("UAY" -> "Y"), ("GUN" -> "V"),
    ("UAR" -> "Z"), ("URA" -> "Z"))

  lazy val aaToComp = compToAA groupBy { _._2 } map { case (key, value) => (key, value.unzip._1) }

  lazy val aaList = compToAA.values.toSet.toList
  lazy val aaListAsSeq = aaList.map(AminoAcidSequence(_))

  lazy val compToCodon = {
    var M = Map[String, List[String]]()
    compToAA.keys.foreach { k =>
      val l = k.toList.map(c => symbolToN(c.toString()))
      val prod = homogeneousCartesianProduct(l: _*)
      M += (k -> prod.map(v => v.reduce(_ + _)))
    }
    M
  }

  lazy val codonToComp = compToCodon flatMap (m => m._2.map(_ -> m._1))

  def symbolToAA(symbol: String) = symbol match {
    case "*" => aaList
    case _ => List(symbol)
  }

  implicit def toAminoAcidSequence(n: NucleotideSequence): AminoAcidSequence = {
    val aa = for { i <- 0 until n.seq.size by 3 } yield (compToAA(codonToComp(n.seq.substring(i, i + 3))))
    AminoAcidSequence(("" /: aa)(_ + _))
  }

}









