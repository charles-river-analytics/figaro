/*
 * SufficientStatisticsVariableEliminationTest.scala
 * Tests for sufficient statistics variable elimination
 * 
 * Created By:      Michael Howard (mhoward@cra.com)
 * Creation Date:   Jun 6, 2013
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.test.algorithm.learning

import org.scalatest.Matchers
import org.scalatest.{ PrivateMethodTester, WordSpec }
import com.cra.figaro.algorithm.learning._
import com.cra.figaro.algorithm._
import com.cra.figaro.algorithm.factored._
import com.cra.figaro.algorithm.factored.factors._
import com.cra.figaro.algorithm.lazyfactored._
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.language._
import com.cra.figaro.language.Universe._
import com.cra.figaro.library.atomic.continuous._
import com.cra.figaro.util
import scala.collection._

class SufficientStatisticsVariableEliminationTest extends WordSpec with PrivateMethodTester with Matchers {

  "Sufficient Statistics Variable Elimination" when
    {

      "provided with a semiring and set of parameters" should
        {

          "create correct factors which include sufficient statistics for a beta parameter" in
            {
              val universe = Universe.createNew
              val f1 = Flip(0.5)
              val f2 = Flip(0.2)
              val p1 = Beta(1, 1)
              val f3 = Flip(p1)
              val targetParameters = List(p1)

              val emptyMap : mutable.Map[Parameter[_], Seq[Double]] = mutable.Map.empty[Parameter[_], Seq[Double]]

              for (p <- targetParameters) {
                emptyMap += p -> p.zeroSufficientStatistics
              }

              //Defines the length of the sequences corresponding to different parameters
              val paramMap = emptyMap.toMap
              val zero = (0.0, emptyMap)
              val one = (1.0, emptyMap)

              val algorithm = SufficientStatisticsVariableElimination(paramMap)
              val (elements, _) = algorithm.getNeededElements(List(f1,f2,p1,f3), Int.MaxValue) 
              val values = Values(universe)
              values(f1)
              values(f2)
              values(p1)
              values(f3)
              val factors = algorithm.getFactors(elements, elements)

              for (factor <- factors) {
                //println(factor.toReadableString)

                if (factor.variables.contains(Variable(f1)) == true) {
                  factor.getIndices.foreach(a => factor.get(a)._2 should equal(zero._2))
                } else if (factor.variables.contains(Variable(f1)) == true) {
                  factor.getIndices.foreach(a => factor.get(a)._2 should equal(zero._2))
                } else if (factor.variables.contains(Variable(f3)) == true) {
                  val i = Variable(f3).range.indexOf(Regular(true))
                  //println(factor.get(List(i))._2(p1))
                  //println(factor.get(List(i + 1))._2(p1))
                }
              }

            }

          "create correct factors which include sufficient statistics for a Dirichlet parameter with two concentration parameters" in
            {
              val universe = Universe.createNew
              val f1 = Select(0.5 -> true, 0.5 -> false)
              val f2 = Select(0.2 -> true, 0.8 -> false)
              val p1 = Dirichlet(1, 1)
              val f3 = Select(p1, true, false)
              val targetParameters = List(p1)

              val emptyMap : mutable.Map[Parameter[_], Seq[Double]] = mutable.Map.empty[Parameter[_], Seq[Double]]

              for (p <- targetParameters) {
                emptyMap += p -> p.zeroSufficientStatistics
              }

              //Defines the length of the sequences corresponding to different parameters
              val paramMap = emptyMap.toMap
              val zero = (0.0, emptyMap)
              val one = (1.0, emptyMap)

              val algorithm = SufficientStatisticsVariableElimination(paramMap)
              val (elements, _) = algorithm.getNeededElements(List(f1,f2,p1,f3), Int.MaxValue) 
              val values = Values(universe)
              values(f1)
              values(f2)
              values(p1)
              values(f3)
              val factors = algorithm.getFactors(elements, elements)
              
              for (factor <- factors) {

                if (factor.variables.contains(Variable(f1)) == true) {
                  factor.getIndices.foreach(a => factor.get(a)._2 should equal(zero._2))
                } else if (factor.variables.contains(Variable(f1)) == true) {
                  factor.getIndices.foreach(a => factor.get(a)._2 should equal(zero._2))
                } else if (factor.variables.contains(Variable(f3)) == true) {

                  factor.getIndices.foreach(a => factor.get(a)._2.contains(p1) should equal(true))
                  val i = Variable(f3).range.indexOf(Regular(true))
                  factor.get(List(i))._2(p1) should equal(Seq(1.0, 0.0))
                  factor.get(List(i + 1))._2(p1) should equal(Seq(0.0, 1.0))
                }
              }

            }

          "create correct factors which include sufficient statistics for a Dirichlet parameter with three concentration parameters" in
            {
              val universe = Universe.createNew
              val f1 = Select(0.5 -> 1, 0.4 -> 2, 0.1 -> 3)
              val f2 = Select(0.1 -> 1, 0.8 -> 2, 0.1 -> 3)
              val p1 = Dirichlet(1, 1, 1)
              val f3 = Select(p1, 1, 2, 3)
              val targetParameters = List(p1)

              val emptyMap : mutable.Map[Parameter[_], Seq[Double]] = mutable.Map.empty[Parameter[_], Seq[Double]]

              for (p <- targetParameters) {
                emptyMap += p -> p.zeroSufficientStatistics
              }

              //Defines the length of the sequences corresponding to different parameters
              val paramMap = emptyMap.toMap
              val zero = (0.0, emptyMap)
              val one = (1.0, emptyMap)

              val algorithm = SufficientStatisticsVariableElimination(paramMap)
              val (elements, _) = algorithm.getNeededElements(List(f1,f2,p1,f3), Int.MaxValue) 
              val values = Values(universe)
              values(f1)
              values(f2)
              values(p1)
              values(f3)
              val factors = algorithm.getFactors(elements, elements)
              for (factor <- factors) {

                if (factor.variables.contains(Variable(f1)) == true) {
                  factor.getIndices.foreach(a => factor.get(a)._2 should equal(zero._2))
                } else if (factor.variables.contains(Variable(f1)) == true) {
                  factor.getIndices.foreach(a => factor.get(a)._2 should equal(zero._2))
                } else if (factor.variables.contains(Variable(f3)) == true) {

                  factor.getIndices.foreach(a => factor.get(a)._2.contains(p1) should equal(true))
                  val i = Variable(f3).range.indexOf(Regular(1))
                  factor.get(List(i))._2(p1) should equal(Seq(1.0, 0.0, 0.0))
                  factor.get(List(i + 1))._2(p1) should equal(Seq(0.0, 1.0, 0.0))
                  factor.get(List(i + 2))._2(p1) should equal(Seq(0.0, 0.0, 1.0))
                }
              }

            }

          "correctly deduce sufficient statistics for a beta parameter" in
            {
              val universe = Universe.createNew
              val f1 = Flip(0.9)
              val p1 = Beta(12, 12)
              val f2 = Flip(p1)
              f2.observe(true)

              val targetParameters = List(p1)

              val emptyMap = mutable.Map.empty[Parameter[_], Seq[Double]]

              for (p <- targetParameters) {
                emptyMap += p -> p.zeroSufficientStatistics

              }

              //Defines the length of the sequences corresponding to different parameters
              val paramMap = emptyMap.toMap
              val zero = (0.0, emptyMap)
              val one = (1.0, emptyMap)

              val algorithm = SufficientStatisticsVariableElimination(paramMap)
              algorithm.start
              val map = algorithm.getSufficientStatisticsForAllParameters
              val sufficientStatisticsForBeta = map(p1)
              println(sufficientStatisticsForBeta)
              sufficientStatisticsForBeta(0) should be(1.0 +- 0.01)
              sufficientStatisticsForBeta(1) should be(0.0 +- 0.01)

            }

          "correctly deduce sufficient statistics for a Dirichlet parameter with two concentration parameters" in
            {
              val universe = Universe.createNew
              val f1 = Select(0.5 -> true, 0.5 -> false)
              val f2 = Select(0.2 -> true, 0.8 -> false)
              val p1 = Dirichlet(1, 1)
              val f3 = Select(p1, true, false)
              f3.observe(true)
              val targetParameters = List(p1)

              val emptyMap = mutable.Map.empty[Parameter[_], Seq[Double]]

              for (p <- targetParameters) {
                emptyMap += p -> p.zeroSufficientStatistics
              }

              //Defines the length of the sequences corresponding to different parameters
              val paramMap = emptyMap.toMap
              val zero = (0.0, emptyMap)
              val one = (1.0, emptyMap)

              val algorithm = SufficientStatisticsVariableElimination(paramMap)
              algorithm.start
              val map = algorithm.getSufficientStatisticsForAllParameters
              val sufficientStatisticsForBeta = map(p1)
              sufficientStatisticsForBeta(0) should be(1.0 +- 0.01)
              sufficientStatisticsForBeta(1) should be(0.0 +- 0.01)

            }

          "correctly deduce sufficient statistics for a Dirichlet parameter with three concentration parameters" in
            {
              val universe = Universe.createNew
              val f1 = Select(0.5 -> 1, 0.4 -> 2, 0.1 -> 3)
              val f2 = Select(0.1 -> 1, 0.8 -> 2, 0.1 -> 3)
              val p1 = Dirichlet(1, 1, 1)
              val f3 = Select(p1, 1, 2, 3)

              f3.observe(1)

              val targetParameters = List(p1)

              val emptyMap = mutable.Map.empty[Parameter[_], Seq[Double]]

              for (p <- targetParameters) {
                emptyMap += p -> p.zeroSufficientStatistics
              }

              //Defines the length of the sequences corresponding to different parameters
              val paramMap = emptyMap.toMap
              val zero = (0.0, emptyMap)
              val one = (1.0, emptyMap)

              val algorithm = SufficientStatisticsVariableElimination(paramMap)
              algorithm.start
              val map = algorithm.getSufficientStatisticsForAllParameters
              val sufficientStatisticsForBeta = map(p1)
              //EM will remember the prior probabilities
              //We haven't seen any '2's or '3's, so the sufficient statistics are zero. 
              sufficientStatisticsForBeta(0) should be(1.0 +- 0.01)
              sufficientStatisticsForBeta(1) should be(0.0 +- 0.01)
              sufficientStatisticsForBeta(2) should be(0.0 +- 0.01)
            }

          "correctly deduce sufficient statistics for a Dirichlet parameter with three concentration parameters and two observations" in
            {
              val universe = Universe.createNew
              val f1 = Select(0.5 -> 1, 0.4 -> 2, 0.1 -> 3)
              val f2 = Select(0.1 -> 1, 0.8 -> 2, 0.1 -> 3)
              val p1 = Dirichlet(1, 1, 1)
              val f3 = Select(p1, 1, 2, 3)

              f3.observe(1)
              val f4 = Select(p1, 1, 2, 3)
              f4.observe(2)

              val targetParameters = List(p1)

              val emptyMap = mutable.Map.empty[Parameter[_], Seq[Double]]

              for (p <- targetParameters) {
                emptyMap += p -> p.zeroSufficientStatistics
              }

              //Defines the length of the sequences corresponding to different parameters
              val paramMap = emptyMap.toMap
              val zero = (0.0, emptyMap)
              val one = (1.0, emptyMap)

              val algorithm = SufficientStatisticsVariableElimination(paramMap)
              algorithm.start
              val map = algorithm.getSufficientStatisticsForAllParameters
              val sufficientStatisticsForBeta = map(p1)
              //EM will remember the prior probabilities
              //We haven't seen any 3s, so the result is zero. 
              sufficientStatisticsForBeta(0) should be(1.0 +- 0.01)
              sufficientStatisticsForBeta(1) should be(1.0 +- 0.01)
              sufficientStatisticsForBeta(2) should be(0.0 +- 0.01)
            }

          "correctly deduce sufficient statistics for a Dirichlet parameter with three concentration parameters and three observations" in
            {
              val universe = Universe.createNew
              val f1 = Select(0.5 -> 1, 0.4 -> 2, 0.1 -> 3)
              val f2 = Select(0.1 -> 1, 0.8 -> 2, 0.1 -> 3)
              val p1 = Dirichlet(1, 1, 1)
              val f3 = Select(p1, 1, 2, 3)

              f3.observe(1)
              val f4 = Select(p1, 1, 2, 3)
              f4.observe(2)
              val f5 = Select(p1, 1, 2, 3)
              f5.observe(3)

              val targetParameters = List(p1)

              val emptyMap = mutable.Map.empty[Parameter[_], Seq[Double]]

              for (p <- targetParameters) {
                emptyMap += p -> p.zeroSufficientStatistics
              }

              //Defines the length of the sequences corresponding to different parameters
              val paramMap = emptyMap.toMap
              val zero = (0.0, emptyMap)
              val one = (1.0, emptyMap)

              val algorithm = SufficientStatisticsVariableElimination(paramMap)
              algorithm.start
              val map = algorithm.getSufficientStatisticsForAllParameters
              val sufficientStatisticsForBeta = map(p1)

              //We have seen an equal number of 1s, 2s and 3s.
              sufficientStatisticsForBeta(0) should be(1.0 +- 0.01)
              sufficientStatisticsForBeta(1) should be(1.0 +- 0.01)
              sufficientStatisticsForBeta(2) should be(1.0 +- 0.01)
            }

          "correctly deduce sufficient statistics for a Dirichlet parameter with three concentration parameters and four observations" in
            {
              val universe = Universe.createNew
              val f1 = Select(0.5 -> 1, 0.4 -> 2, 0.1 -> 3)
              val f2 = Select(0.1 -> 1, 0.8 -> 2, 0.1 -> 3)
              val p1 = Dirichlet(1, 1, 1)
              val f3 = Select(p1, 1, 2, 3)

              f3.observe(1)
              val f4 = Select(p1, 1, 2, 3)
              f4.observe(2)
              val f5 = Select(p1, 1, 2, 3)
              f5.observe(3)

              val f6 = Select(p1, 1, 2, 3)
              f6.observe(1)

              val targetParameters = List(p1)

              val emptyMap = mutable.Map.empty[Parameter[_], Seq[Double]]

              for (p <- targetParameters) {
                emptyMap += p -> p.zeroSufficientStatistics
              }

              //Defines the length of the sequences corresponding to different parameters
              val paramMap = emptyMap.toMap
              val zero = (0.0, emptyMap)
              val one = (1.0, emptyMap)

              val algorithm = SufficientStatisticsVariableElimination(paramMap)
              algorithm.start
              val map = algorithm.getSufficientStatisticsForAllParameters
              val sufficientStatisticsForBeta = map(p1)

              sufficientStatisticsForBeta(0) should be(2.0 +- 0.01)
              sufficientStatisticsForBeta(1) should be(1.0 +- 0.01)
              sufficientStatisticsForBeta(2) should be(1.0 +- 0.01)
            }

        }

    }

}
