/*
 * ChainFactorTest.scala 
 * ChainFactor tests.
 * 
 * Created By:      Glenn Takata (gtakata@cra.com)
 * Creation Date:   Mar 24, 2015
 * 
 * Copyright 2015 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.test.experimental.factored.factors.factory

import org.scalatest.Matchers
import org.scalatest.PrivateMethodTester
import org.scalatest.WordSpec
import com.cra.figaro.algorithm.Values
import com.cra.figaro.algorithm.factored.factors.{SumProductSemiring, Variable}
import com.cra.figaro.algorithm.lazyfactored.LazyValues
import com.cra.figaro.algorithm.lazyfactored.Regular
import com.cra.figaro.algorithm.lazyfactored.ValueSet
import com.cra.figaro.algorithm.sampling.ProbEvidenceSampler
import com.cra.figaro.language.Apply
import com.cra.figaro.language.Apply3
import com.cra.figaro.language.Apply4
import com.cra.figaro.language.Apply5
import com.cra.figaro.language.CachingChain
import com.cra.figaro.language.Chain
import com.cra.figaro.language.Condition
import com.cra.figaro.language.Constant
import com.cra.figaro.language.Dist
import com.cra.figaro.language.Flip
import com.cra.figaro.language.Inject
import com.cra.figaro.language.Name.stringToName
import com.cra.figaro.language.NamedEvidence
import com.cra.figaro.language.Reference.stringToReference
import com.cra.figaro.language.Select
import com.cra.figaro.language.Universe
import com.cra.figaro.library.atomic.continuous.Normal
import com.cra.figaro.library.atomic.continuous.Uniform
import com.cra.figaro.library.compound.CPD
import com.cra.figaro.algorithm.factored.ParticleGenerator
import com.cra.figaro.experimental.factored.factors.{Factory}

class ChainFactorTest extends WordSpec with Matchers with PrivateMethodTester {

  "Making factors from an element" when {

    "given a chain" should {
      "produce a conditional selector for each parent value" in {
        Universe.createNew()
        val v1 = Flip(0.2)
        val v2 = Select(0.1 -> 1, 0.9 -> 2)
        val v3 = Constant(3)
        val v4 = Chain(v1, (b: Boolean) => if (b) v2; else v3)
        Values()(v4)
        val v1Vals = Variable(v1).range
        val v2Vals = Variable(v2).range
        val v4Vals = Variable(v4).range
        val v1t = v1Vals indexOf Regular(true)
        val v1f = v1Vals indexOf Regular(false)
        val v21 = v2Vals indexOf Regular(1)
        val v22 = v2Vals indexOf Regular(2)
        val v41 = v4Vals indexOf Regular(1)
        val v42 = v4Vals indexOf Regular(2)
        val v43 = v4Vals indexOf Regular(3)

        val factor = Factory.make(v4)
        val List(v4Factor) = Factory.combineFactors(factor, SumProductSemiring, true)
        val vars = v4Factor.variables

        v4Factor.get(List(v1t, 0, v21, v41)) should equal(1.0)
        v4Factor.get(List(v1t, 0, v22, v41)) should equal(0.0)
        v4Factor.get(List(v1t, 0, v21, v42)) should equal(0.0)
        v4Factor.get(List(v1t, 0, v22, v42)) should equal(1.0)
        v4Factor.get(List(v1t, 0, v21, v43)) should equal(0.0)
        v4Factor.get(List(v1t, 0, v22, v43)) should equal(0.0)
        v4Factor.get(List(v1f, 0, v21, v41)) should equal(0.0)
        v4Factor.get(List(v1f, 0, v22, v41)) should equal(0.0)
        v4Factor.get(List(v1f, 0, v21, v42)) should equal(0.0)
        v4Factor.get(List(v1f, 0, v22, v42)) should equal(0.0)
        v4Factor.get(List(v1f, 0, v21, v43)) should equal(1.0)
        v4Factor.get(List(v1f, 0, v22, v43)) should equal(1.0)

      }

      "produce a conditional selector for each non-temporary parent value" in {
        Universe.createNew()
        val v1 = Flip(0.2)
        val v4 = Chain(v1, (b: Boolean) => if (b) Select(0.1 -> 1, 0.9 -> 2); else Constant(3))
        Values()(v4)
        val v1Vals = Variable(v1).range
        val v4Vals = Variable(v4).range

        val v1t = v1Vals indexOf Regular(true)
        val v1f = v1Vals indexOf Regular(false)
        val v41 = v4Vals indexOf Regular(1)
        val v42 = v4Vals indexOf Regular(2)
        val v43 = v4Vals indexOf Regular(3)

        val factor = Factory.make(v4)
        val List(v4Factor) = Factory.combineFactors(factor, SumProductSemiring, true)

        v4Factor.get(List(v1t, v41)) should equal(0.1)
        v4Factor.get(List(v1t, v42)) should equal(0.9)
        v4Factor.get(List(v1t, v43)) should equal(0.0)
        v4Factor.get(List(v1f, v41)) should equal(0.0)
        v4Factor.get(List(v1f, v42)) should equal(0.0)
        v4Factor.get(List(v1f, v43)) should equal(1.0)
      }
    }
  }
}
