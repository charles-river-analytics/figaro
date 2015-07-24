/*
 * CacheTest.scala   
 * Needs description
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Jan 1, 2009
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.test.util

import org.scalatest.Matchers
import org.scalatest.WordSpec

import com.cra.figaro.language.CachingChain
import com.cra.figaro.language.Chain
import com.cra.figaro.language.Constant
import com.cra.figaro.language.Flip
import com.cra.figaro.language.Universe
import com.cra.figaro.library.atomic.continuous.Uniform
import com.cra.figaro.library.cache.MHCache
import com.cra.figaro.library.compound.If

class CacheTest extends WordSpec with Matchers {
  "A MH cache" should {
    "correctly retrieve cache elements for caching chains" in {
      val u = Universe.createNew()
      val cc = new MHCache(u)
      var sum = 0
      def fn(b: Boolean) = {
        sum += 1
        Constant(b)
      }
      val f = Flip(0.5)
      val c = CachingChain(f, fn)
      f.value = true; cc(c)
      f.value = false; cc(c)
      f.value = true; cc(c)
      sum should equal(2)
    }

    "keep the stack at maximum of two for non-caching chains" in {
      val u = Universe.createNew()
      val cc = new MHCache(u)
      val f = Uniform(0.0, 1.0)
      val c = Chain(f, (d: Double) => Constant(d))
      for { _ <- 0 until 10 } {
        f.generate
        cc(c)
      }
      cc.nccCache(c).size should equal(2)
    }

    "remove deactivated elements from the cache" in {
      val u = Universe.createNew()
      val cc = new MHCache(u)
      val a1 = Flip(0.1)
      val a2 = Flip(0.2)
      val s = Flip(0.5)
      val c = If(s, a1, a2)
      s.value = true; cc(c)
      s.value = false; cc(c)
      cc.ccCache(c).size should equal(2)
      a1.deactivate
      cc.ccCache(c).size should equal(1)
    }

    "correctly clear the context of elements removed from the stack" in {
      val u = Universe.createNew()
      val cc = new MHCache(u)
      def fn(d: Double) = {
        Flip(d); Flip(d); Flip(d)
      }      
      val f = Uniform(0.0, 1.0)            
      val c = Chain(f, fn)
      
      f.generate; cc(c)
      c.directContextContents.size should equal(3)
      f.generate; cc(c)
      c.directContextContents.size should equal(6)
      f.generate; cc(c)
      c.directContextContents.size should equal(6) 
      u.activeElements.size should equal(8)
    }
    
    "correctly clear the caches when clearing temporaries" in {
      val u = Universe.createNew()
      val cc = new MHCache(u)
      def fn(d: Double) = {
        Flip(d); Flip(d); Flip(d)
      }      
      val f = Uniform(0.0, 1.0)
      val perm = Constant(false)
      val fl = Flip(0.5)
      val c2 = CachingChain(fl, (b: Boolean) => {
        if (b) perm
        else {
          Flip(f)
        }
      })
      
      for {_ <- 0 until 100} {
        f.generate
        fl.generate       
        cc(c2)
      }      
      u.clearTemporaries 
      u.activeElements.size should equal(4)
      cc.ccCache(c2).size should equal(1)
      cc.ccInvertedCache(perm).size should equal (1)      
    }
  }
}
