/*
 * Test.scala   
 * Test runner for com.cra.figaro.test.util.
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Jan 1, 2009
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 */

package com.cra.figaro.test.util

private object Test {
  def main(args: Array[String]) = {
    (new UtilTest).execute()
    (new SelectableSetTest).execute()
    (new PriorityMapTest).execute()
    (new ResamplerTest).execute()
  }
}