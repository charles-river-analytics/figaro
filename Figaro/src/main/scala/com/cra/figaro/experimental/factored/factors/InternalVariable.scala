/*
 * Variable.scala
 * Variables that appear in factors.
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Jan 1, 2009
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.experimental.factored.factors

import com.cra.figaro.algorithm._
import com.cra.figaro.algorithm.factored.factors._
import com.cra.figaro.language._
import scala.collection.mutable.Map
import com.cra.figaro.algorithm.lazyfactored.{ LazyValues, Extended, ValueSet }

/**
 * Variables that are internal to Factors.
 * 
 * This is the same as a temporary variable, but is more explicitly identified
 *
 * @param range The range of values of the variable
 */

class InternalVariable[T](valueSet: ValueSet[T]) extends Variable(valueSet)
