/*
 * TrainablePatterns.scala
 * Trait for classes which support training from observed data.   
 * 
 * Created By:      Dan Garant (dgarant@cra.com)
 * Creation Date:   May 20, 2016
 * 
 * Copyright 2016 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.patterns.learning

import com.cra.figaro.language.Element


/**
 * A conditional model which is trained based on observed data.
 * @param <Input> Type of one instance of the input data
 * @param <Output> Type of one instance of the output data
 */
trait TrainableModel[Input, Output] {
  
  /** Produce an output corresponding to the specified input */
  def model(input:Input):Element[Output]
  
  /** Fit the parameters of the model given a set of input, output pairs */
  def train(data:List[(Input, Output)])
  
}
