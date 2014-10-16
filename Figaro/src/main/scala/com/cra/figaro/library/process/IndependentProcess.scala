package com.cra.figaro.library.process

import com.cra.figaro.language._

trait IndependentProcess[Index, Value] extends Process[Index, Value] {
  val generator: Index => Element[Value] 

  def generate(index: Index) = generator(index)
  
  def generate(indices: List[Index]) = {
    Map(indices.map(index => (index, generator(index))):_*)
  }
}