package com.cra.figaro.util.visualization.results

/**
 * @author gtakata
 */
case class ResultsData[T](val name: String, val distribution: List[(Double, T)]) {
  def resultString: String = {
    val buffer = new StringBuffer("{")
    for ((prob, value) <- distribution) {
      buffer.append("(").append("%06.4f".format(prob)).append("->").append(value).append(")")
    }    
    buffer.append("}")
    return buffer.toString()
  }
}