/*
 * ResultsView.scala 
 * A visual component to display a table of user data. Includes discrete (distribution List) and continuous (element)
 * 
 * Created By:      Glenn Takata (gtakata@cra.com)
 * Creation Date:   Mar 16, 2015
 * 
 * Copyright 2015 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */
package com.cra.figaro.util.visualization.results

import scala.collection.JavaConversions._
import prefuse.Constants
import prefuse.data.Table
import prefuse.data.Tuple
import prefuse.data.io.CSVTableReader
import prefuse.data.query.{NumberRangeModel, ObjectRangeModel}
import prefuse.util.ui.ValuedRangeModel
import scala.collection.JavaConversions

import com.cra.figaro.util.visualization.DataView

/**
 * @author Glenn Takata
 *
 * Mar 16, 2015
 */
class ResultsView[T](data: ResultsData) extends DataView {
  val name = data.name
  val title = name
  
  def nValues = data.distribution.size
  
  def range: ValuedRangeModel = {
    val values = JavaConversions.asJavaCollection(data.distribution.map(_._2)).toArray()
    new ObjectRangeModel(values.asInstanceOf[Array[Object]])
  }
 
  def getTable = readTable
  
  def readTable: Table = {
      val resultsTable = new Table()
      resultsTable.addColumn("Name", classOf[String])
      resultsTable.addColumn("Value", classOf[Object])
      resultsTable.addColumn("Probability", classOf[Float])
      
      resultsTable.addRows(nValues)
      
      var row = 0
      val name = data.name
      for (value <- data.distribution) {        
   
        resultsTable.set(row, "Name", name)
        resultsTable.set(row, "Value", value._2)
        resultsTable.set(row, "Probability", value._1)
        
        row += 1
      }
      resultsTable
  }
  
  def dataType = {
    data match {
      case ContinuousData(_, _) => Constants.NUMERICAL
      case _ => Constants.NOMINAL
    }
  }
  
  def yMax = math.min(1.1 * data.distribution.reduceLeft((a, b) => if (a._1 > b._1) a else b)._1, 1.0)
  def yRangeModel = {
    data match {
      case ContinuousData(_, _) => 
        new NumberRangeModel(0, yMax, 0, yMax )
      case _ => {
        if (yMax < 0.5)
          new NumberRangeModel(0, yMax, 0, yMax)
        else
          new NumberRangeModel(0, 1.0, 0, 1.0)
      }
    }
  }
}

class NoResult() extends ResultsView(DiscreteData("No Data", List((1.0, true))))