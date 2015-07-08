/*
 * DataView.scala 
 * Internal data repository/method for use by tables, charts, histograms, etc
 * 
 * Created By:      Glenn Takata (gtakata@cra.com)
 * Creation Date:   Apr 9, 2015
 * 
 * Copyright 2015 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */
package com.cra.figaro.util.visualization

import prefuse.data.Table
import prefuse.data.query.NumberRangeModel
import prefuse.util.ui.ValuedRangeModel

/**
 * @author Glenn Takata (gtakata@cra.com)
 *
 * Mar 17, 2015
 */
trait DataView {
  def name: String
  def title: String
  def range: ValuedRangeModel 
  
  def nValues: Int
  
  def getTable: Table

  def yMax: Double
  def dataType: Int
  def yRangeModel: NumberRangeModel
}