/**
 *
 */
package com.cra.figaro.util.visualization.histogram

import prefuse.data.Table
import prefuse.data.query.ObjectRangeModel

/**
 * @author Glenn Takata (gtakata@cra.com)
 *
 * Mar 17, 2015
 */
trait DataView {
  def name: String
  def title: String
  def range: ObjectRangeModel 
  
  def nValues: Int
  
  def getTable: Table

}