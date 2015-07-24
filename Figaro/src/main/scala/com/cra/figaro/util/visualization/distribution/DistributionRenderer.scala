/*
 * DistributionRenderer.scala 
 * Display distribution elements based on position, value, color gradient
 * 
 * Created By:      Glenn Takata (gtakata@cra.com)
 * Creation Date:   Jul 6, 2015
 * 
 * Copyright 2015 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */
package com.cra.figaro.util.visualization.distribution

import java.awt.Graphics2D
import java.awt.Shape
import java.awt.geom.Rectangle2D

import prefuse.Constants
import prefuse.render.AbstractShapeRenderer
import prefuse.util.ColorLib
import prefuse.util.GraphicsLib
import prefuse.visual.VisualItem

import com.cra.figaro.util.ColorGradient
import com.cra.figaro.util.visualization.DataView

/**
 * @author Glenn Takata
 *
 */
class DistributionRenderer(color: String, dataview: DataView) extends AbstractShapeRenderer {
  var bounds: Rectangle2D = _
  var isVertical: Boolean = true
  var orientation = Constants.ORIENT_BOTTOM_TOP;
  var barWidth: Int = 5
  var nBars: Int = dataview.nValues
  var pMax: Double = dataview.yMax
  var rect = new Rectangle2D.Double();

  val gradient = new ColorGradient
  gradient.setGradient(color)

  def setBounds(newBounds: Rectangle2D) {
    bounds = newBounds;
    barWidth = (bounds.getWidth / nBars).toInt
  }

  def setOrientation(orient: Int) {

    if (orient != Constants.ORIENT_LEFT_RIGHT &&
      orient != Constants.ORIENT_RIGHT_LEFT)
    {
      throw new IllegalArgumentException(
        "Invalid orientation value: " + orient);
    }
    orientation = orient;
    isVertical = (orientation == Constants.ORIENT_TOP_BOTTOM ||
      orientation == Constants.ORIENT_BOTTOM_TOP);
  }

  override def getRawShape(item: VisualItem): Shape = {
    var width: Double = 0
    var height: Double = 0

    var x = item.getX()
    var y = item.getY()

    width = math.min(bounds.getWidth / nBars, 30)
    height = bounds.getHeight - y

    // Center the bar around the x-location
    if (width > 1) {
      x = x - width / 2;

    }

    rect.setFrame(x, y, width, height);
    return rect;

  }

  override def render(g: Graphics2D, item: VisualItem) {
    val shape = getShape(item);

    val probability = item.getSourceTuple.getFloat("Probability")
    val value = probability / pMax

    gradient.getColorAtValue(value.floatValue()) match {
      case Some(color) =>
        item.setFillColor(ColorLib.rgba(color.red, color.green, color.blue, 1))
        GraphicsLib.paint(g, item, shape, null, AbstractShapeRenderer.RENDER_TYPE_FILL);
      case _ =>
    }

  }

}