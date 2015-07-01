/**
 *
 */
package com.cra.figaro.util.visualization.histogram

import java.awt.Graphics2D
import java.awt.Shape
import java.awt.geom.Rectangle2D

import prefuse.Constants
import prefuse.render.AbstractShapeRenderer
import prefuse.util.ColorLib
import prefuse.util.GraphicsLib
import prefuse.visual.VisualItem

import com.cra.figaro.util.ColorGradient

/**
 * @author Glenn Takata
 *
 */
class HistogramRenderer(color: String, nBars: Int = 5) extends AbstractShapeRenderer {
  var bounds: Rectangle2D = _
  var isVertical: Boolean = true
  var orientation = Constants.ORIENT_BOTTOM_TOP;
  var barWidth: Int = 5
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

    val values = item.getSourceTuple

    gradient.getColorAtValue(values.getFloat("Probability")) match {
      case Some(color) =>
        item.setFillColor(ColorLib.rgba(color.red, color.green, color.blue, 1))
        GraphicsLib.paint(g, item, shape, null, AbstractShapeRenderer.RENDER_TYPE_FILL);
      case _ =>
    }

  }

}