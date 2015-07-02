/**
 *
 */
package com.cra.figaro.util

import scala.collection.mutable.ListBuffer

/**
 * @author Glenn Takata
 *
 */
case class ColorPoint(red: Float, green: Float, blue: Float, value: Float)

class ColorGradient {
  var selectedGradient = ColorGradient.MONOCHROME
  var gradients: Map[String, List[ColorPoint]] = Map()
  
  buildGradientMap
  
  def setGradient(gradient: String) {
    selectedGradient = gradient
  }
  
  def buildGradientMap {
    gradients += (ColorGradient.HEATMAP -> heatMap, 
        ColorGradient.MONOCHROME -> monochromeMap, 
        ColorGradient.TWOCOLOR -> twoColorMap,
        ColorGradient.SINGLECOLOR -> singleColor
    )
  }
  
  // A buffer of ColorPoints in ascending order of cumulative value (probability)
  def currentGradient: List[ColorPoint] = gradients(selectedGradient)
  
//  // Inserts a new color point into its correct position in the heatmap:
//  def addColorPoint(red: Float, green: Float, blue: Float, value: Float) {
//    heatmap.find(_.value > value) match {
//      case Some(c) =>
//        val index = heatmap.indexOf(c)
//        heatmap.insert(index - 1, ColorPoint(red, green, blue, value))
//      case _ =>
//        heatmap += ColorPoint(red, green, blue, value);
//    }
//  }
//
//  // Clears colormap
//  def clearGradient() { heatmap.clear(); }

  //-- Inputs a (value) between 0 and 1 and outputs the (red), (green) and (blue)
  //-- values representing that position in the gradient.
  def getColorAtValue(value: Float): Option[ColorPoint] = {
    if (currentGradient.isEmpty) {
      None
    } else {
      var previousPoint = currentGradient.head
      var result = currentGradient.last
      currentGradient.find(value < _.value) match {
        case Some(point) =>
          val index = Math.max(0, currentGradient.indexOf(point))
          previousPoint = currentGradient(index - 1)
          val distance = point.value - previousPoint.value
          val distanceRatio = if (distance == 0) 0 else (value - previousPoint.value) / distance
          val red = (point.red - previousPoint.red) * distanceRatio + previousPoint.red
          val green = (point.green - previousPoint.green) * distanceRatio + previousPoint.green
          val blue = (point.blue - previousPoint.blue) * distanceRatio + previousPoint.blue
          Some(ColorPoint(red, green, blue, value))
        case _ => Some(result)
      }
    }
  }
  
  // A variety of color maps
  def heatMap: List[ColorPoint] = {
    var newColors = new ListBuffer[ColorPoint]
    newColors.append(ColorPoint(0, 0, 1, 0.0f)) // Blue.
    newColors.append(ColorPoint(0, 1, 1, 0.25f)) // Cyan.
    newColors.append(ColorPoint(0, 1, 0, 0.5f)) // Green.
    newColors.append(ColorPoint(1, 1, 0, 0.75f)) // Yellow.
    newColors.append(ColorPoint(1, 0, 0, 1.0f)) // Red.

    newColors.toList
  }
  
  def monochromeMap: List[ColorPoint] = {
    var newColors = new ListBuffer[ColorPoint]
    newColors.append(ColorPoint(1, 1, 1, 0.0f)) // White
    newColors.append(ColorPoint(1, 0, 0, 1.0f)) // Red

    newColors.toList
  }

  def twoColorMap: List[ColorPoint] = {
    var newColors = new ListBuffer[ColorPoint]
    newColors.append(ColorPoint(0, 0, 1, 0.0f)) // Blue
    newColors.append(ColorPoint(1, 1, 0, 1.0f)) // Yellow

    newColors.toList
  }
  
  def singleColor: List[ColorPoint] = {
    var newColors = new ListBuffer[ColorPoint]
    newColors.append(ColorPoint(0, 0, 1, 0.0f)) // Blue
    newColors.append(ColorPoint(0, 0, 1, 1.0f)) // Blue

    newColors.toList
  }
}

object ColorGradient {
  val HEATMAP = "heatMap"
  val MONOCHROME = "monochrome"
  val TWOCOLOR = "twoColor"
  val SINGLECOLOR = "singleColor"
}

