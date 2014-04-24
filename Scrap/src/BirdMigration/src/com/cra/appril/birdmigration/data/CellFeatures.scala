package com.cra.appril.birdmigration.data
//Plain old data
class CellFeatures(val distance: Double, val vectorDifference: Double, val windDirection: Double, val fromEqualsTo: Boolean)

object CellFeatures {
  def apply(distance: Double, vectorDifference: Double, windDirection: Double, fromEqualsTo: Boolean) = { new CellFeatures(distance, vectorDifference,windDirection,fromEqualsTo)}
}