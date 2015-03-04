package com.cra.figaro.experimental.structured

sealed abstract class Bounds
case object Lower extends Bounds
case object Upper extends Bounds
