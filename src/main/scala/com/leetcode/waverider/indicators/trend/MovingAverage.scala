package com.leetcode.waverider.indicators.trend

/**
  * Created by Ben on 4/18/2017.
  */
class MovingAverage {

  var timePeriod:Int = _
  var value:Double = _
  var avgType:MovingAverage.AvgType.AvgType = _

  override def toString = s"MovingAverage($timePeriod, $value, $avgType)"
}

object MovingAverage {

  object AvgType extends Enumeration {
    val SMA, EMA = Value
    type AvgType = Value
  }
}
