package com.leetcode.waverider.data.indicators.trend

import com.leetcode.waverider.data.indicators.Writable

/**
  * Created by Ben on 4/18/2017.
  */
class MovingAverage extends Writable {

  var timePeriod:Int = _
  var value:Option[Double] = None
  var avgType:MovingAverage.AvgType.AvgType = _

  override def toString = s"MovingAverage($timePeriod, $value, $avgType)"

  override def headers: List[String] = {
    List(avgType.toString + timePeriod.toString)
  }

  override def features: List[String] = {
    List(value.getOrElse("").toString)
  }
}

object MovingAverage {

  object AvgType extends Enumeration {
    val SMA, EMA = Value
    type AvgType = Value
  }
}
