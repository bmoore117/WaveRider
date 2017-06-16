package com.leetcode.waverider.data.indicators.trend

import com.leetcode.waverider.data.Writable
import com.leetcode.waverider.data.indicators.trend.MovingAverage.AvgType.AvgType

/**
  * Created by Ben on 4/18/2017.
  */
class MovingAverage(val settings: MovingAverageSettings) extends Writable {

  var value:Option[Double] = None

  override def toString = s"MovingAverage($value, $settings)"

  override def headers: List[String] = {
    List(settings.avgType.toString + settings.timePeriod.toString)
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

case class MovingAverageSettings(timePeriod: Int, avgType: AvgType)