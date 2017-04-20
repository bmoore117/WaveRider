package com.leetcode.waverider.indicators.trend

import com.leetcode.waverider.indicators.Writable

/**
  * Created by Ben on 4/18/2017.
  */
class MovingAverage extends Writable {

  var timePeriod:Int = _
  var value:Option[Double] = _
  var avgType:MovingAverage.AvgType.AvgType = _

  override def toString = s"MovingAverage($timePeriod, $value, $avgType)"

  override def getFeatureHeaderList: List[String] = {
    List(avgType.toString + timePeriod.toString)
  }

  override def toFeatureList: List[String] = {
    List(value.getOrElse("").toString)
  }
}

object MovingAverage {

  object AvgType extends Enumeration {
    val SMA, EMA = Value
    type AvgType = Value
  }
}
