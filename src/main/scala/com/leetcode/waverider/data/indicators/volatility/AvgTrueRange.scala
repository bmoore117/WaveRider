package com.leetcode.waverider.data.indicators.volatility

import com.leetcode.waverider.data.Writable

/**
  * Created by Ben on 4/17/2017.
  */
class AvgTrueRange(val settings: AvgTrueRangeSettings) extends Writable{
  var value:Option[Double] = None

  override def headers: List[String] = {
    List("ATR")
  }

  override def features: List[String] = {
    List(value.getOrElse("").toString)
  }
}

case class AvgTrueRangeSettings(timePeriod: Int)
