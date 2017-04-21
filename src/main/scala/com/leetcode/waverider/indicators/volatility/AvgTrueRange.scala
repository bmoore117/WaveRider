package com.leetcode.waverider.indicators.volatility

import com.leetcode.waverider.indicators.Writable

/**
  * Created by Ben on 4/17/2017.
  */
class AvgTrueRange extends Writable{
  var value:Option[Double] = None

  override def headers: List[String] = {
    List("ATR")
  }

  override def features: List[String] = {
    List(value.getOrElse("").toString)
  }
}
