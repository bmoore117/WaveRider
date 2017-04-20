package com.leetcode.waverider.indicators.volatility

import com.leetcode.waverider.indicators.Writable

/**
  * Created by Ben on 4/17/2017.
  */
class AvgTrueRange extends Writable{
  var value:Option[Double] = _

  override def getFeatureHeaderList: List[String] = {
    List("ATR")
  }

  override def toFeatureList: List[String] = {
    List(value.getOrElse("").toString)
  }
}
