package com.leetcode.waverider.data.indicators.historical

import com.leetcode.waverider.data.Writable

/**
  * Created by Ben on 5/2/2017.
  */
class HistoricalTrend(val priceChange:Option[Double], val trendDuration:Option[Int]) extends Writable {
  override def headers: List[String] = {
    List("priceChange", "durationChange")
  }

  override def features: List[String] = {
    List(priceChange.getOrElse("").toString, trendDuration.getOrElse("").toString)
  }
}
