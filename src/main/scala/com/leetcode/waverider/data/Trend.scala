package com.leetcode.waverider.data

/**
  * Created by Ben on 4/27/2017.
  */
class Trend(val startIdx:Option[Int], val endIdx:Option[Int], val trendPctDelta:Option[Double], val trendDuration:Option[Int]) extends Writable {
  override def headers: List[String] = {
    List("priceChange", "durationChange")
  }

  override def features: List[String] = {
    List(trendPctDelta.getOrElse("").toString, trendDuration.getOrElse("").toString)
  }
}
