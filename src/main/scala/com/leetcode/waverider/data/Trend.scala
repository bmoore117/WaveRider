package com.leetcode.waverider.data

/**
  * Created by Ben on 4/27/2017.
  */
class Trend(val startIdx:Option[Int], val endIdx:Option[Int], val pctDelta:Option[Double], val duration:Option[Int]) extends Writable {
  override def headers: List[String] = List("SIZE", "DURATION")

  override def features: List[String] = List(pctDelta.getOrElse("").toString, duration.getOrElse("").toString)
}
