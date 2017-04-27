package com.leetcode.waverider.data

/**
  * Created by Ben on 4/27/2017.
  */
class ChangeMarker extends Writable {

  var valueChangeFromHere:Double = _
  var trendDurationFromHere:Int = _

  override def headers: List[String] = {
    List("valueChangeFromHere", "trendDurationFromHere")
  }

  override def features: List[String] = {
    List(valueChangeFromHere.toString, trendDurationFromHere.toString)
  }
}
