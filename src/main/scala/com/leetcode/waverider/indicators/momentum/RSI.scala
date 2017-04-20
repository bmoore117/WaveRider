package com.leetcode.waverider.indicators.momentum

import com.leetcode.waverider.indicators.Writable

/**
  * Created by Benjamin on 4/18/2017.
  */
class RSI extends Writable {

  var timePeriod:Int = _
  var value:Option[Double] = _


  override def toString = s"RSI($timePeriod, $value)"

  override def getFeatureHeaderList: List[String] = {
    List("RSI")
  }

  override def toFeatureList: List[String] = {
    List(value.getOrElse("").toString)
  }
}
