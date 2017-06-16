package com.leetcode.waverider.data.indicators.momentum

import com.leetcode.waverider.data.Writable

/**
  * Created by Benjamin on 4/18/2017.
  */
class RSI(val settings: RSISettings) extends Writable {

  var value:Option[Double] = None

  override def toString = s"RSI($settings, $value)"

  override def headers: List[String] = {
    List("RSI")
  }

  override def features: List[String] = {
    List(value.getOrElse("").toString)
  }
}

case class RSISettings(timePeriod: Int)