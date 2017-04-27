package com.leetcode.waverider.data.indicators.volume

import com.leetcode.waverider.data.Writable

/**
  * Created by Benjamin on 4/18/2017.
  */
class OnBalanceVolume extends Writable {

  var value:Option[Int] = None

  override def toString = s"OnBalanceVolume($value)"

  override def headers: List[String] = {
    List("OBV")
  }

  override def features: List[String] = {
    List(value.getOrElse("").toString)
  }
}
