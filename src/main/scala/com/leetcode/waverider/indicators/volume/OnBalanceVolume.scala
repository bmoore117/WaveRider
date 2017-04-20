package com.leetcode.waverider.indicators.volume

import com.leetcode.waverider.indicators.Writable

/**
  * Created by Benjamin on 4/18/2017.
  */
class OnBalanceVolume extends Writable {

  var value:Option[Int] = _

  override def toString = s"OnBalanceVolume($value)"

  override def getFeatureHeaderList: List[String] = {
    List("OBV")
  }

  override def toFeatureList: List[String] = {
    List(value.getOrElse("").toString)
  }
}
