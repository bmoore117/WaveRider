package com.leetcode.waverider.indicators.volatility

import com.leetcode.waverider.indicators.Writable

/**
  * Created by Benjamin on 4/15/2017.
  */
class BBand extends Writable {
  var upperBand:Option[Double] = _
  var avg:Option[Double] = _
  var lowerBand:Option[Double] = _
  var bandDistance:Int = _


  override def toString = s"BBand(upperBand=$upperBand, avg=$avg, lowerBand=$lowerBand, bandDistance=$bandDistance)"

  override def getFeatureHeaderList: List[String] = {
    List("BBANDUPPER", "BBANDAVG", "BBANDLOWER")
  }

  override def toFeatureList: List[String] = {
    List(upperBand.getOrElse("").toString, avg.getOrElse("").toString, lowerBand.getOrElse("").toString)
  }
}
