package com.leetcode.waverider.data.indicators.volatility

import com.leetcode.waverider.data.Writable

/**
  * Created by Benjamin on 4/15/2017.
  */
class BBand(settings: BBandSettings) extends Writable {

  var upperBand:Option[Double] = None
  var avg:Option[Double] = None
  var lowerBand:Option[Double] = None

  override def toString = s"BBand(upperBand=$upperBand, avg=$avg, lowerBand=$lowerBand, settings=$BBandSettings)"

  override def headers: List[String] = {
    List("BBANDUPPER", "BBANDAVG", "BBANDLOWER")
  }

  override def features: List[String] = {
    List(upperBand.getOrElse("").toString, avg.getOrElse("").toString, lowerBand.getOrElse("").toString)
  }
}

case class BBandSettings(timePeriod: Int, distanceDeviations: Int)