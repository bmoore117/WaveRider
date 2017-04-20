package com.leetcode.waverider.indicators.trend

import com.leetcode.waverider.indicators.Writable

/**
  * Created by Ben on 4/18/2017.
  */
class MACD extends Writable {
  var fastPeriod:Int = _
  var slowPeriod:Int = _
  var signalPeriod:Int = _

  var macd:Option[Double] = _
  var macdSignal:Option[Double] = _
  var macdHist:Option[Double] = _


  override def toString = s"MACD($fastPeriod, $slowPeriod, $signalPeriod, $macd, $macdSignal, $macdHist)"

  override def getFeatureHeaderList: List[String] = {
    List("MACD", "MACDSIGNAL")
  }

  override def toFeatureList: List[String] = {
    List(macd.getOrElse("").toString, macdSignal.getOrElse("").toString)
  }
}
