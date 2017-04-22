package com.leetcode.waverider.data.indicators.trend

import com.leetcode.waverider.data.indicators.Writable

/**
  * Created by Ben on 4/18/2017.
  */
class MACD extends Writable {
  var fastPeriod:Int = _
  var slowPeriod:Int = _
  var signalPeriod:Int = _

  var macd:Option[Double] = None
  var macdSignal:Option[Double] = None
  var macdHist:Option[Double] = None


  override def toString = s"MACD($fastPeriod, $slowPeriod, $signalPeriod, $macd, $macdSignal, $macdHist)"

  override def headers: List[String] = {
    List("MACD", "MACDSIGNAL")
  }

  override def features: List[String] = {
    List(macd.getOrElse("").toString, macdSignal.getOrElse("").toString)
  }
}
