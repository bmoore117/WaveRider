package com.leetcode.waverider.data.indicators.trend

import com.leetcode.waverider.data.Writable

/**
  * Created by Ben on 4/18/2017.
  */
class MACD(val settings: MACDSettings) extends Writable {
  var macd:Option[Double] = None
  var macdSignal:Option[Double] = None
  var macdHist:Option[Double] = None

  override def toString = s"MACD($macd, $macdSignal, $macdHist, $settings)"

  override def headers: List[String] = {
    List("MACD", "MACDSIGNAL")
  }

  override def features: List[String] = {
    List(macd.getOrElse("").toString, macdSignal.getOrElse("").toString)
  }
}

case class MACDSettings(slowTimePeriod: Int, fastTimePeriod:Int, signalPeriod: Int)