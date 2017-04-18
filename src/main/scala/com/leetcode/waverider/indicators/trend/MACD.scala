package com.leetcode.waverider.indicators.trend

/**
  * Created by Ben on 4/18/2017.
  */
class MACD {
  var fastPeriod:Int = _
  var slowPeriod:Int = _
  var signalPeriod:Int = _

  var macd:Double = _
  var macdSignal:Double = _
  var macdHist:Double = _


  override def toString = s"MACD($fastPeriod, $slowPeriod, $signalPeriod, $macd, $macdSignal, $macdHist)"
}
