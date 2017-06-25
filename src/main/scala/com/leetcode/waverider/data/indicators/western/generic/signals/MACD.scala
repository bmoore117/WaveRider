package com.leetcode.waverider.data.indicators.western.generic.signals

import com.leetcode.waverider.data.indicators.IndicatorSettings
import com.leetcode.waverider.data.{AnalyzedMarketDay, RawMarketDay, Trend, Writable}
import com.leetcode.waverider.utils.LastNQueue
import com.tictactec.ta.lib.{Core, MInteger, RetCode}

import scala.collection.mutable.ListBuffer

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

case class MACDSettings(slowTimePeriod: Int, fastTimePeriod:Int, signalPeriod: Int, property: String) extends IndicatorSettings {
  override def instantiateIndicator(core: Core, rawDays: ListBuffer[RawMarketDay],
                                    analyzedDays: ListBuffer[AnalyzedMarketDay], last100Trends: LastNQueue[Trend], current: Trend): Writable = {
    val totalPeriods = slowTimePeriod + signalPeriod - 1

    val macdObj = new MACD(this)

    if(rawDays.length >= totalPeriods) {
      val days = rawDays.takeRight(totalPeriods)
      val method = days.head.getClass.getDeclaredMethod(property)
      val in = days.map(day => method.invoke(day).asInstanceOf[Number].doubleValue()).toArray

      val macd = new Array[Double](1)
      val macdSignal = new Array[Double](1)
      val macdHist = new Array[Double](1)

      val retCode = core.macd(0, in.length - 1, in, fastTimePeriod, slowTimePeriod, signalPeriod, new MInteger, new MInteger, macd, macdSignal, macdHist)

      if(retCode == RetCode.Success) {
        macdObj.macd = Some(macd.head)
        macdObj.macdSignal = Some(macdSignal.head)
        macdObj.macdHist = Some(macdHist.head)
      }
    }

    macdObj
  }
}