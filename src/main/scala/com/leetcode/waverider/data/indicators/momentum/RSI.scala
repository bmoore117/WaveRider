package com.leetcode.waverider.data.indicators.momentum

import com.leetcode.waverider.data.{AnalyzedMarketDay, RawMarketDay, Writable}
import com.leetcode.waverider.data.indicators.IndicatorSettings
import com.tictactec.ta.lib.{Core, MInteger, RetCode}

import scala.collection.mutable.ListBuffer

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

case class RSISettings(timePeriod: Int) extends IndicatorSettings {
  override def instantiateIndicator(core: Core, rawDays: ListBuffer[RawMarketDay], analyzedMarketDays: ListBuffer[AnalyzedMarketDay]): Writable = {
    val rsi = new RSI(this)

    //must include 1 extra day, as first element in array needs a prior element
    if(rawDays.length > timePeriod) {
      val days = rawDays.slice(rawDays.length - timePeriod - 1, rawDays.length)

      val closingPrices = days.map(day => day.close).toArray

      val outRSI = new Array[Double](1)

      val retCode = core.rsi(0, closingPrices.length - 1, closingPrices, timePeriod, new MInteger, new MInteger, outRSI)

      if (retCode == RetCode.Success) {
        rsi.value = Some(outRSI.head)
      }
    }

    rsi
  }
}