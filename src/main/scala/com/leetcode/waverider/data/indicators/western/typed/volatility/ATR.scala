package com.leetcode.waverider.data.indicators.western.typed.volatility

import com.leetcode.waverider.data.indicators.IndicatorBuilder
import com.leetcode.waverider.data.{AnalyzedMarketDay, RawMarketDay, Trend, Writable}
import com.leetcode.waverider.utils.LastNQueue
import com.tictactec.ta.lib.{Core, MInteger, RetCode}

import scala.collection.mutable.ListBuffer

/**
  * Created by Ben on 4/17/2017.
  */
class AvgTrueRange(val settings: ATRBuilder) extends Writable{
  var value:Option[Double] = None

  override def headers: List[String] = {
    List("ATR")
  }

  override def features: List[String] = {
    List(value.getOrElse("").toString)
  }
}

case class ATRBuilder(timePeriod: Int) extends IndicatorBuilder {
  override def instantiateIndicator(core: Core, rawDays: ListBuffer[RawMarketDay],
                                    analyzedDays: ListBuffer[AnalyzedMarketDay], last100Trends: LastNQueue[Trend], current: Trend): Writable = {
    val atr = new AvgTrueRange(this)

    //strictly greater than, as we need 15 points for a 14 day ATR: we need 1 point past the last, as TR requires it
    if(rawDays.length > timePeriod) {
      val days = rawDays.takeRight(timePeriod)

      val highs = days.map(day => day.high).toArray
      val lows = days.map(day => day.low).toArray
      val close = days.map(day => day.close).toArray

      val result = new Array[Double](1)

      val retCode = core.atr(0, days.length - 1, highs, lows, close, timePeriod, new MInteger, new MInteger, result)

      if(retCode == RetCode.Success) {
        atr.value = Some(result.head)
      }
    }

    atr
  }
}
