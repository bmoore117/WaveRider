package com.leetcode.waverider.data.indicators.trend

import com.leetcode.waverider.data.{AnalyzedMarketDay, RawMarketDay, Writable}
import com.leetcode.waverider.data.indicators.IndicatorSettings
import com.leetcode.waverider.data.indicators.trend.MovingAverage.AvgType
import com.leetcode.waverider.data.indicators.trend.MovingAverage.AvgType.AvgType
import com.tictactec.ta.lib.{Core, MInteger, RetCode}

import scala.collection.mutable.ListBuffer

/**
  * Created by Ben on 4/18/2017.
  */
class MovingAverage(val settings: MovingAverageSettings) extends Writable {

  var value:Option[Double] = None

  override def toString = s"MovingAverage($value, $settings)"

  override def headers: List[String] = {
    List(settings.avgType.toString + settings.timePeriod.toString)
  }

  override def features: List[String] = {
    List(value.getOrElse("").toString)
  }
}

object MovingAverage {
  object AvgType extends Enumeration {
    val SMA, EMA = Value
    type AvgType = Value
  }
}

case class MovingAverageSettings(timePeriod: Int, avgType: AvgType) extends IndicatorSettings {
  override def instantiateIndicator(core: Core, rawDays: ListBuffer[RawMarketDay], analyzedMarketDays: ListBuffer[AnalyzedMarketDay]): Writable = {
    val ma = new MovingAverage(this)

    if(rawDays.length >= timePeriod) {
      val days = rawDays.slice(rawDays.length - timePeriod, rawDays.length)

      val closingPrices = days.map(day => day.close).toArray

      val avg = new Array[Double](1)

      var retCode:RetCode = null

      if(avgType == AvgType.EMA) {
        retCode = core.ema(0, closingPrices.length - 1, closingPrices, timePeriod, new MInteger, new MInteger, avg)
      } else {
        retCode = core.sma(0, closingPrices.length - 1, closingPrices, timePeriod, new MInteger, new MInteger, avg)
      }

      if (retCode == RetCode.Success) {
        ma.value = Some(avg.head)
      }
    }

    ma
  }
}