package com.leetcode.waverider.data.indicators.western.generic.trend

import com.leetcode.waverider.data.{AnalyzedMarketDay, RawMarketDay, Trend, Writable}
import com.leetcode.waverider.data.indicators.IndicatorSettings
import com.leetcode.waverider.data.indicators.western.generic.trend.MovingAverage.AvgType
import com.leetcode.waverider.data.indicators.western.generic.trend.MovingAverage.AvgType.AvgType
import com.leetcode.waverider.utils.LastNQueue
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

case class MovingAverageSettings(timePeriod: Int, avgType: AvgType, property: String) extends IndicatorSettings {
  override def instantiateIndicator(core: Core, rawDays: ListBuffer[RawMarketDay],
                                    analyzedDays: ListBuffer[AnalyzedMarketDay], last100Trends: LastNQueue[Trend], current: Trend): Writable = {
    val ma = new MovingAverage(this)

    if(rawDays.length >= timePeriod) {
      val days = rawDays.slice(rawDays.length - timePeriod, rawDays.length)
      val field = days.head.getClass.getDeclaredField(property)
      val in = days.map(day => field.getDouble(day)).toArray
      val result = new Array[Double](1)

      var retCode:RetCode = null

      if(avgType == AvgType.EMA) {
        retCode = core.ema(0, in.length - 1, in, timePeriod, new MInteger, new MInteger, result)
      } else {
        retCode = core.sma(0, in.length - 1, in, timePeriod, new MInteger, new MInteger, result)
      }

      if (retCode == RetCode.Success) {
        ma.value = Some(result.head)
      }
    }

    ma
  }
}