package com.leetcode.waverider.data.indicators.western.generic.rate

import com.leetcode.waverider.data.{AnalyzedMarketDay, RawMarketDay, Trend, Writable}
import com.leetcode.waverider.data.indicators.IndicatorSettings
import com.leetcode.waverider.utils.LastNQueue
import com.tictactec.ta.lib.{Core, MInteger, RetCode}

import scala.collection.mutable.ListBuffer

/**
  * Created by Ben on 6/17/2017.
  */
class MOM(val settings: MOMSettings) extends Writable {
  var value:Option[Double] = None

  override def headers: List[String] = List("MOM")

  override def features: List[String] = List(value.getOrElse("").toString)
}

case class MOMSettings(timePeriod: Int, property: String) extends IndicatorSettings {
  override def instantiateIndicator(core: Core, rawDays: ListBuffer[RawMarketDay],
                                    analyzedDays: ListBuffer[AnalyzedMarketDay], last100Trends: LastNQueue[Trend], current: Trend): Writable = {
    val mom = new MOM(this)
    if(rawDays.length >= timePeriod) {
      val days = rawDays.slice(rawDays.length - timePeriod, rawDays.length)
      val field = days.head.getClass.getDeclaredField(property)
      val in = days.map(day => field.getDouble(day)).toArray
      val result = new Array[Double](1)

      val retCode = core.mom(0, days.length - 1, in, timePeriod, new MInteger, new MInteger, result)

      if(retCode == RetCode.Success) {
        mom.value = Some(result.head)
      }
    }
    mom
  }
}