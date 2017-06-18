package com.leetcode.waverider.data.indicators.typed.discovery

import com.leetcode.waverider.data.{AnalyzedMarketDay, RawMarketDay, Writable}
import com.leetcode.waverider.data.indicators.IndicatorSettings
import com.tictactec.ta.lib.{Core, MAType, MInteger, RetCode}

import scala.collection.mutable.ListBuffer

/**
  * Created by Ben on 6/17/2017.
  */
class MFI(val settings: MFISettings) extends Writable {
  override def headers: List[String] = List("MFI")

  override def features: List[String] = List(value.getOrElse("").toString)

  var value:Option[Double] = None
}

case class MFISettings(timePeriod:Int) extends IndicatorSettings {
  override def instantiateIndicator(core: Core, rawDays: ListBuffer[RawMarketDay], analyzedDays: ListBuffer[AnalyzedMarketDay]): Writable = {
    val mfi = new MFI(this)

    if(rawDays.length >= timePeriod) {
      val days = rawDays.slice(rawDays.length - timePeriod, rawDays.length)

      val high = new Array[Double](days.length)
      val low = new Array[Double](days.length)
      val close = new Array[Double](days.length)
      val volume = new Array[Double](days.length)

      val result = new Array[Double](1)

      days.indices.foreach(i => {
        val day = days(i)
        high(i) = day.high
        low(i) = day.low
        close(i) = day.close
        volume(i) = day.volume
      })

      val retCode = core.mfi(0, days.length - 1, high, low, close, volume, timePeriod, new MInteger, new MInteger, result)

      if(retCode == RetCode.Success) {
        mfi.value = Some(result.head)
      }
    }

    mfi
  }
}