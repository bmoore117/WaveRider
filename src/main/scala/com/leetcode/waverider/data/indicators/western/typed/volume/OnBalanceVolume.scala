package com.leetcode.waverider.data.indicators.western.typed.volume

import com.leetcode.waverider.data.{AnalyzedMarketDay, RawMarketDay, Trend, Writable}
import com.leetcode.waverider.data.indicators.IndicatorBuilder
import com.leetcode.waverider.utils.LastNQueue
import com.tictactec.ta.lib.{Core, MInteger}

import scala.collection.mutable.ListBuffer

/**
  * Created by Benjamin on 4/18/2017.
  */
class OnBalanceVolume extends Writable {

  var value:Option[Double] = None

  override def toString = s"OnBalanceVolume($value)"

  override def headers: List[String] = {
    List("OBV")
  }

  override def features: List[String] = {
    List(value.getOrElse("").toString)
  }
}

case class OBVBuilder(timePeriod: Int) extends IndicatorBuilder {
  override def instantiateIndicator(core: Core, rawDays: ListBuffer[RawMarketDay],
                                    last100Trends: LastNQueue[Trend], current: Trend): Writable = {
    val obv = new OnBalanceVolume

    if(rawDays.length > timePeriod) {
      val days = rawDays.takeRight(timePeriod + 1) //+ 1 so starting day will have value

      val close = days.map(day => day.close).toArray
      val volume = days.map(day => day.volume.toDouble).toArray

      val result = new Array[Double](days.length)

      core.obv(0, days.length - 1, close, volume, new MInteger, new MInteger, result)

      obv.value = Some(result.sum)
    }

    obv
  }
}
