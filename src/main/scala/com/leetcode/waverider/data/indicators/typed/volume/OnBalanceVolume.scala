package com.leetcode.waverider.data.indicators.typed.volume

import com.leetcode.waverider.data.{AnalyzedMarketDay, RawMarketDay, Writable}
import com.leetcode.waverider.data.indicators.IndicatorSettings
import com.tictactec.ta.lib.Core

import scala.collection.mutable.ListBuffer

/**
  * Created by Benjamin on 4/18/2017.
  */
class OnBalanceVolume extends Writable {

  var value:Option[Int] = None

  override def toString = s"OnBalanceVolume($value)"

  override def headers: List[String] = {
    List("OBV")
  }

  override def features: List[String] = {
    List(value.getOrElse("").toString)
  }
}

case class OnBalanceVolumeSettings() extends IndicatorSettings {
  override def instantiateIndicator(core: Core, rawDays: ListBuffer[RawMarketDay], analyzedMarketDays: ListBuffer[AnalyzedMarketDay]): Writable = {
    val todayOBV = new OnBalanceVolume

    if(rawDays.length > 1 && analyzedMarketDays.nonEmpty) {

      val days = rawDays.slice(rawDays.length - 2, rawDays.length)

      val prices = days.map(day => day.close)
      val volume = days.map(day => day.volume)

      val yesterdayOBV = analyzedMarketDays.last.indicators.find(indicator => indicator.isInstanceOf[OnBalanceVolume])
      val yesterday = yesterdayOBV.get.asInstanceOf[OnBalanceVolume]

      yesterday.value match {
        case Some(obv) =>
          if(prices.head > prices.last) {
            todayOBV.value = Some(obv + volume.last)
          } else if(prices.head < prices.last) {
            todayOBV.value = Some(obv - volume.last)
          }
        case None =>
          if(prices.head > prices.last) {
            todayOBV.value = Some(volume.last)
          } else if(prices.head < prices.last) {
            todayOBV.value = Some(-volume.last)
          }
      }
    }

    todayOBV
  }
}
