package com.leetcode.waverider.data.indicators

import com.leetcode.waverider.data.indicators.western.typed.volume.{OBVBuilder, OnBalanceVolume}
import com.leetcode.waverider.data.{RawMarketDay, Trend, Writable}
import com.leetcode.waverider.utils.LastNQueue
import com.tictactec.ta.lib.Core

import scala.collection.mutable.ListBuffer

/**
  * Created by Ben on 7/10/2017.
  */
class PriceActionSet(timePeriod: Int) extends Writable {
  var signals:Option[List[Signal]] = None

  override def headers: List[String] = signals.getOrElse(Nil).flatMap(signal => signal.headers)

  override def features: List[String] = signals.getOrElse(Nil).flatMap(signal => signal.features)
}

class Signal(order: Int) extends Writable {

  var change:Option[Double] = None
  var range:Option[Double] = None
  var obv:Option[Double] = None

  override def headers: List[String] = List("Change" + order, "Range" + order, "OBV" + order)

  override def features: List[String] = List(change.getOrElse("").toString, range.getOrElse("").toString, obv.getOrElse("").toString)
}

case class PriceActionSetBuilder(timePeriod: Int) extends IndicatorBuilder {

  override def instantiateIndicator(core: Core, rawDays: ListBuffer[RawMarketDay],
                                    last100Trends: LastNQueue[Trend], current: Trend): Writable = {

    val priceActionSet = new PriceActionSet(timePeriod)

    if(rawDays.length > timePeriod + 1) { //plus 1 added because OBV below will need it
      val days = rawDays.takeRight(timePeriod + 1)

      val signals = new ListBuffer[Signal]
      for(i <- 0 until timePeriod) {
        val change = (days.last.close - days(i).close)/days.last.close

        val subRange = days.takeRight(days.length - i)
        val HH = subRange.maxBy(day => day.high).high
        val LL = subRange.minBy(day => day.low).low

        val range = (HH - LL)/days.last.close

        val oBVBuilder = OBVBuilder(subRange.length)
        val obv = oBVBuilder.instantiateIndicator(core, rawDays, last100Trends, current)
          .asInstanceOf[OnBalanceVolume]

        val signal = new Signal(i)
        signal.change = Some(change)
        signal.range = Some(range)
        signal.obv = obv.value

        signals.append(signal)
      }

      priceActionSet.signals = Some(signals.toList)
    }

    priceActionSet
  }
}