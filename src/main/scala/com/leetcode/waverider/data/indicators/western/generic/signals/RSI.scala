package com.leetcode.waverider.data.indicators.western.generic.signals

import com.leetcode.waverider.data.{AnalyzedMarketDay, RawMarketDay, Trend, Writable}
import com.leetcode.waverider.data.indicators.IndicatorBuilder
import com.leetcode.waverider.utils.LastNQueue
import com.tictactec.ta.lib.{Core, MInteger, RetCode}

import scala.collection.mutable.ListBuffer

/**
  * Created by Benjamin on 4/18/2017.
  */
class RSI(val settings: RSIBuilder) extends Writable {

  var value:Option[Double] = None

  override def toString = s"RSI($settings, $value)"

  override def headers: List[String] = {
    List("RSI")
  }

  override def features: List[String] = {
    List(value.getOrElse("").toString)
  }
}

case class RSIBuilder(timePeriod: Int, property: String) extends IndicatorBuilder {
  override def instantiateIndicator(core: Core, rawDays: ListBuffer[RawMarketDay],
                                    analyzedDays: ListBuffer[AnalyzedMarketDay], last100Trends: LastNQueue[Trend], current: Trend): Writable = {
    val rsi = new RSI(this)

    //must include 1 extra day, as first element in array needs a prior element
    if(rawDays.length > timePeriod) {
      val days = rawDays.takeRight(timePeriod)
      val method = days.head.getClass.getDeclaredMethod(property)
      val in = days.map(day => method.invoke(day).asInstanceOf[Number].doubleValue()).toArray
      val result = new Array[Double](1)

      val retCode = core.rsi(0, in.length - 1, in, timePeriod, new MInteger, new MInteger, result)

      if (retCode == RetCode.Success) {
        rsi.value = Some(result.head)
      }
    }

    rsi
  }
}