package com.leetcode.waverider.data.indicators.western.generic.rate

import com.leetcode.waverider.data.{AnalyzedMarketDay, RawMarketDay, Trend, Writable}
import com.leetcode.waverider.data.indicators.IndicatorBuilder
import com.leetcode.waverider.utils.LastNQueue
import com.tictactec.ta.lib.{Core, MInteger, RetCode}

import scala.collection.mutable.ListBuffer

/**
  * Created by Ben on 6/17/2017.
  */
class MOM(val settings: MOMBuilder) extends Writable {
  var value:Option[Double] = None

  override def headers: List[String] = List("MOM")

  override def features: List[String] = List(value.getOrElse("").toString)
}

case class MOMBuilder(timePeriod: Int, property: String) extends IndicatorBuilder {
  override def instantiateIndicator(core: Core, rawDays: ListBuffer[RawMarketDay],
                                    analyzedDays: ListBuffer[AnalyzedMarketDay], last100Trends: LastNQueue[Trend], current: Trend): Writable = {
    val mom = new MOM(this)
    if(rawDays.length >= timePeriod + 1) {
      val days = rawDays.takeRight(timePeriod + 1)
      val method = days.head.getClass.getDeclaredMethod(property)
      val in = days.map(day => method.invoke(day).asInstanceOf[Number].doubleValue()).toArray
      val result = new Array[Double](1)

      val retCode = core.mom(0, days.length - 1, in, timePeriod, new MInteger, new MInteger, result)

      if(retCode == RetCode.Success) {
        mom.value = Some(result.head)
      }
    }
    mom
  }
}