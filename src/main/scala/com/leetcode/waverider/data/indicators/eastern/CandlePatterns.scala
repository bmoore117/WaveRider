package com.leetcode.waverider.data.indicators.eastern

import com.leetcode.waverider.data.indicators.IndicatorSettings
import com.leetcode.waverider.data.{AnalyzedMarketDay, RawMarketDay, Trend, Writable}
import com.leetcode.waverider.utils.LastNQueue
import com.tictactec.ta.lib.{Core, MInteger}

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

/**
  * Created by Ben on 6/21/2017.
  */
class CandlePatterns extends Writable {
  override def headers: List[String] = names

  override def features: List[String] = values.map(i => i.toString)

  var values:List[Int] = Nil
  var names:List[String] = Nil
}

case class CandlePatternsBuilder(timePeriod: Int) extends IndicatorSettings {
  override def instantiateIndicator(core: Core, rawDays: ListBuffer[RawMarketDay], analyzedDays: ListBuffer[AnalyzedMarketDay], last100Trends: LastNQueue[Trend], current: Trend): Writable = {
    val days = rawDays.slice(rawDays.length - timePeriod, rawDays.length)

    val high = new Array[Double](days.length)
    val low = new Array[Double](days.length)
    val close = new Array[Double](days.length)
    val open = new Array[Double](days.length)

    days.indices.foreach(i => {
      val day = days(i)
      high(i) = day.high
      low(i) = day.low
      close(i) = day.close
      open(i) = day.open
    })

    val resultList = new ArrayBuffer[Int]()
    val nameList = new ListBuffer[String]()

    val test = new Array[Float](1)
    test.getClass

    core.getClass.getDeclaredMethods.filter(m => m.getName.contains("cdl")
      && !m.getName.contains("Lookback")
      && !m.getParameterTypes.contains(test.getClass)
    ).sortBy(m => m.getName).foreach(m => {
        if(m.getParameterCount == 9) {
          val result = new Array[Int](days.length)
          m.invoke(core, new Integer(0), new Integer(days.length - 1), open, high, low, close, new MInteger, new MInteger, result)
          resultList.append(result.sum)
          nameList.append(m.getName.replace("cdl", ""))
        } else if(m.getParameterCount == 10) {
          val result = new Array[Int](days.length)
          m.invoke(core, new Integer(0), new Integer(days.length - 1), open, high, low, close, new java.lang.Double(Double.MinValue), new MInteger, new MInteger, result)
          resultList.append(result.sum)
          nameList.append(m.getName.replace("cdl", ""))
        }
      })

    val patterns = new CandlePatterns
    patterns.names = nameList.toList
    patterns.values = resultList.toList

    patterns
  }
}
