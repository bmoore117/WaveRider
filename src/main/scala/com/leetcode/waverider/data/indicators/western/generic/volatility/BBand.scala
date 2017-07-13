package com.leetcode.waverider.data.indicators.western.generic.volatility

import com.leetcode.waverider.data.indicators.IndicatorBuilder
import com.leetcode.waverider.data.{AnalyzedMarketDay, RawMarketDay, Trend, Writable}
import com.leetcode.waverider.utils.LastNQueue
import com.tictactec.ta.lib.{Core, MAType, MInteger, RetCode}

import scala.collection.mutable.ListBuffer

/**
  * Created by Benjamin on 4/15/2017.
  */
class BBand(settings: BBandBuilder) extends Writable {

  var upperBand:Option[Double] = None
  var avg:Option[Double] = None
  var lowerBand:Option[Double] = None

  override def toString = s"BBand(upperBand=$upperBand, avg=$avg, lowerBand=$lowerBand, settings=$BBandBuilder)"

  override def headers: List[String] = {
    List("BBANDUPPER", "BBANDAVG", "BBANDLOWER")
  }

  override def features: List[String] = {
    List(upperBand.getOrElse("").toString, avg.getOrElse("").toString, lowerBand.getOrElse("").toString)
  }
}

case class BBandBuilder(timePeriod: Int, distanceDeviations: Int, property: String) extends IndicatorBuilder {
  override def instantiateIndicator(core: Core, rawDays: ListBuffer[RawMarketDay],
                                    last100Trends: LastNQueue[Trend], current: Trend): Writable = {
    val band = new BBand(this)

    if(rawDays.length >= timePeriod) {
      val days = rawDays.takeRight(timePeriod)
      val method = days.head.getClass.getDeclaredMethod(property)
      val in = days.map(day => method.invoke(day).asInstanceOf[Number].doubleValue()).toArray

      val upperBand: Array[Double] = new Array[Double](1)
      val avg: Array[Double] = new Array[Double](1)
      val lowerBand: Array[Double] = new Array[Double](1)

      val begin = new MInteger
      val nbElement = new MInteger

      val retCode = core.bbands(0, in.length - 1, in, timePeriod, distanceDeviations,
        distanceDeviations, MAType.Sma, begin, nbElement, upperBand, avg, lowerBand)

      if(retCode == RetCode.Success) {
        band.upperBand = Some(upperBand.head)
        band.avg = Some(avg.head)
        band.lowerBand = Some(lowerBand.head)
      }
    }

    band
  }
}