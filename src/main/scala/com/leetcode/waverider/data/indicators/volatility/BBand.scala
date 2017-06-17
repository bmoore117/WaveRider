package com.leetcode.waverider.data.indicators.volatility

import com.leetcode.waverider.data.indicators.IndicatorSettings
import com.leetcode.waverider.data.{AnalyzedMarketDay, RawMarketDay, Writable}
import com.tictactec.ta.lib.{Core, MAType, MInteger, RetCode}

import scala.collection.mutable.ListBuffer

/**
  * Created by Benjamin on 4/15/2017.
  */
class BBand(settings: BBandSettings) extends Writable {

  var upperBand:Option[Double] = None
  var avg:Option[Double] = None
  var lowerBand:Option[Double] = None

  override def toString = s"BBand(upperBand=$upperBand, avg=$avg, lowerBand=$lowerBand, settings=$BBandSettings)"

  override def headers: List[String] = {
    List("BBANDUPPER", "BBANDAVG", "BBANDLOWER")
  }

  override def features: List[String] = {
    List(upperBand.getOrElse("").toString, avg.getOrElse("").toString, lowerBand.getOrElse("").toString)
  }
}

case class BBandSettings(timePeriod: Int, distanceDeviations: Int) extends IndicatorSettings {
  override def instantiateIndicator(core: Core, rawDays: ListBuffer[RawMarketDay], analyzedMarketDays: ListBuffer[AnalyzedMarketDay]): BBand = {
    val band = new BBand(this)

    if(rawDays.length >= timePeriod) {

      val prices = rawDays.slice(rawDays.length - timePeriod, rawDays.length).map(day => day.close).toArray

      val upperBand: Array[Double] = new Array[Double](1)
      val avg: Array[Double] = new Array[Double](1)
      val lowerBand: Array[Double] = new Array[Double](1)

      val begin = new MInteger
      val nbElement = new MInteger

      val retCode = core.bbands(0, prices.length - 1, prices, timePeriod, distanceDeviations,
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