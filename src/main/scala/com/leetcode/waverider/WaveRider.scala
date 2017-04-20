package com.leetcode.waverider

import java.io.File
import java.text.SimpleDateFormat

import com.github.tototoshi.csv._
import com.leetcode.waverider.indicators.momentum.RSI
import com.leetcode.waverider.indicators.trend.MovingAverage.AvgType
import com.leetcode.waverider.indicators.trend.MovingAverage.AvgType.AvgType
import com.leetcode.waverider.indicators.trend.{MACD, MovingAverage}
import com.leetcode.waverider.indicators.volatility.{AvgTrueRange, BBand}
import com.leetcode.waverider.indicators.volume.OnBalanceVolume
import com.tictactec.ta.lib.{Core, MAType, MInteger, RetCode}

import scala.collection.mutable.ArrayBuffer

/**
  * Created by Benjamin on 4/15/2017.
  *
  *
  * Indicators for trend trading: moving averages, RSI, MACD
  * Indicators for range trading: BBands, MACD. BBands identify range, MACD can signal reversal
  *
  * General utility: average true range, indicates overall volatility. On-balance volume, indicates future trend breakout
  *
  */
object WaveRider {

  val marketActivity = new ArrayBuffer[RawMarketDay]()
  val core = new Core

  val analyzedMarketDays = new ArrayBuffer[AnalyzedMarketDay]()

  def main(args: Array[String]): Unit = {
    if (args.length == 1) {
      val marketFile = new File(args(0))

      if (marketFile.exists) {
        val market = CSVReader.open(marketFile)

        market.iterator.next()

        market.iterator.foreach(day => {
          val mktDay = initTypes(day)
          marketActivity.append(mktDay)

          println("Date: " + day.head)

          val rsi = RSI()

          val macd_result = macd()

          val ema200 = movingAverage(200, AvgType.EMA)
          val ema100 = movingAverage(100, AvgType.EMA)
          val ema50 = movingAverage(50, AvgType.EMA)
          val ema25 = movingAverage(25, AvgType.EMA)
          val ema15 = movingAverage(15, AvgType.EMA)
          val ema10 = movingAverage(10, AvgType.EMA)
          val ema5 = movingAverage(5, AvgType.EMA)

          val atr = averageTrueRange()

          val band = bollinger()

          val obv = OBV()

          val analyzedDay = new AnalyzedMarketDay(rsi, macd_result, ema200, ema100, ema50, ema25, ema15, ema10, ema5, atr, band, obv)

          analyzedMarketDays.append(analyzedDay)

        })
      }
    } else {
      println("Supply single market .csv file, such as from Yahoo finance")
    }
  }


  def initTypes(day: Seq[String]): RawMarketDay = {

    val format = new SimpleDateFormat("yyyy-MM-DD")
    val mktDay = new RawMarketDay

    mktDay.date = format.parse(day.head)
    mktDay.open = day(1).toDouble
    mktDay.high = day(2).toDouble
    mktDay.low = day(3).toDouble
    mktDay.close = day(4).toDouble
    mktDay.volume = day(5).toInt
    mktDay.adjustedClose = day(6).toDouble

    mktDay
  }

  def bollinger(): Option[BBand] = {
    val TIME_PERIOD = 21
    val DISTANCE_DEVIATIONS = 2

    if (marketActivity.length >= TIME_PERIOD) {

      val prices = marketActivity.slice(marketActivity.length - TIME_PERIOD, marketActivity.length).map(day => day.close).toArray

      val upperBand: Array[Double] = new Array[Double](1)
      val avg: Array[Double] = new Array[Double](1)
      val lowerBand: Array[Double] = new Array[Double](1)

      val begin = new MInteger
      val nbElement = new MInteger

      val retCode = core.bbands(0, prices.length - 1, prices, TIME_PERIOD, DISTANCE_DEVIATIONS, DISTANCE_DEVIATIONS, MAType.Sma, begin, nbElement, upperBand, avg, lowerBand)

      if (retCode == RetCode.Success) {
        val band = new BBand
        band.upperBand = upperBand.head
        band.avg = avg.head
        band.lowerBand = lowerBand.head
        band.bandDistance = DISTANCE_DEVIATIONS

        return Some(band)
      }
    }

    None
  }

  def averageTrueRange(): Option[AvgTrueRange] = {
    val TIME_PERIOD = 14

    //strictly greater than, as we need 15 points for a 14 day ATR: we need 1 point past the last, as TR requires it
    if (marketActivity.length > TIME_PERIOD) {
      val days = marketActivity.slice(marketActivity.length - TIME_PERIOD - 1, marketActivity.length)

      val highs = days.map(day => day.high).toArray
      val lows = days.map(day => day.low).toArray
      val close = days.map(day => day.close).toArray

      val result = new Array[Double](1)

      val retCode = core.atr(0, days.length - 1, highs, lows, close, TIME_PERIOD, new MInteger, new MInteger, result)

      if (retCode == RetCode.Success) {
        val atr = new AvgTrueRange

        atr.value = result.head

        return Some(atr)
      }
    }

    None
  }

  def movingAverage(timePeriod: Int, avgType: AvgType): Option[MovingAverage] = {

    if (marketActivity.length >= timePeriod) {
      val days = marketActivity.slice(marketActivity.length - timePeriod, marketActivity.length)

      val closingPrices = days.map(day => day.close).toArray

      val avg = new Array[Double](1)

      var retCode:RetCode = null

      if(avgType == AvgType.EMA) {
        retCode = core.ema(0, closingPrices.length - 1, closingPrices, timePeriod, new MInteger, new MInteger, avg)
      } else {
        retCode = core.sma(0, closingPrices.length - 1, closingPrices, timePeriod, new MInteger, new MInteger, avg)
      }

      if (retCode == RetCode.Success) {
        val ma = new MovingAverage

        ma.avgType = avgType
        ma.timePeriod = timePeriod
        ma.value = avg.head

        return Some(ma)
      }
    }

    None
  }

  def macd(): Option[MACD] = {

    val SLOW_TIME_PERIOD = 26
    val FAST_TIME_PERIOD = 12
    val SIGNAL_PERIOD = 9

    val TOTAL_PERIODS = SLOW_TIME_PERIOD + SIGNAL_PERIOD

    if (marketActivity.length >= TOTAL_PERIODS) {
      val days = marketActivity.slice(marketActivity.length - TOTAL_PERIODS, marketActivity.length)

      val close = days.map(day => day.close).toArray

      val macd = new Array[Double](1)
      val macdSignal = new Array[Double](1)
      val macdHist = new Array[Double](1)

      //TIME_PERIOD - 1 in lookback period, as array is indexed 0-13
      val retCode = core.macd(0, close.length - 1, close, FAST_TIME_PERIOD, SLOW_TIME_PERIOD, SIGNAL_PERIOD, new MInteger, new MInteger, macd, macdSignal, macdHist)

      if (retCode == RetCode.Success) {
        val macdObj = new MACD

        macdObj.fastPeriod = FAST_TIME_PERIOD
        macdObj.slowPeriod = SLOW_TIME_PERIOD
        macdObj.signalPeriod = SIGNAL_PERIOD

        macdObj.macd = macd.head
        macdObj.macdSignal = macdSignal.head
        macdObj.macdHist = macdHist.head

        return Some(macdObj)
      }
    }

    None
  }

  def RSI(): Option[RSI] = {

    val TIME_PERIOD = 14

    //must include 1 extra day, as first element in array needs a prior element
    if (marketActivity.length > TIME_PERIOD) {
      val days = marketActivity.slice(marketActivity.length - TIME_PERIOD - 1, marketActivity.length)

      val closingPrices = days.map(day => day.close).toArray

      val outRSI = new Array[Double](1)

      val retCode = core.rsi(0, closingPrices.length - 1, closingPrices, TIME_PERIOD, new MInteger, new MInteger, outRSI)

      if (retCode == RetCode.Success) {
        val rsi = new RSI

        rsi.timePeriod = TIME_PERIOD
        rsi.value = outRSI.head

        return Some(rsi)
      }
    }

    None
  }

  def OBV(): Option[OnBalanceVolume] = {

    if(marketActivity.length > 1 && analyzedMarketDays.nonEmpty) {

      val days = marketActivity.slice(marketActivity.length - 2, marketActivity.length)

      val prices = days.map(day => day.close)
      val volume = days.map(day => day.volume)

      val yesterdayOBV = analyzedMarketDays.last.onBalanceVolume

      val todayOBV = new OnBalanceVolume

      yesterdayOBV match {
        case Some(obv) =>
          if(prices.head > prices.last) {
            todayOBV.value = obv.value + volume.last
          } else if(prices.head < prices.last) {
            todayOBV.value = obv.value - volume.last
          }
        case None =>
          if(prices.head > prices.last) {
            todayOBV.value = volume.last
          } else if(prices.head < prices.last) {
            todayOBV.value = -volume.last
          }
      }

      return Some(todayOBV)
    }

    None
  }
}
