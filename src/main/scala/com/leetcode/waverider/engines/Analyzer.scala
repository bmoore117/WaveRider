package com.leetcode.waverider.engines

import java.io.File

import com.github.tototoshi.csv._
import com.leetcode.waverider.adapters.Adapter
import com.leetcode.waverider.data.{AnalyzedMarketDay, RawMarketDay}
import com.leetcode.waverider.data.indicators.momentum.RSI
import com.leetcode.waverider.data.indicators.trend.MovingAverage.AvgType
import com.leetcode.waverider.data.indicators.trend.MovingAverage.AvgType.AvgType
import com.leetcode.waverider.data.indicators.trend.{MACD, MovingAverage}
import com.leetcode.waverider.data.indicators.volatility.{AvgTrueRange, BBand}
import com.leetcode.waverider.data.indicators.volume.OnBalanceVolume
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
class Analyzer(val market: Adapter) {
  
  val rawDays = new ArrayBuffer[RawMarketDay]()
  val analyzedMarketDays = new ArrayBuffer[AnalyzedMarketDay]()

  val core = new Core

  def writeLabeling(): Unit = {

    val threshold = 5

    val writer = CSVWriter.open(new File("trainLabels.csv"))
    writer.writeRow(List("index", "tradeable"))

    rawDays.indices.foreach(i => {
      //if the next 5 days fall within the range of data we have
      if(i+5 <= rawDays.length - 1) {
        val startingPoint = rawDays(i).close

        var lowestPct:Double = 0
        var highestPct:Double = 0

        for(j <- i+1 to i+threshold) {
          val pctChange = (rawDays(j).close - startingPoint) / startingPoint

          if(pctChange > highestPct) {
            highestPct = pctChange
          }

          if(pctChange < lowestPct) {
            lowestPct = pctChange
          }
        }

        if(highestPct >= threshold / 100.0 && lowestPct <= threshold / 1000.0) {
          writer.writeRow(List((i+1).toString, "1"))
        } else {
          writer.writeRow(List((i+1).toString, "0"))
        }
      } else {
        writer.writeRow(List((i+1).toString, "0"))
      }
    })

    writer.close()
  }

  def analyzeNext(day: RawMarketDay): Unit = {
    rawDays.append(day)

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
  }

  def writeAnalysis(): Unit = {
    val writer = CSVWriter.open(new File("train.csv"))
    writer.writeRow("index" :: analyzedMarketDays.head.headers)

    analyzedMarketDays.indices.foreach(i => {
      val day = analyzedMarketDays(i)
      writer.writeRow((i+1).toString :: day.features)
    })
    writer.close()
  }


  def bollinger(): BBand = {
    val TIME_PERIOD = 21
    val DISTANCE_DEVIATIONS = 2

    val band = new BBand

    if (rawDays.length >= TIME_PERIOD) {

      val prices = rawDays.slice(rawDays.length - TIME_PERIOD, rawDays.length).map(day => day.close).toArray

      val upperBand: Array[Double] = new Array[Double](1)
      val avg: Array[Double] = new Array[Double](1)
      val lowerBand: Array[Double] = new Array[Double](1)

      val begin = new MInteger
      val nbElement = new MInteger

      val retCode = core.bbands(0, prices.length - 1, prices, TIME_PERIOD, DISTANCE_DEVIATIONS, DISTANCE_DEVIATIONS, MAType.Sma, begin, nbElement, upperBand, avg, lowerBand)

      if (retCode == RetCode.Success) {
        band.upperBand = Some(upperBand.head)
        band.avg = Some(avg.head)
        band.lowerBand = Some(lowerBand.head)
        band.bandDistance = DISTANCE_DEVIATIONS
      }
    }

    band
  }

  def averageTrueRange(): AvgTrueRange = {
    val TIME_PERIOD = 14

    val atr = new AvgTrueRange

    //strictly greater than, as we need 15 points for a 14 day ATR: we need 1 point past the last, as TR requires it
    if (rawDays.length > TIME_PERIOD) {
      val days = rawDays.slice(rawDays.length - TIME_PERIOD - 1, rawDays.length)

      val highs = days.map(day => day.high).toArray
      val lows = days.map(day => day.low).toArray
      val close = days.map(day => day.close).toArray

      val result = new Array[Double](1)

      val retCode = core.atr(0, days.length - 1, highs, lows, close, TIME_PERIOD, new MInteger, new MInteger, result)

      if (retCode == RetCode.Success) {
        atr.value = Some(result.head)
      }
    }

    atr
  }

  def movingAverage(timePeriod: Int, avgType: AvgType): MovingAverage = {

    val ma = new MovingAverage
    ma.avgType = avgType
    ma.timePeriod = timePeriod

    if (rawDays.length >= timePeriod) {
      val days = rawDays.slice(rawDays.length - timePeriod, rawDays.length)

      val closingPrices = days.map(day => day.close).toArray

      val avg = new Array[Double](1)

      var retCode:RetCode = null

      if(avgType == AvgType.EMA) {
        retCode = core.ema(0, closingPrices.length - 1, closingPrices, timePeriod, new MInteger, new MInteger, avg)
      } else {
        retCode = core.sma(0, closingPrices.length - 1, closingPrices, timePeriod, new MInteger, new MInteger, avg)
      }

      if (retCode == RetCode.Success) {
        ma.value = Some(avg.head)
      }
    }

    ma
  }

  def macd(): MACD = {

    val SLOW_TIME_PERIOD = 26
    val FAST_TIME_PERIOD = 12
    val SIGNAL_PERIOD = 9

    val TOTAL_PERIODS = SLOW_TIME_PERIOD + SIGNAL_PERIOD - 1

    val macdObj = new MACD

    if (rawDays.length >= TOTAL_PERIODS) {
      val days = rawDays.slice(rawDays.length - TOTAL_PERIODS, rawDays.length)

      val close = days.map(day => day.close).toArray

      val macd = new Array[Double](1)
      val macdSignal = new Array[Double](1)
      val macdHist = new Array[Double](1)

      val retCode = core.macd(0, close.length - 1, close, FAST_TIME_PERIOD, SLOW_TIME_PERIOD, SIGNAL_PERIOD, new MInteger, new MInteger, macd, macdSignal, macdHist)

      if (retCode == RetCode.Success) {

        macdObj.fastPeriod = FAST_TIME_PERIOD
        macdObj.slowPeriod = SLOW_TIME_PERIOD
        macdObj.signalPeriod = SIGNAL_PERIOD

        macdObj.macd = Some(macd.head)
        macdObj.macdSignal = Some(macdSignal.head)
        macdObj.macdHist = Some(macdHist.head)

      }
    }

    macdObj
  }

  def RSI(): RSI = {

    val TIME_PERIOD = 14

    val rsi = new RSI

    //must include 1 extra day, as first element in array needs a prior element
    if (rawDays.length > TIME_PERIOD) {
      val days = rawDays.slice(rawDays.length - TIME_PERIOD - 1, rawDays.length)

      val closingPrices = days.map(day => day.close).toArray

      val outRSI = new Array[Double](1)

      val retCode = core.rsi(0, closingPrices.length - 1, closingPrices, TIME_PERIOD, new MInteger, new MInteger, outRSI)

      if (retCode == RetCode.Success) {

        rsi.timePeriod = TIME_PERIOD
        rsi.value = Some(outRSI.head)
      }
    }

    rsi
  }

  def OBV(): OnBalanceVolume = {

    val todayOBV = new OnBalanceVolume

    if(rawDays.length > 1 && analyzedMarketDays.nonEmpty) {

      val days = rawDays.slice(rawDays.length - 2, rawDays.length)

      val prices = days.map(day => day.close)
      val volume = days.map(day => day.volume)

      val yesterdayOBV = analyzedMarketDays.last.onBalanceVolume

      yesterdayOBV.value match {
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
