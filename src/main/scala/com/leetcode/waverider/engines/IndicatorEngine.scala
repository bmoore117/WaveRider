package com.leetcode.waverider.engines

import java.io.File

import com.github.tototoshi.csv._
import com.leetcode.waverider.adapters.Adapter
import com.leetcode.waverider.data.indicators.momentum.{RSI, RSISettings}
import com.leetcode.waverider.data.indicators.trend.MovingAverage.AvgType
import com.leetcode.waverider.data.indicators.trend.{MACD, MACDSettings, MovingAverage, MovingAverageSettings}
import com.leetcode.waverider.data.indicators.volatility.{AvgTrueRange, AvgTrueRangeSettings, BBand, BBandSettings}
import com.leetcode.waverider.data.indicators.volume.OnBalanceVolume
import com.leetcode.waverider.data.{AnalyzedMarketDay, RawMarketDay, Trend}
import com.leetcode.waverider.utils.TrendUtils
import com.tictactec.ta.lib.{Core, MAType, MInteger, RetCode}

import scala.collection.mutable.ArrayBuffer

object AnalysisType extends Enumeration {
  val TREND, CLOSING_CHANGE = Value
  type AnalysisType = Value
}

object IndicatorEngine {

  val supportedFeatures = List(BBandSettings(21, 2), AvgTrueRangeSettings(14), MovingAverageSettings(200, AvgType.EMA),
    MovingAverageSettings(100, AvgType.EMA), MovingAverageSettings(50, AvgType.EMA), MovingAverageSettings(25, AvgType.EMA),
    MovingAverageSettings(15, AvgType.EMA), MovingAverageSettings(10, AvgType.EMA), MovingAverageSettings(5, AvgType.EMA),
    MovingAverageSettings(2, AvgType.EMA), MACDSettings(12, 12, 9), RSISettings(14))
}

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
class IndicatorEngine(val market: Adapter, val analysisType: AnalysisType.AnalysisType) {
  
  val rawDays = new ArrayBuffer[RawMarketDay]()
  val analyzedMarketDays = new ArrayBuffer[AnalyzedMarketDay]()

  val core = new Core

  var lastInflectionIdx:Int = 0
  var isTrendUp:Boolean = false

  def analyzeNext(day: RawMarketDay, indicators: List()): Unit = {
    rawDays.append(day)







    if(analysisType == AnalysisType.TREND) {
      val rsi = RSI()
      val macd_result = macd()
      val ema200 = movingAverage(200, AvgType.EMA)
      val ema100 = movingAverage(100, AvgType.EMA)
      val ema50 = movingAverage(50, AvgType.EMA)
      val ema25 = movingAverage(25, AvgType.EMA)
      val ema15 = movingAverage(15, AvgType.EMA)
      val ema10 = movingAverage(10, AvgType.EMA)
      val ema5 = movingAverage(5, AvgType.EMA)
      val ema2 = movingAverage(2, AvgType.EMA)
      val atr = averageTrueRange()
      val band = bollinger()
      val obv = OBV()
      val historicalTrend = trend(day)

      val indicators = List(rsi, macd_result, ema200, ema100, ema50, ema25, ema15, ema10, ema5, ema2, atr, band, obv, historicalTrend)
      analyzedMarketDays.append(new AnalyzedMarketDay(day, indicators))
    } else {
      val ema200 = movingAverage(200, AvgType.EMA)
      val ema100 = movingAverage(100, AvgType.EMA)
      val ema50 = movingAverage(50, AvgType.EMA)
      val ema25 = movingAverage(25, AvgType.EMA)
      val ema15 = movingAverage(15, AvgType.EMA)
      val ema10 = movingAverage(10, AvgType.EMA)
      val ema5 = movingAverage(5, AvgType.EMA)
      val ema2 = movingAverage(2, AvgType.EMA)

      val indicators = List(ema200, ema100, ema50, ema25, ema15, ema10, ema5, ema2)
      analyzedMarketDays.append(new AnalyzedMarketDay(day, indicators))
    }
  }

  def writeAnalysis(): Unit = {

    val prices = rawDays.map(day => day.close).toList

    val trends = if(analysisType == AnalysisType.TREND) {
      val temp = TrendUtils.buildTrendData(prices)
      TrendUtils.findEndOfTrendChanges(prices, temp)
    } else {
      TrendUtils.buildTrendData(prices, 10)
    }

    val writer = CSVWriter.open(new File("train.csv"))
    writer.writeRow(analyzedMarketDays.head.headers ++ trends.head.headers)

    analyzedMarketDays.indices.foreach(i => {
      val day = analyzedMarketDays(i)
      if(i < analyzedMarketDays.length - 1 && day.features.forall(value => !value.isEmpty)) {
        writer.writeRow(day.features ++ trends(i).features)
      }
    })
    writer.close()
  }

  def bollinger(params: BBandSettings): BBand = {
    val band = new BBand(params)

    if(rawDays.length >= params.timePeriod) {

      val prices = rawDays.slice(rawDays.length - params.timePeriod, rawDays.length).map(day => day.close).toArray

      val upperBand: Array[Double] = new Array[Double](1)
      val avg: Array[Double] = new Array[Double](1)
      val lowerBand: Array[Double] = new Array[Double](1)

      val begin = new MInteger
      val nbElement = new MInteger

      val retCode = core.bbands(0, prices.length - 1, prices, params.timePeriod, params.distanceDeviations,
        params.distanceDeviations, MAType.Sma, begin, nbElement, upperBand, avg, lowerBand)

      if(retCode == RetCode.Success) {
        band.upperBand = Some(upperBand.head)
        band.avg = Some(avg.head)
        band.lowerBand = Some(lowerBand.head)
      }
    }

    band
  }

  def averageTrueRange(params: AvgTrueRangeSettings): AvgTrueRange = {
    val atr = new AvgTrueRange(params)

    //strictly greater than, as we need 15 points for a 14 day ATR: we need 1 point past the last, as TR requires it
    if(rawDays.length > params.timePeriod) {
      val days = rawDays.slice(rawDays.length - params.timePeriod - 1, rawDays.length)

      val highs = days.map(day => day.high).toArray
      val lows = days.map(day => day.low).toArray
      val close = days.map(day => day.close).toArray

      val result = new Array[Double](1)

      val retCode = core.atr(0, days.length - 1, highs, lows, close, params.timePeriod, new MInteger, new MInteger, result)

      if(retCode == RetCode.Success) {
        atr.value = Some(result.head)
      }
    }

    atr
  }

  def movingAverage(params: MovingAverageSettings): MovingAverage = {
    val ma = new MovingAverage(params)

    if(rawDays.length >= params.timePeriod) {
      val days = rawDays.slice(rawDays.length - params.timePeriod, rawDays.length)

      val closingPrices = days.map(day => day.close).toArray

      val avg = new Array[Double](1)

      var retCode:RetCode = null

      if(params.avgType == AvgType.EMA) {
        retCode = core.ema(0, closingPrices.length - 1, closingPrices, params.timePeriod, new MInteger, new MInteger, avg)
      } else {
        retCode = core.sma(0, closingPrices.length - 1, closingPrices, params.timePeriod, new MInteger, new MInteger, avg)
      }

      if (retCode == RetCode.Success) {
        ma.value = Some(avg.head)
      }
    }

    ma
  }

  def macd(params: MACDSettings): MACD = {
    val totalPeriods = params.slowTimePeriod + params.signalPeriod - 1

    val macdObj = new MACD(params)

    if(rawDays.length >= totalPeriods) {
      val days = rawDays.slice(rawDays.length - totalPeriods, rawDays.length)

      val close = days.map(day => day.close).toArray

      val macd = new Array[Double](1)
      val macdSignal = new Array[Double](1)
      val macdHist = new Array[Double](1)

      val retCode = core.macd(0, close.length - 1, close, params.fastTimePeriod, params.slowTimePeriod, params.signalPeriod, new MInteger, new MInteger, macd, macdSignal, macdHist)

      if(retCode == RetCode.Success) {
        macdObj.macd = Some(macd.head)
        macdObj.macdSignal = Some(macdSignal.head)
        macdObj.macdHist = Some(macdHist.head)
      }
    }

    macdObj
  }

  def RSI(params: RSISettings): RSI = {
    val rsi = new RSI(params)

    //must include 1 extra day, as first element in array needs a prior element
    if(rawDays.length > params.timePeriod) {
      val days = rawDays.slice(rawDays.length - params.timePeriod - 1, rawDays.length)

      val closingPrices = days.map(day => day.close).toArray

      val outRSI = new Array[Double](1)

      val retCode = core.rsi(0, closingPrices.length - 1, closingPrices, params.timePeriod, new MInteger, new MInteger, outRSI)

      if (retCode == RetCode.Success) {
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

  def trend(today: RawMarketDay): Trend = {
    if(rawDays.length == 1) {
      new Trend(None, None, None, None)
    } else {

      if(isTrendUp) {
        if(rawDays.last.close < rawDays(rawDays.length - 2).close) {
          lastInflectionIdx = rawDays.length - 2
          isTrendUp = false
        }
      } else if(!isTrendUp) {
        if(rawDays.last.close > rawDays(rawDays.length - 2).close) {
          lastInflectionIdx = rawDays.length - 2
          isTrendUp = true
        }
      }

      val valueChg = (rawDays.last.close - rawDays(lastInflectionIdx).close) / rawDays(lastInflectionIdx).close
      val duration = (rawDays.length - 1) - lastInflectionIdx

      new Trend(None, None, Some(valueChg), Some(duration))
    }
  }
}
