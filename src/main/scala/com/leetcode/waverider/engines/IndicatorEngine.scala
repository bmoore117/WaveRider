package com.leetcode.waverider.engines

import java.io.File

import com.github.tototoshi.csv._
import com.leetcode.waverider.adapters.Adapter
import com.leetcode.waverider.data.indicators.IndicatorSettings
import com.leetcode.waverider.data.indicators.momentum.RSISettings
import com.leetcode.waverider.data.indicators.trend.MovingAverage.AvgType
import com.leetcode.waverider.data.indicators.trend.{MACDSettings, MovingAverageSettings}
import com.leetcode.waverider.data.indicators.volatility.{AvgTrueRangeSettings, BBandSettings}
import com.leetcode.waverider.data.indicators.volume.OnBalanceVolume
import com.leetcode.waverider.data.{AnalyzedMarketDay, RawMarketDay, Trend}
import com.leetcode.waverider.utils.TrendUtils
import com.tictactec.ta.lib.Core

import scala.collection.mutable.ListBuffer

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
  
  val rawDays = new ListBuffer[RawMarketDay]()
  val analyzedMarketDays = new ListBuffer[AnalyzedMarketDay]()

  val core = new Core

  var lastInflectionIdx:Int = 0
  var isTrendUp:Boolean = false

  def analyzeNext(day: RawMarketDay, indicators: List[IndicatorSettings]): Unit = {
    rawDays.append(day)

    val results = trend() :: indicators.map(indicator =>
      indicator.instantiateIndicator(core, rawDays, analyzedMarketDays))

    analyzedMarketDays.append(new AnalyzedMarketDay(day, results))
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

  def trend(): Trend = {
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
