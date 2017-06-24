package com.leetcode.waverider.engines

import java.io.File

import com.github.tototoshi.csv._
import com.leetcode.waverider.adapters.Adapter
import com.leetcode.waverider.data.indicators.IndicatorSettings
import com.leetcode.waverider.data.indicators.custom.TrendStatsBuilder
import com.leetcode.waverider.data.indicators.eastern.CandlePatternsBuilder
import com.leetcode.waverider.data.indicators.western.generic.rate.{MOMSettings, ROCRSettings}
import com.leetcode.waverider.data.indicators.western.generic.signals.{MACDSettings, RSISettings}
import com.leetcode.waverider.data.indicators.western.generic.trend.MovingAverage.AvgType
import com.leetcode.waverider.data.indicators.western.generic.trend.MovingAverageSettings
import com.leetcode.waverider.data.indicators.western.typed.volatility.ATRSettings
import com.leetcode.waverider.data.indicators.western.generic.volatility.BBandSettings
import com.leetcode.waverider.data.indicators.western.typed.discovery.MFISettings
import com.leetcode.waverider.data.{AnalyzedMarketDay, Label, RawMarketDay, Trend}
import com.leetcode.waverider.utils.{LastNQueue, TrendUtils}
import com.tictactec.ta.lib.Core

import scala.collection.mutable.ListBuffer

object IndicatorEngine {

  /*
  * BBandSettings(21, 2, "close"), ATRSettings(14), MovingAverageSettings(200, AvgType.EMA, "close"),
    MovingAverageSettings(100, AvgType.EMA, "close"), MovingAverageSettings(50, AvgType.EMA, "close"), MovingAverageSettings(25, AvgType.EMA, "close"),
    MovingAverageSettings(15, AvgType.EMA, "close"), MovingAverageSettings(10, AvgType.EMA, "close"), MovingAverageSettings(5, AvgType.EMA, "close"),
    MovingAverageSettings(2, AvgType.EMA, "close"), MACDSettings(12, 12, 9, "close"), RSISettings(14, "close"), MOMSettings(14, "close"), MOMSettings(14, "volume"),
    ROCRSettings(14, "close"), MFISettings(14), */
  val supportedFeatures = List(CandlePatternsBuilder(10), TrendStatsBuilder())
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
class IndicatorEngine(val market: Adapter, trendWindowToPredict: Option[Int]) {
  val rawDays = new ListBuffer[RawMarketDay]()
  var analyzedMarketDays = new ListBuffer[AnalyzedMarketDay]()
  val core = new Core
  val trendQueue = new LastNQueue[Trend](100)

  var lastInflectionIdx:Int = 0
  var startingClose:Double = 0
  var isTrendUp:Boolean = false
  var current: Trend = null

  def analyzeNext(day: RawMarketDay, indicators: List[IndicatorSettings]): Unit = {
    rawDays.append(day)

    trend()

    //trend here supplies the instantaneous trend
    val results = indicators.map(indicator =>
      indicator.instantiateIndicator(core, rawDays, analyzedMarketDays, trendQueue, current))

    analyzedMarketDays.append(new AnalyzedMarketDay(day, results))
  }

  def writeAnalysis(): Unit = {
    val prices = rawDays.map(day => day.close).toList

    //need to further break these down into a categorical: Trend up or down, for classification
    val labels = (if(trendWindowToPredict.isDefined) {
      analyzedMarketDays = analyzedMarketDays.dropRight(trendWindowToPredict.get)
      TrendUtils.buildTrendData(prices, trendWindowToPredict.get) //fixed-window trend
    } else {
      val temp = TrendUtils.buildTrendData(prices) //instantaneous trend, i.e. hard mode prediction
      TrendUtils.findEndOfTrendChanges(prices, temp)
    }).map(trend => {
      val direction = if(math.signum(trend.pctDelta.get) > 0) {
        "UP"
      } else {
        "DOWN"
      }
      new Label(List("CLASS"), List(direction))
    })

    val writer = CSVWriter.open(new File("train.csv"))
    writer.writeRow(analyzedMarketDays.head.headers ++ labels.head.headers)

    analyzedMarketDays.indices.foreach(i => {
      val day = analyzedMarketDays(i)
      if(i < analyzedMarketDays.length - 1 && day.features.forall(value => !value.isEmpty)) {
        writer.writeRow(day.features ++ labels(i).features)
      }
    })
    writer.close()
  }

  def trend(): Unit = {
    if(rawDays.length == 1) {
      lastInflectionIdx = 0
      startingClose = rawDays.head.close

      isTrendUp = rawDays.head.close > rawDays.head.open
      val pctChange = (rawDays.head.close - rawDays.head.open)/rawDays.head.open
      current = new Trend(Some(0), Some(0), Some(pctChange), Some(1))
    } else {
      val pctChange = Some(((rawDays.last.close - startingClose)/startingClose).abs)
      val duration = Some(rawDays.length - lastInflectionIdx)

      if(isTrendUp) {
        if(rawDays.last.close < startingClose) { // if we break trend

          trendQueue.enqueue(new Trend(Some(lastInflectionIdx), Some(rawDays.length - 1),
            pctChange, duration))

          lastInflectionIdx = rawDays.length - 1
          startingClose = rawDays.last.close
          isTrendUp = false

          current = new Trend(Some(rawDays.length - 1), Some(rawDays.length - 1), Some(0), Some(0))
        } else {
          current = new Trend(Some(lastInflectionIdx), Some(rawDays.length - 1), pctChange, duration)
        }
      } else if(!isTrendUp) {
        if(rawDays.last.close > startingClose) { //same

          trendQueue.enqueue(new Trend(Some(lastInflectionIdx), Some(rawDays.length - 1),
            pctChange, duration))

          lastInflectionIdx = rawDays.length - 1
          startingClose = rawDays.last.close
          isTrendUp = true

          current = new Trend(Some(rawDays.length - 1), Some(rawDays.length - 1), Some(0), Some(0))
        } else {
          current = new Trend(Some(lastInflectionIdx), Some(rawDays.length - 1), pctChange, duration)
        }
      }
    }
  }

  def reversal(): Unit = {
    /*if(rawDays.length == 1) {
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
    }*/
  }
}
