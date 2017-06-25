package com.leetcode.waverider.engines

import java.io.File

import com.github.tototoshi.csv._
import com.leetcode.waverider.adapters.Adapter
import com.leetcode.waverider.data.indicators.IndicatorBuilder
import com.leetcode.waverider.data.indicators.custom.TrendStatsBuilder
import com.leetcode.waverider.data.indicators.eastern.CandlePatternsBuilder
import com.leetcode.waverider.data.indicators.western.generic.rate.{MOMBuilder, ROCRBuilder}
import com.leetcode.waverider.data.indicators.western.generic.signals.{MACDBuilder, RSIBuilder}
import com.leetcode.waverider.data.indicators.western.generic.trend.MABuilder
import com.leetcode.waverider.data.indicators.western.generic.trend.MovingAverage.AvgType
import com.leetcode.waverider.data.indicators.western.generic.volatility.BBandBuilder
import com.leetcode.waverider.data.indicators.western.typed.discovery.MFIBuilder
import com.leetcode.waverider.data.indicators.western.typed.volatility.ATRBuilder
import com.leetcode.waverider.data.{AnalyzedMarketDay, Label, RawMarketDay, Trend}
import com.leetcode.waverider.utils.{LastNQueue, TrendUtils}
import com.tictactec.ta.lib.Core

import scala.collection.mutable.ListBuffer

object IndicatorEngine {

  val supportedFeatures = Set(ATRBuilder(14),
    BBandBuilder(21, 2, "close"),
    CandlePatternsBuilder(10),
    MACDBuilder(12, 12, 9, "close"),
    MFIBuilder(14),
    MOMBuilder(3, "close"),
    MOMBuilder(4, "volume"),
    MOMBuilder(1, "close"),
    MOMBuilder(1, "volume"),
    MABuilder(200, AvgType.EMA, "close"),
    MABuilder(100, AvgType.EMA, "close"),
    MABuilder(50, AvgType.EMA, "close"),
    MABuilder(25, AvgType.EMA, "close"),
    MABuilder(15, AvgType.EMA, "close"),
    MABuilder(10, AvgType.EMA, "close"),
    MABuilder(5, AvgType.EMA, "close"),
    MABuilder(2, AvgType.EMA, "close"),
    ROCRBuilder(12, "close"),
    ROCRBuilder(3, "close"),
    RSIBuilder(12, "close"),
    RSIBuilder(6, "close"),
    TrendStatsBuilder())
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
  var isTrendUp:Boolean = false
  var current: Trend = _

  def analyzeNext(day: RawMarketDay, indicators: Set[IndicatorBuilder]): Unit = {
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
        "YES"
      } else {
        "NO"
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

  /**
    * With this definition, the earliest we can have a trend is 3 days. Start on the first day to record close,
    * on the second day establish direction, on the third a reversal may occur.
    */
  def trend(): Unit = {
    if(rawDays.length == 1) {
      lastInflectionIdx = 0
    } else if(rawDays.length == 2) {
      isTrendUp = rawDays.last.close > rawDays(lastInflectionIdx).close
    } else {
      val pctChange = (rawDays.last.close - rawDays(lastInflectionIdx).close)/rawDays(lastInflectionIdx).close
      val duration = (rawDays.length - 1) - lastInflectionIdx

      if(isTrendUp) {
        if(rawDays.last.close < rawDays(rawDays.length - 2).close) { // if we break trend
          isTrendUp = false
          splitTrend(pctChange, duration)
        } else {
          current = new Trend(Some(lastInflectionIdx), Some(rawDays.length - 1), Some(pctChange), Some(duration))
        }
      } else if(!isTrendUp) {
        if(rawDays.last.close > rawDays(rawDays.length - 2).close) { //same
          isTrendUp = true
          splitTrend(pctChange, duration)
        } else {
          current = new Trend(Some(lastInflectionIdx), Some(rawDays.length - 1), Some(pctChange), Some(duration))
        }
      }
    }
  }

  private def splitTrend(pctChange: Double, duration: Int): Unit = {
    trendQueue.enqueue(new Trend(Some(lastInflectionIdx), Some(rawDays.length - 1),
      Some(pctChange), Some(duration)))

    lastInflectionIdx = rawDays.length - 1

    current = new Trend(Some(rawDays.length - 1), Some(rawDays.length - 1), Some(0), Some(0))
  }
}
