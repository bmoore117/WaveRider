package com.leetcode.waverider

import com.leetcode.waverider.indicators.Writable
import com.leetcode.waverider.indicators.momentum.RSI
import com.leetcode.waverider.indicators.trend.{MACD, MovingAverage}
import com.leetcode.waverider.indicators.volatility.{AvgTrueRange, BBand}
import com.leetcode.waverider.indicators.volume.OnBalanceVolume

/**
  * Created by Ben on 4/19/2017.
  */
class AnalyzedMarketDay(val rsi: RSI, val MACD: MACD, val ma200: MovingAverage,
                        val ma100: MovingAverage, val ma50: MovingAverage,
                        val ma25: MovingAverage, val ma15: MovingAverage,
                        val ma10: MovingAverage, val ma5: MovingAverage,
                        val avgTrueRange: AvgTrueRange, val bBand: BBand,
                        val onBalanceVolume: OnBalanceVolume) extends Writable {




  override def toString = s"AnalyzedMarketDay($rsi, $MACD, $ma200, $ma100, $ma50, $ma25, $ma15, $ma10, $ma5, $avgTrueRange, $bBand, $onBalanceVolume)"

  override def getFeatureHeaderList: List[String] = {
    rsi.getFeatureHeaderList ++ MACD.getFeatureHeaderList ++ ma200.getFeatureHeaderList ++ ma100.getFeatureHeaderList ++ ma50.getFeatureHeaderList ++ ma25.getFeatureHeaderList ++ ma15.getFeatureHeaderList ++ ma10.getFeatureHeaderList ++ ma5.getFeatureHeaderList ++ avgTrueRange.getFeatureHeaderList ++ bBand.getFeatureHeaderList ++ onBalanceVolume.getFeatureHeaderList
  }

  override def toFeatureList: List[String] = {
    rsi.toFeatureList ++ MACD.toFeatureList ++ ma200.toFeatureList ++ ma100.toFeatureList ++ ma50.toFeatureList ++ ma25.toFeatureList ++ ma15.toFeatureList ++ ma10.toFeatureList ++ ma5.toFeatureList ++ avgTrueRange.toFeatureList ++ bBand.toFeatureList ++ onBalanceVolume.toFeatureList
  }
}
