package com.leetcode.waverider

import com.leetcode.waverider.indicators.momentum.RSI
import com.leetcode.waverider.indicators.trend.{MACD, MovingAverage}
import com.leetcode.waverider.indicators.volatility.{AvgTrueRange, BBand}
import com.leetcode.waverider.indicators.volume.OnBalanceVolume

/**
  * Created by Ben on 4/19/2017.
  */
class AnalyzedMarketDay(val rsi: Option[RSI], val mACD: Option[MACD], val ma200: Option[MovingAverage],
                        val ma100: Option[MovingAverage], val ma50: Option[MovingAverage],
                        val ma25: Option[MovingAverage], val ma15: Option[MovingAverage],
                        val ma10: Option[MovingAverage], val ma5: Option[MovingAverage],
                        val avgTrueRange: Option[AvgTrueRange], val bBand: Option[BBand],
                        val onBalanceVolume: Option[OnBalanceVolume]) {


  override def toString = s"AnalyzedMarketDay($rsi, $mACD, $ma200, $ma100, $ma50, $ma25, $ma15, $ma10, $ma5, $avgTrueRange, $bBand, $onBalanceVolume)"
}
