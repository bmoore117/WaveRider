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

  override def headers: List[String] = {
    rsi.headers ++ MACD.headers ++ ma200.headers ++ ma100.headers ++ ma50.headers ++ ma25.headers ++ ma15.headers ++ ma10.headers ++ ma5.headers ++ avgTrueRange.headers ++ bBand.headers ++ onBalanceVolume.headers
  }

  override def features: List[String] = {
    rsi.features ++ MACD.features ++ ma200.features ++ ma100.features ++ ma50.features ++ ma25.features ++ ma15.features ++ ma10.features ++ ma5.features ++ avgTrueRange.features ++ bBand.features ++ onBalanceVolume.features
  }
}
