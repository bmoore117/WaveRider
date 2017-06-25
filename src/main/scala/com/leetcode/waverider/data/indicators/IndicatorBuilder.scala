package com.leetcode.waverider.data.indicators

import com.leetcode.waverider.data.{AnalyzedMarketDay, RawMarketDay, Trend, Writable}
import com.leetcode.waverider.utils.LastNQueue
import com.tictactec.ta.lib.Core

import scala.collection.mutable.ListBuffer

/**
  * Created by Ben on 6/15/2017.
  */
trait IndicatorBuilder {
  def instantiateIndicator(core: Core, rawDays: ListBuffer[RawMarketDay], analyzedDays: ListBuffer[AnalyzedMarketDay],
                           last100Trends:LastNQueue[Trend], current: Trend): Writable
}
