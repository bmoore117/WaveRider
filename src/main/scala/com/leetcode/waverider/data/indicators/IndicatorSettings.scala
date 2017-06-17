package com.leetcode.waverider.data.indicators

import com.leetcode.waverider.data.{AnalyzedMarketDay, RawMarketDay, Writable}
import com.tictactec.ta.lib.Core

import scala.collection.mutable.ListBuffer

/**
  * Created by Ben on 6/15/2017.
  */
trait IndicatorSettings {
  def instantiateIndicator(core: Core, rawDays: ListBuffer[RawMarketDay], analyzedDays: ListBuffer[AnalyzedMarketDay]): Writable
}
