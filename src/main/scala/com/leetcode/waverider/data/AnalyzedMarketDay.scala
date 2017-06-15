package com.leetcode.waverider.data

import scala.collection.mutable.ListBuffer

/**
  * Created by Ben on 4/19/2017.
  */
class AnalyzedMarketDay(val day: RawMarketDay, val indicators: List[Writable]) extends RawMarketDay with Writable {

  override def toString = s"AnalyzedMarketDay($day, $indicators)"

  override def headers: List[String] = {
    val headers = new ListBuffer[String]()

    indicators.foreach(indicator => headers ++ indicator.headers)

    headers.toList
  }

  override def features: List[String] = {
    val features = new ListBuffer[String]()

    indicators.foreach(indicator => features ++ indicator.features)

    features.toList
  }

  def getIndicatorCount:Int = {indicators.length}

}