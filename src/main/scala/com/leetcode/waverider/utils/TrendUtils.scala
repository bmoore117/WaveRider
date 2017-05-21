/*
package com.leetcode.waverider.utils

import com.leetcode.waverider.data.Trend

import scala.collection.mutable

/**
  * Created by Ben on 4/27/2017.
  */
object TrendUtils {

  def findLocalMaxima(list:List[Double]):List[Int] = {

    if(list.length < 2) {
      return List[Int]()
    }

    var results = new mutable.ArrayBuffer[Int]()
    var prevPt = list.head

    var isUp = false

    for(i <- list.indices) {

      val point = list(i)

      if(prevPt < point) {
        if(i == list.length - 1) {
          results += i
        }
        isUp = true
      } else if(prevPt > point) {
        if(isUp) {
          results += i - 1
        }
        isUp = false
      }
      prevPt = point
    }

    if(results.isEmpty && list.head == list.max) {
      results += 0
    }

    results.toList
  }

  def findLocalMinima(list:List[Double]):List[Int] = {

    if(list.length < 2) {
      return List[Int]()
    }

    var results = new mutable.ArrayBuffer[Int]()
    var prevPt = list.head

    var isDown = false

    for(i <- list.indices) {

      val point = list(i)

      if(prevPt > point) {
        if(i == list.length - 1) {
          results += i
        }
        isDown = true
      } else if(prevPt < point) {
        if(isDown) {
          results += i - 1
        }
        isDown = false
      }
      prevPt = point
    }

    if(results.isEmpty && list.head == list.min) {
      results += 0
    }

    results.toList
  }

  def buildTrendData(prices:List[Double]):List[Trend] = {

    val max = findLocalMaxima(prices)
    val min = findLocalMinima(prices)

    val inflectionPts = (List(0) ++ max ++ min ++ List(prices.length - 1)).sortBy(i => i)

    val trends = new mutable.ArrayBuffer[Trend]()

    var prevPoint = -1

    for(point <- inflectionPts) {
      if(prevPoint != -1) {

        val price = prices(point)
        val prevPrice = prices(prevPoint)

        val delta = price - prevPrice
        val pctDelta = delta/prevPrice
        val trendDuration = point - prevPoint

        trends.append(new Trend(Some(prevPoint), Some(point), Some(pctDelta), Some(trendDuration)))
      }
      prevPoint = point
    }

    trends.toList
  }

  def findEndOfTrendChanges(prices:List[Double], trends:List[Trend]):List[Trend] = {

    val results = new mutable.ArrayBuffer[Trend]()

    var j = 0
    prices.indices.foreach(i => {

      val trend = trends(j)

      var trendStart = trend.startIdx.get
      var trendEnd = trend.endIdx.get

      while((i < trendStart || i >= trendEnd) && j < trends.length - 1) {
        j = j + 1
        trendStart = trends(j).startIdx.get
        trendEnd = trends(j).endIdx.get
      }

      if(j < trends.length - 1) {
        val price = prices(i)

        val changeMarker = new Trend(None, None, Some((prices(trends(j).endIdx.get) - price) / price), Some(trendEnd - i))

        results.append(changeMarker)
      }
    })

    results.toList
  }

}
*/
