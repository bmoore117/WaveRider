package com.leetcode.waverider.utils

import com.leetcode.waverider.data.{ChangeMarker, Trend}

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

    val inflectionPts = (max ++ min).sortBy(i => i)

    val trends = new mutable.ArrayBuffer[Trend]()

    var prevPoint = -1

    for(point <- inflectionPts) {
      if(prevPoint != -1) {

        val price = prices(point)
        val prevPrice = prices(prevPoint)

        val delta = price - prevPrice
        val pctDelta = delta/prevPrice
        val trendDuration = point - prevPoint

        trends.append(new Trend(prevPoint, point, pctDelta, trendDuration))
      }
      prevPoint = point
    }

    trends.toList
  }

  def findEndOfTrendChanges(prices:List[Double], trends:List[Trend]):List[ChangeMarker] = {

    val results = new mutable.ArrayBuffer[ChangeMarker]()

    var j = 1
    prices.indices.foreach(i => {

      val trend = trends(i)

      var trendStart = trend.startIdx
      var trendEnd = trend.endIdx

      while((i < trendStart || i >= trendEnd) && j < trends.length - 1) {
        j = j + 1
        trendStart = trends(j).startIdx
        trendEnd = trends(j).endIdx
      }

      if(j < trends.length - 1) {
        val price = prices(i)

        val changeMarker = new ChangeMarker

        changeMarker.valueChangeFromHere = (prices(trends(j).endIdx) - price) / prices(trends(j).endIdx)
        changeMarker.trendDurationFromHere = trendEnd - i

        results.append(changeMarker)
      }
    })

    results.toList
  }

}
