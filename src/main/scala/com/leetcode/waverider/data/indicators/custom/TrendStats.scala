package com.leetcode.waverider.data.indicators.custom

import com.leetcode.waverider.data.indicators.IndicatorBuilder
import com.leetcode.waverider.data.{AnalyzedMarketDay, RawMarketDay, Trend, Writable}
import com.leetcode.waverider.utils.LastNQueue
import com.tictactec.ta.lib.Core

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * Created by Ben on 6/19/2017.
  */
class TrendStats extends Trend(None, None, None, None) {
  override def headers: List[String] = List("SIZE%", "DURATION%", "DIRECTION", "ACCELERATION")

  override def features: List[String] = List(sizePercentile.getOrElse("").toString,
    durationPercentile.getOrElse("").toString, direction.getOrElse("").toString, acceleration.getOrElse("").toString)

  var sizePercentile:Option[Double] = None
  var durationPercentile:Option[Double] = None
  var direction:Option[Int] = None
  var acceleration:Option[Int] = None
}

case class TrendStatsBuilder() extends IndicatorBuilder {

  private val ONE = 1
  private val MINUS_ONE = -1

  override def instantiateIndicator(core: Core, rawDays: ListBuffer[RawMarketDay],
                                    last100Trends:LastNQueue[Trend], current: Trend): Writable = {
    val stats = new TrendStats()

    val trends = last100Trends.toList

    if(trends.isEmpty || current.duration.get == 0 || rawDays.length < 4) {
      return stats
    }

    val sizeDistribution = new mutable.HashMap[Double, Int]()
    val durationDistribution = new mutable.HashMap[Int, Int]()

    //build distributions
    trends.foreach(trend => {
      if(!sizeDistribution.contains(trend.pctDelta.get.abs)) {
        sizeDistribution.put(trend.pctDelta.get.abs, 1)
      } else {
        sizeDistribution.update(trend.pctDelta.get.abs, sizeDistribution(trend.pctDelta.get.abs) + 1)
      }

      if(!durationDistribution.contains(trend.duration.get)) {
        durationDistribution.put(trend.duration.get, 1)
      } else {
        durationDistribution.update(trend.duration.get, durationDistribution(trend.duration.get) + 1)
      }
    })

    //find size percentile
    val sizeKeys = sizeDistribution.keySet.toList.sorted
    var sum = 0
    var i = 0
    while (i < sizeKeys.length && current.pctDelta.get.abs >= sizeKeys(i))  {
      sum += sizeDistribution(sizeKeys(i))
      i += 1
    }

    val sizePercentile = sum.toDouble / sizeDistribution.values.sum.toDouble

    //find duration percentile
    val durationKeys = durationDistribution.keySet.toList.sorted
    sum = 0
    i = 0
    while (i < durationKeys.length && current.duration.get >= durationKeys(i)) {
      sum += durationDistribution(durationKeys(i))
      i += 1
    }
    val durationPercentile = sum.toDouble / durationDistribution.values.sum.toDouble

    //find basic trend direction
    val direction = if(math.signum(current.pctDelta.get) > 0) {
      ONE
    } else {
      MINUS_ONE
    }

    //ensure we have enough for speed & accel formulas
    var startIdx = current.duration.get
    if(current.duration.get < 4) {
      while(current.endIdx.get - startIdx < 4) {
        startIdx -= 1
      }
    }

    //find trend acceleration
    val days = rawDays.slice(startIdx, current.endIdx.get).map(day => day.close)

    val speed = days.sliding(2).map(pair => pair.last - pair.head).toList
    val acceleration = speed.sliding(2).map(pair => pair.last - pair.head).toList
    val force = acceleration.sliding(2).map(pair => pair.last - pair.head).toList

    val accelerationType = if(force.last > force.head) {
      ONE
    } else {
      MINUS_ONE
    }

    stats.sizePercentile = Some(sizePercentile)
    stats.durationPercentile = Some(durationPercentile)
    stats.direction = Some(direction)
    stats.acceleration = Some(accelerationType)

    stats
  }
}