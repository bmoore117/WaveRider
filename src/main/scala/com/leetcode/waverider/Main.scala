package com.leetcode.waverider

import com.leetcode.waverider.adapters.impl.YahooFileAdapter
import com.leetcode.waverider.data.indicators.IndicatorBuilder
import com.leetcode.waverider.engines.{IndicatorEngine, MLEngine}

/**
  * Created by Ben on 4/22/2017.
  */
object Main {

  def main(args: Array[String]): Unit = {
    if (args.length == 1) {

      val adapter = new YahooFileAdapter()
      adapter.init(args)

      val featureEngine = new IndicatorEngine(adapter, Some(10))

      var day = adapter.next()

      var bestSubset:Set[IndicatorBuilder] = null
      var highestScore = Double.MinValue
      IndicatorEngine.supportedFeatures.subsets().foreach(set => {
        if(set.nonEmpty) {
          val castSet = set.asInstanceOf[Set[IndicatorBuilder]]
          while (day.isDefined) {
            featureEngine.analyzeNext(day.get, castSet)
            day = adapter.next()
          }

          featureEngine.writeAnalysis()

          val mlEngine = new MLEngine("train.csv", "test.csv", set.size)

          val score = mlEngine.train()
          println("Features used :" + castSet.toString())
          if(score > highestScore) {
            bestSubset = castSet
            highestScore = score
          }
        }
      })

      println("Best score: " + highestScore)
      println("Features used :" + bestSubset.toString())

    } else if (args.length == 2) {
      //val engine = new MLEngine(args.head, args.last, IndicatorEngine.supportedFeatures.)

      //engine.train()
    }

    else {
      println("Supply single market .csv file, such as from Yahoo finance, and a mode: analyze or label")
    }
  }
}
