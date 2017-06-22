package com.leetcode.waverider

import com.leetcode.waverider.adapters.impl.YahooFileAdapter
import com.leetcode.waverider.engines.{IndicatorEngine, MLEngine}

/**
  * Created by Ben on 4/22/2017.
  */
object Main {

  def main(args: Array[String]): Unit = {
    if (args.length == 1) {

      val adapter = new YahooFileAdapter()
      adapter.init(args)

      val engine = new IndicatorEngine(adapter, Some(10))

      var day = adapter.next()

      while (day.isDefined) {
        engine.analyzeNext(day.get, IndicatorEngine.supportedFeatures)
        day = adapter.next()
      }

      engine.writeAnalysis()

    } else if (args.length == 2) {
      val engine = new MLEngine(args.head, args.last, IndicatorEngine.supportedFeatures.)

      engine.train()
    }

    else {
      println("Supply single market .csv file, such as from Yahoo finance, and a mode: analyze or label")
    }
  }
}
