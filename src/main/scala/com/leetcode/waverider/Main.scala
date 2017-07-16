package com.leetcode.waverider

import java.io.File

import com.github.tototoshi.csv.{CSVReader, CSVWriter}
import com.leetcode.waverider.adapters.impl.YahooFileAdapter
import com.leetcode.waverider.data.indicators.IndicatorBuilder
import com.leetcode.waverider.engines.{IndicatorEngine, MLEngine, TestEngine}

import scala.collection.immutable.ListSet
import scala.collection.mutable.ListBuffer

/**
  * Created by Ben on 4/22/2017.
  */
object Main {

  def main(args: Array[String]): Unit = {
    if (args.length == 1) {

      val adapter = new YahooFileAdapter()
      adapter.init(args)

      val featureEngine = new IndicatorEngine(adapter, Some(4))

      var bestSubset:Set[IndicatorBuilder] = null
      var highestScore = Double.MinValue
      var i = 0
      IndicatorEngine.supportedFeatures.subsets().foreach(set => {
        if(set.nonEmpty) {
          val castSet = set.asInstanceOf[ListSet[IndicatorBuilder]]

          adapter.reset()
          var day = adapter.next()
          while (day.isDefined) {
            featureEngine.analyzeNext(day.get, castSet)
            day = adapter.next()
          }

          featureEngine.writeAnalysis()
          featureEngine.reset()

          val engine = new MLEngine("train.csv", "validate.csv")

          //val engine = new TestEngine("train.csv", "validate.csv")

          val score = engine.evaluate()
          println("Features used :" + castSet.toString())
          println("Set number " + i)
          if(score > highestScore) {
            bestSubset = castSet
            highestScore = score
          }
          i += 1
        }
      })

      //println("Best score: " + highestScore)
      //println("Features used :" + bestSubset.toString())

    } else if (args.length == 2) {
      val reader = CSVReader.open(new File(args.head))
      val buf = new ListBuffer[Seq[String]]
      reader.iterator.foreach(day => {
        buf.append(day)
      })
      reader.close()

      var result = buf.map(row => if(row.last.toDouble > 0) row.take(row.length - 1) :+ "1" else row.take(row.length - 1) :+ "0")

      val trainAmt = (result.length * 0.8).toInt
      val testAmt = result.length - trainAmt

      val trainData = result.take(trainAmt)
      result = result.drop(trainAmt)

      val testData = result.take(testAmt)
      result = result.drop(testAmt)

      val trainWriter = CSVWriter.open(new File("signals.csv"))
      trainData.foreach(row => trainWriter.writeRow(row))
      trainWriter.close()

      val testWriter = CSVWriter.open(new File("signalsTrain.csv"))
      testData.foreach(row => testWriter.writeRow(row))
      testWriter.close()
    }

    else {
      println("Supply single market .csv file, such as from Yahoo finance, and a mode: analyze or label")
    }
  }
}
