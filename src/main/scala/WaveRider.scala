import java.io.File
import java.text.SimpleDateFormat

import com.github.tototoshi.csv._
import com.tictactec.ta._
import com.tictactec.ta.lib.{Core, MAType, MInteger, RetCode}

import scala.collection.mutable.ArrayBuffer

/**
  * Created by Benjamin on 4/15/2017.
  */
object WaveRider {

  val marketActivity = new ArrayBuffer[MarketDay]()
  val core = new Core

  def main(args: Array[String]): Unit = {
    if(args.length == 1) {
      val marketFile = new File(args(0))

      if (marketFile.exists) {
        val market = CSVReader.open(marketFile)

        market.iterator.next()

        market.iterator.foreach(day => {
          val mktDay = initTypes(day)
          marketActivity.append(mktDay)

          println("Date: " + day(0))

          val band = bollinger()

          band.foreach(band => println(band.toString))

        })
      }
    } else {
      println("Supply single market .csv file, such as from Yahoo finance")
    }
  }


  def initTypes(day: Seq[String]): MarketDay = {

    val format = new SimpleDateFormat("yyyy-MM-DD")
    val mktDay = new MarketDay

    mktDay.date = format.parse(day(0))
    mktDay.open = day(1).toDouble
    mktDay.high = day(2).toDouble
    mktDay.low = day(3).toDouble
    mktDay.close = day(4).toDouble
    mktDay.adjustedClose = day(5).toDouble

    mktDay
  }

  def bollinger(): Option[BBand] = {
    val TIME_PERIOD = 21
    val DISTANCE_DEVIATIONS = 2

    if(marketActivity.length >= TIME_PERIOD) {

      val prices = marketActivity.slice(marketActivity.length - TIME_PERIOD, marketActivity.length).map(day => day.close).toArray

      val upperBand:Array[Double] = new Array[Double](1)
      val avg:Array[Double] = new Array[Double](1)
      val lowerBand:Array[Double] = new Array[Double](1)

      val begin = new MInteger
      val nbElement = new MInteger

      //RetCode re = c.bbands(startIdx, endIdx, inReal, optInTimePeriod, optInNbDevUp, optInNbDevDn, optInMAType, outBegIdx, outNBElement, outRealUpperBand, outRealMiddleBand, outRealLowerBand)
      val retCode = core.bbands(0, TIME_PERIOD - 1, prices, TIME_PERIOD, DISTANCE_DEVIATIONS, DISTANCE_DEVIATIONS, MAType.Sma, begin, nbElement, upperBand, avg, lowerBand)

      if(retCode == RetCode.Success) {
        val band = new BBand
        band.upperBand = upperBand(0)
        band.avg = avg(0)
        band.lowerBand = lowerBand(0)
        band.bandDistance = DISTANCE_DEVIATIONS

        return Some(band)
      } else {
        return None
      }
    }
    return None
  }
}
