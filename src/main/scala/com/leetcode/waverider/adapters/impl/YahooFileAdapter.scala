package com.leetcode.waverider.adapters.impl

import java.io.File
import java.text.SimpleDateFormat

import com.github.tototoshi.csv.CSVReader
import com.leetcode.waverider.adapters.Adapter
import com.leetcode.waverider.data.{AnalyzedMarketDay, RawMarketDay}

import scala.collection.mutable.ArrayBuffer

/**
  * Created by Ben on 4/22/2017.
  */
class YahooFileAdapter extends Adapter {

  var reader:CSVReader = _
  var market:ArrayBuffer[RawMarketDay] = new ArrayBuffer[RawMarketDay]()

  var currentIdx = 0

  override def init(params: Seq[String]): Unit = {
    reader = CSVReader.open(new File(params.head))

    reader.iterator.next()

    reader.iterator.foreach(day => {
      val mktDay = initTypes(day)
      market.append(mktDay)
    })

    market = market.sortBy(_.date)
    reader.close()
  }

  override def next(): Option[RawMarketDay] = {
    if(currentIdx < market.length - 1) {
      val retVal = market(currentIdx)
      currentIdx += 1
      return Some(retVal)
    }
    None
  }

  override def handleOutput(analyzedDay: AnalyzedMarketDay): Unit = {}

  def initTypes(day: Seq[String]): RawMarketDay = {
    val format = new SimpleDateFormat("yyyy-MM-DD")
    new RawMarketDay(format.parse(day.head), day(1).toDouble, day(2).toDouble, day(3).toDouble, day(4).toDouble, day(5).toInt, day(6).toDouble)
  }

  override def dispose(): Unit = {}
}
