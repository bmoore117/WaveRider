package com.leetcode.waverider.adapters.impl

import java.io.File
import java.text.SimpleDateFormat

import com.github.tototoshi.csv.CSVReader
import com.leetcode.waverider.adapters.Adapter
import com.leetcode.waverider.data.RawMarketDay

import scala.collection.mutable.ListBuffer

/**
  * Created by Ben on 4/22/2017.
  */
class YahooFileAdapter extends Adapter {

  val format = new SimpleDateFormat("yyyy-MM-DD")

  var reader:CSVReader = _
  var market:ListBuffer[RawMarketDay] = new ListBuffer[RawMarketDay]()

  var currentIdx = 0

  override def init(params: Seq[String]): Unit = {
    reader = CSVReader.open(new File(params.head))

    reader.iterator.next()

    reader.iterator.foreach(day => {
      val mktDay = initTypes(day)
      market.append(mktDay)
    })

    market = market.sortBy(day => day.date)
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

  def initTypes(day: Seq[String]): RawMarketDay = {
    new RawMarketDay(day.head, day(1).toDouble, day(2).toDouble, day(3).toDouble, day(4).toDouble, day(5).toInt, day(6).toDouble)
  }

  override def reset(): Unit = {
    currentIdx = 0
  }
}
