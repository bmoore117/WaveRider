package com.leetcode.waverider.adapters

/**
  * Created by Ben on 5/27/2017.
  */
trait DataSource {

  def getNextSample:Option[Map[String, Array[Double]]]

}
