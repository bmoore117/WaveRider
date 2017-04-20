package com.leetcode.waverider.indicators

/**
  * Created by Ben on 4/20/2017.
  */
trait Writable {
  def getFeatureHeaderList: List[String]
  def toFeatureList: List[String]
}
