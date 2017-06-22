package com.leetcode.waverider.data

/**
  * Created by Ben on 6/19/2017.
  */
class Label(val h: List[String], val f: List[String]) extends Writable {
  override def headers: List[String] = h

  override def features: List[String] = f
}
