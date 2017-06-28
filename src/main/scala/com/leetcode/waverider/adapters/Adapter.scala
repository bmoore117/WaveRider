package com.leetcode.waverider.adapters

import com.leetcode.waverider.data.RawMarketDay

/**
  * Created by Ben on 4/22/2017.
  */
trait Adapter {

  def init(params: Seq[String])

  def next(): Option[RawMarketDay]

  def reset(): Unit

}
