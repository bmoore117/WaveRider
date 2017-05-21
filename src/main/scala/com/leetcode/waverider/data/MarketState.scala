package com.leetcode.waverider.data

import org.deeplearning4j.rl4j.space.Encodable

/**
  * Created by Ben on 5/20/2017.
  */
class MarketState(val state:Int, val sampleId:BigInt) extends Encodable {

  override def toArray: Array[Double] = { Array[Double](state.toDouble) }

}
