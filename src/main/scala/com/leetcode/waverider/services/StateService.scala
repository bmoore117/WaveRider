package com.leetcode.waverider.services

import com.leetcode.waverider.utils.MathUtils

import scala.collection.mutable

/**
  * Created by Ben on 5/21/2017.
  * This class will take in a sample from the database and do the correlations on it to reduce it down to a single integer
  * It will be used to supply data for the MarketStates
  */
class StateService {

  val FEATURE_COUNT = 10
  object Features extends Enumeration {
    type Feature = Value
    val High, Low, Volume, Last, BaseVolume, Bid, Ask, OpenBuyOrders, OpenSellOrders, PrevDay = Value
  }

  /*/*
    * Precompute the possible clusterings, by running through the subsets of the different features
    * @return A sequence of sets of features, where each set is a indexed cluster in the main sequence
    */
  val clusterCombinations:Seq[Set[Features.Feature]] = {
    val set = Set(Features.High, Features.Low, Features.Volume, Features.Last, Features.BaseVolume, Features.Bid, Features.Ask, Features.OpenBuyOrders, Features.OpenSellOrders, Features.PrevDay)

    val builder = new ArrayBuffer[Set[Features.Feature]]()

    set.subsets().foreach(subset => {
      builder.append(subset)
    })

    builder.toList
  }*/

  val observedStates = new mutable.ListBuffer[String]()

  /**
    * This method takes in a set of observations over time for a given feature set, finds the correlations between them
    * and takes the sign (-1, 0, 1) of each correlation, building a string of such signs to represent the state. It then
    * assigns an index to all such unique strings, and returns that index as a final representation of state
    *
    * @param sample An object returned from a DataSource implementation.
    * @return An integer indicating which unique
    */
  def sampleToCluster(sample:Map[String, Array[Double]]): Int = {

    val keyList = sample.keySet.toIndexedSeq
    val builder = new StringBuilder

    for(i <- keyList.indices) {
      val firstName = keyList(i)
      val first = sample(firstName)

      for(j <- i + 1 until keyList.length) {
        val secondName = keyList(j)
        val second = sample(secondName)

        var corr = MathUtils.sampleCorrelation(first, second)
        if(corr < 0.05) { //throw away weak correlations
          corr = 0
        }
        builder.append(math.signum(corr))
      }
    }

    val state = builder.mkString

    if(!observedStates.contains(state)) {
      observedStates.append(state)
    }

    observedStates.indexOf(state)
  }
}
