package com.leetcode.waverider

import com.leetcode.waverider.services.{DBService, StateService}

/**
  * Created by Ben on 4/22/2017.
  */
object Main {

  def main(args: Array[String]): Unit = {

    val dBService = new DBService
    val stateService = new StateService

    var sample = dBService.getNextSample
    while(sample.isDefined) {
      println(stateService.sampleToCluster(sample.get))
      sample = dBService.getNextSample
    }
  }
}
