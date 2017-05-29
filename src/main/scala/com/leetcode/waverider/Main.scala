package com.leetcode.waverider

import com.leetcode.waverider.agent.SimulationTradeAgent
import com.leetcode.waverider.engines.QEngine
import com.leetcode.waverider.services.{DBService, StateService}

/**
  * Created by Ben on 4/22/2017.
  */
object Main {

  def main(args: Array[String]): Unit = {

    val dBService = new DBService
    val stateService = new StateService
    val qEngine = new QEngine(0.2, 0.5, 0.05)
    val tradeAgent = new SimulationTradeAgent

    var sample = dBService.getNextSample

    while(sample.isDefined) {
      val state = stateService.sampleToCluster(sample.get)
      qEngine.updateTransitionMatrix(state)

      val action = qEngine.getBestAction(state, tradeAgent.getValidActions())
      val reward = tradeAgent.takeAction(action)

      qEngine.updateQMatrix(state, action, reward, tradeAgent.getValidActions())

      sample = dBService.getNextSample
    }
  }
}
