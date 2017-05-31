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
    val tradeAgent = new SimulationTradeAgent(100)

    var sample = dBService.getNextSample

    val initializationPeriod = 50
    var i = 0
    while(sample.get("last").length > 0) {

      if(i == 50) {
        tradeAgent.reset()
      }

      val state = stateService.sampleToCluster(sample.get)
      qEngine.updateTransitionMatrix(state)

      val price = dBService.getPriceForCurrentSample

      val action = qEngine.getBestAction(state, tradeAgent.getValidActions(0))
      val reward = tradeAgent.takeAction(action, price, i < 50)

      qEngine.updateQMatrix(state, action, reward, tradeAgent.getValidActions(0))

      sample = dBService.getNextSample
      println("Iteration: " + i + " state: " + state + " action: " + action + " reward: " + reward + " final portfolio value: " + tradeAgent.getPortfolioValue(price))
      i = i + 1
    }

    println(qEngine.qMatrixHits)
  }
}
