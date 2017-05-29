package com.leetcode.waverider.agent

/**
  * Rough idea is to store price tranches here, and get a q-value for each of them. As well as do the buying and selling.
  *
  * Actions: sell or buy percentages of portfolio by 10s
  * Will sell in most favorable order, e.g. will have a map of # shares bought at x price. Is there a simpler way?
  *
  * Simulation trade agent does not interact with the full order book, so sales aren't going to be completely real
  * We just want to see if the core Q learning will work
  * So the last price observed from the DB needs to be passed in
  *
  * Created by Ben on 5/28/2017.
  */
class SimulationTradeAgent {

  def takeAction(action:Int, price:Double): Double = {

    0
  }

  def getValidActions(): Seq[Int] = {

    Nil
  }
}
