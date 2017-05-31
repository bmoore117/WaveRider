package com.leetcode.waverider.agent

import scala.collection.mutable

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
class SimulationTradeAgent(var cash:Double) {

  //association is price, amount purchased
  private val portfolio = new mutable.HashMap[Double, Double]()

  //negative actions sell, positive buy
  private val actions = -9 to 9

  //TODO can this be simplified?
  def takeAction(action:Int, price:Double, simulation:Boolean): Double = {

    if(action == 0) {
      0
    } else if(action > 0) {
      val spendAmount = cash * action/10
      if(!simulation) {
        cash = cash - spendAmount
      }
      val purchaseAmount = spendAmount / price
      val tranche = portfolio.getOrElseUpdate(price, 0)
      portfolio.update(price, tranche + purchaseAmount)
      0
    } else {
      val unitsToSell = portfolio.values.sum * math.abs(action/10.0)
      var totalSold = 0.0
      var reward = 0.0

      while(totalSold != unitsToSell && portfolio.nonEmpty) {
        val lowestTranchePrice = portfolio.keySet.min
        var lowestTrancheUnits = portfolio(lowestTranchePrice)

        val priceDiff = price - lowestTranchePrice

        if(lowestTrancheUnits > unitsToSell) {
          lowestTrancheUnits = lowestTrancheUnits - unitsToSell
          totalSold = unitsToSell
          portfolio.update(lowestTranchePrice, lowestTrancheUnits)
        } else if(lowestTrancheUnits == unitsToSell) {
          lowestTrancheUnits = lowestTrancheUnits - unitsToSell
          totalSold = unitsToSell
          portfolio.remove(lowestTranchePrice)
        } else { //lowestTrancheUnits < unitsToSell
          totalSold = totalSold + lowestTrancheUnits
          portfolio.remove(lowestTranchePrice)
        }

        reward += priceDiff * totalSold
      }

      if(!simulation) {
        cash = cash + price*unitsToSell
      }

     reward
    }
  }

  def reset(): Unit = {
    cash = 100
    portfolio.clear()
  }

  def getValidActions(price:Double): Seq[Int] = {

    val validActions = actions

    if(portfolio.isEmpty) {
      validActions.filter(action => action < 0)
    }

    if(cash == 0) {
      validActions.filter(action => action > 0)
    }

    validActions //TODO implement trading fee restrictions
  }

  def getPortfolioValue(price:Double):Double = {
    portfolio.values.sum*price + cash
  }
}
