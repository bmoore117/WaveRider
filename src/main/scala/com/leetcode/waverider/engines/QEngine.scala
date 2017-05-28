package com.leetcode.waverider.engines

import scala.collection.mutable

/**
  * Created by Ben on 5/27/2017.
  */
class QEngine(val learningRate:Double, val discountRate:Double, val randomActionProbability) {

  val transitionDistribution = new mutable.HashMap[Int, mutable.HashMap[Int, Int]]

  val qMatrix = new mutable.HashMap[Int, mutable.HashMap[Int, Double]]

  var lastState:Option[Int] = None

  def updateTransitionMatrix(state:Int): Unit = {
    if(lastState.isDefined) {

      //get distribution for last state
      val distribution = transitionDistribution.get(lastState.get)

      //if defined, see if distribution contains a transition for current state
      //and if that defined, add 1 to number of times we've seen it occur
      //else add the new transition, with a count of 1
      //if distribution itself not defined, create it, and add the transition we've seen with a count of 1
      if(distribution.isDefined) {
        val transition = distribution.get.get(state)

        if(transition.isDefined) {
          distribution.get.put(state, transition.get + 1)
        } else {
          distribution.get.put(state, 1)
        }

      } else {
        val dist = new mutable.HashMap[Int, Int]()
        dist.put(state, 1)
        transitionDistribution.put(lastState.get, dist)
      }
    }

    lastState = Some(state)
  }

  def getMostLikelyNextState(state:Int): Int = {
    val distribution = transitionDistribution(state)

    var bestPair = (0, Int.MinValue)

    distribution.foreach(pair => {
      if(pair._2 > bestPair._2) {
        bestPair = pair
      }
    })

    bestPair._1
  }

  def getBestAction(state:Int): Int = {
    val actions = qMatrix(state)

    var bestPair = (0, Double.MinValue)

    actions.foreach(pair => {
      if(pair._2 > bestPair._2) {
        bestPair = pair
      }
    })

    bestPair._1
  }

  def getStateActionQ(state:Int, action:Int): Double = {
    val stateActionValues = qMatrix.get(state)

    if(stateActionValues.isDefined) {
      val q = stateActionValues.get.get(action)

      if(q.isDefined) {
        return q.get
      }
    }

    0
  }

  def updateQMatrix(state:Int, action:Int): Unit = {
    val stateActionValues = qMatrix.get(state)

    val nextState = getMostLikelyNextState(state)
    val nextStateBestAction = getBestAction(nextState)

    if(stateActionValues.isDefined) {
      val actionValue = stateActionValues.get.get(action)

      if(actionValue.isDefined) {

      } else {
        stateActionValues.get.put(action, learningRate*(1 + discountRate*getStateActionQ(nextState, nextStateBestAction)))
      }

    } else {
      val actionValues = new mutable.HashMap[Int, Double]()
      actionValues.put(action, learningRate*(1 + discountRate*getStateActionQ(nextState, nextStateBestAction))) //where 1 is currently placeholder for reward
      qMatrix.put(state, actionValues)
    }
  }
}
