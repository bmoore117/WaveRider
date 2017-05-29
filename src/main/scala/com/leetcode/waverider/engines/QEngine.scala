package com.leetcode.waverider.engines

import scala.collection.mutable

/**
  * Created by Ben on 5/27/2017.
  */
class QEngine(val learningRate:Double, val discountRate:Double, val randomActionProbability:Double) {

  private val transitionDistribution = new mutable.HashMap[Int, mutable.HashMap[Int, Int]]

  private val qMatrix = new mutable.HashMap[Int, mutable.HashMap[Int, Double]]

  private val rng = scala.util.Random

  private val range = 1 to 100
  private val winningNumbers = Set(47, 99, 89, 26, 33)

  private var lastState:Option[Int] = None

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

  def getMostLikelyNextState(state:Int): Option[Int] = {
    val distribution = transitionDistribution.get(state)

    if(distribution.isDefined) {
      var bestPair = (0, Int.MinValue)

      distribution.get.foreach(pair => {
        if(pair._2 > bestPair._2) {
          bestPair = pair
        }
      })

      return Some(bestPair._1)
    }

    None
  }

  def getBestAction(state:Int, validActions:Seq[Int]): Int = {

    if (!shouldTakeRandomAction()) {

      val actions = qMatrix.get(state)

      if (actions.isDefined) {

        val remainingActions = actions.get.filter(pair => !validActions.contains(pair._1))

        if(remainingActions.nonEmpty) {
          var bestPair = (0, Double.MinValue)

          remainingActions.foreach(pair => {
            if (pair._2 > bestPair._2) {
              bestPair = pair
            }
          })

          return bestPair._1
        }

      }
    }
    validActions(rng.nextInt(validActions.length))
  }

  def shouldTakeRandomAction():Boolean = {
    val drawing = rng.nextInt(100) + 1

    winningNumbers.contains(drawing)
  }


  def getNextStateBestQ(state:Int, validActions: Seq[Int]): Double = {
    val nextState = getMostLikelyNextState(state)

    val q = if(nextState.isDefined) {
      val nextStateBestAction = getBestAction(nextState.get, validActions)
      qMatrix(nextState.get)(nextStateBestAction)
    } else {
      0
    }

    q
  }

  def updateQMatrix(state:Int, action:Int, reward:Double, validActions: Seq[Int]): Unit = {
    val stateActionValues = qMatrix.get(state)

    if(stateActionValues.isDefined) {
      val actionValue = stateActionValues.get.get(action)

      if(actionValue.isDefined) {
        stateActionValues.get.put(action, actionValue.get + learningRate*(reward + discountRate*getNextStateBestQ(state, validActions) - actionValue.get))
      } else {
        stateActionValues.get.put(action, learningRate*(reward + discountRate*getNextStateBestQ(state, validActions)))
      }
    } else {
      val actionValues = new mutable.HashMap[Int, Double]()
      actionValues.put(action, learningRate*(reward + discountRate*getNextStateBestQ(state, validActions))) //where 1 is currently placeholder for reward
      qMatrix.put(state, actionValues)
    }
  }
}
