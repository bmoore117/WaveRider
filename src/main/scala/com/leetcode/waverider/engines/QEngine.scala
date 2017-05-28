package com.leetcode.waverider.engines

import scala.collection.mutable

/**
  * Created by Ben on 5/27/2017.
  */
class QEngine {

  val transitionProbabilities = new mutable.HashMap[Int, Int]

  val qMatrix = new mutable.HashMap[Int, Seq[Int]]

  val randomActionProbability = 0.05

  def updateTransitionMatrix():Unit = {

  }

  def getNextAction(): Int = {

    0
  }

  def updateQMatrix():Unit = {

  }
}
