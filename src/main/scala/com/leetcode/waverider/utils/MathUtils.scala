package com.leetcode.waverider.utils

import scala.collection.mutable

/**
  * Created by Ben on 4/27/2017.
  */
object MathUtils {

  def findLocalMaxima(list:List[Double]):List[Int] = {

    if(list.length < 2) {
      return List[Int]()
    }

    var results = new mutable.ArrayBuffer[Int]()
    var prevPt = list.head

    var isUp = false

    for(i <- list.indices) {

      val point = list(i)

      if(prevPt < point) {
        if(i == list.length - 1) {
          results += i
        }
        isUp = true
      } else if(prevPt > point) {
        if(isUp) {
          results += i - 1
        }
        isUp = false
      }
      prevPt = point
    }

    if(results.isEmpty && list.head == list.max) {
      results += 0
    }

    results.toList
  }

  def findLocalMinima(list:List[Double]):List[Int] = {

    if(list.length < 2) {
      return List[Int]()
    }

    var results = new mutable.ArrayBuffer[Int]()
    var prevPt = list.head

    var isDown = false

    for(i <- list.indices) {

      val point = list(i)

      if(prevPt > point) {
        if(i == list.length - 1) {
          results += i
        }
        isDown = true
      } else if(prevPt < point) {
        if(isDown) {
          results += i - 1
        }
        isDown = false
      }
      prevPt = point
    }

    if(results.isEmpty && list.head == list.min) {
      results += 0
    }

    results.toList
  }

}
