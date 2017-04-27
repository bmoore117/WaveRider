package com.leetcode.waverider.utils

import scala.collection.mutable

/**
  * Created by Ben on 4/27/2017.
  */
object MathUtils {

  def findLocalMaxima(list:List[Double]):List[Double] = {

    var results = new mutable.ArrayBuffer[Double]()
    var prevPt = list.head

    var isUp = false

    for(i <- list.indices) {

      val point = list(i)

      if(prevPt < point) {
        if(i == list.length - 1 && isUp) {
          results += i - 1
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

    results.toList
  }

}
