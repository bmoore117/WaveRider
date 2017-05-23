package com.leetcode.waverider.utils

/**
  * Created by Ben on 5/21/2017.
  */
object MathUtils {

  def sampleCorrelation(x: Seq[Double], y: Seq[Double]):Double = {

    val xMean = x.sum / x.length
    val yMean = y.sum / y.length

    var crossSum = 0.0

    for(i <- x.indices) {
      crossSum = crossSum + (x(i) - xMean)*(y(i) - yMean)
    }

    var xSum = 0.0

    for(i <- x.indices) {
      xSum = xSum + math.pow(x(i) - xMean, 2)
    }

    xSum = math.sqrt(xSum)

    var ySum = 0.0

    for(i <- y.indices) {
      ySum = ySum + math.pow(y(i) - yMean, 2)
    }

    ySum = math.sqrt(ySum)

    val test = crossSum / (xSum*ySum)

    if(test.isNaN) {
      0
    } else {
      test
    }
  }
}
