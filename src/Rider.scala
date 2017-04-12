import scala.collection.mutable

/**
  * Created by Ben on 4/8/2017.
  */
object Rider {

  object TrendType extends Enumeration {
    type TrendType = Value
    val UP, DOWN, FLAT = Value
  }

  var cash = 100.0
  var holdings = 0

  var macroTrend:TrendType.TrendType = TrendType.FLAT
  var microTrend:TrendType.TrendType = TrendType.FLAT
  var isCross:Boolean = false

  val tradeCost = 1
  val trendMinStrength = 1

  val priceBoundLine = new mutable.Queue[Double]()
  var lastHigh:Double = 0.0
  var lastLow:Double = 0.0


  var scaleFactor:Int = _

  /**
    * Simulates the market using a sine wave
    * Offset by one to keep price >= 0
    * @param iteration
    * @return
    */
  def getNextTick(iteration: Int): Double = {

    if(iteration > 10 && iteration < 18) {
      scaleFactor += 1
    }

    if(iteration > 28 && iteration < 36) {
      scaleFactor -= 1
    }

    (math.sin(iteration) + 1) + scaleFactor
  }

  /**
    * Buy bets the whole cash element, to the nearest purchase-able unit
    * @param price the current price
    */
  def buy(price: Double): Unit = {
    val units = (cash / price).toInt

    println("Buying " + units + " units @ " + price + " per")

    holdings += units
    cash -= units * price

    println("Cash: " + cash)
  }

  /**
    * Sells the entire stake
    * @param price the current price
    */
  def sell(price: Double): Unit = {
    println("Selling " + holdings + " units @ " + price + " per")

    cash += holdings*price
    holdings = 0

    println("Cash: " + cash)
  }

  def main(args: Array[String]): Unit = {

    var trendStrength = 0.0
    var lastTick = 0.0
    var lastStatus = ""

    var downtrendReset = false
    var upTrendReset = false

    for(i <- 1 to 40) {
      isCross = false
      val tick = getNextTick(i)
      println("Iteration: " + i)
      println("Tick price: " + tick)
      println("Last high: " + lastHigh)
      println("Last low: " + lastLow)

      updateTrends(tick)

      if(tick > lastTick) {

        //if we're crossing from a downtrend, but are not sure this constitutes a solid uptrend,
        //go ahead and reset the trend strength, and handle flags so we only reset trend strength once
        if(microTrend == TrendType.DOWN && !upTrendReset) {
          println("Cross detected")
          trendStrength = 0
          downtrendReset = false
          upTrendReset = true
          isCross = true
        }

        //update and display
        trendStrength = trendStrength + (tick - lastTick)
        println("Trend Strength: " + trendStrength)

        //The market was going down, but now since trendStrength is greater than the noise threshold, we have confirmed
        //it to be on an upswing. Sell high
        if(microTrend == TrendType.DOWN && trendStrength >= trendMinStrength && (macroTrend == TrendType.FLAT || macroTrend == TrendType.DOWN)) {
          sell(tick)
        }

        if(macroTrend == TrendType.UP) {
          buy(tick)
        }

        //mark that the market is now high
        if(trendStrength > trendMinStrength) {
          microTrend = TrendType.UP
          println("microTrend: " + microTrend)
        }
      } else if (tick < lastTick) {

        //if we're crossing from an uptrend, but are not sure this constitutes a solid downtrend,
        //go ahead and reset the trend strength, and handle flags so we only reset trend strength once
        if(microTrend == TrendType.UP && !downtrendReset) {
          println("Cross detected")
          trendStrength = 0
          downtrendReset = true
          upTrendReset = false
          isCross = true
        }

        //update and display
        trendStrength = trendStrength + math.abs(tick - lastTick)
        println("Trend Strength: " + trendStrength)

        //The market was going up, but now since trendStrength is greater than the noise threshold, we have confirmed
        //it to be on a downturn. Buy low
        if(microTrend == TrendType.UP && trendStrength >= trendMinStrength && (macroTrend == TrendType.FLAT || macroTrend == TrendType.UP)) {
          buy(tick)
        }

        if(macroTrend == TrendType.DOWN) {
          sell(tick);
        }

        if(trendStrength > trendMinStrength) {
          microTrend = TrendType.DOWN
          println("microTrend: " + microTrend)
        }
      }
      lastTick = tick

      if(tick > lastHigh) {
        lastHigh = tick
      } else if(tick < lastLow) {
        lastLow = tick
      }

      println("")
    }

    println("Final Cash: " + cash)
    println("Final Holdings: " + holdings)
    println("Last price: " + lastTick)
  }

  /**
    * Called to update the bounds for high or low price. Automatically sets the
    * macroTrend if it detects that we have crossed out
    * @param price
    */
  def updateTrends(price: Double): Unit = {

    if (priceBoundLine.length > 6) {

      val priceSum = priceBoundLine.foldLeft(0.0)((total, price) => price + total)
      val meanPrice = priceSum / priceBoundLine.length
      val stdDev = math.sqrt(priceBoundLine.map(_ - meanPrice).map(t => t * t).sum / priceBoundLine.length)
      println("std dev: " + stdDev)

      // check to see whether we have broken out of oscillation. We have done so if the current price is outside the
      // last high or low +- the standard dev for the last n prices
      if(price > lastHigh + stdDev || price < lastLow - stdDev) {//if (math.abs(meanPrice - price) > stdDev * 9) {

        if (microTrend == TrendType.UP) {
          macroTrend = TrendType.UP
        }
        else if(microTrend == TrendType.DOWN) {
          macroTrend = TrendType.DOWN
        }

        lastHigh = price
        lastLow = price

        priceBoundLine.clear()
      } else {
        macroTrend = TrendType.FLAT //TODO should this require several prices in bounds?
      }
    }
    println("macroTrend: " + macroTrend)

    priceBoundLine.enqueue(price)
  }

  def decideTrade(): Unit = {
    if(isCross) {

    }
  }
}
