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

  val tradeCost = 1
  val trendMinStrength = 1

  val priceBoundLine = new mutable.Queue[Double]()

  /**
    * Simulates the market using a sine wave
    * Offset by one to keep price >= 0
    * @param iteration
    * @return
    */
  def getNextTick(iteration: Int): Double = {
    math.sin(iteration) + 1 //
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

    for(i <- 1 to 20) {
      val tick = getNextTick(i)
      println("Iteration: " + i)
      println("Tick price: " + tick)

      updateTrends(tick)

      if(tick > lastTick) {

        //if we're crossing from a downtrend, but are not sure this constitutes a solid uptrend,
        //go ahead and reset the trend strength, and handle flags so we only reset trend strength once
        if(microTrend == TrendType.DOWN && !upTrendReset) {
          println("Cross detected")
          trendStrength = 0
          downtrendReset = false
          upTrendReset = true
        }

        //update and display
        trendStrength = trendStrength + (tick - lastTick)
        println("Trend Strength: " + trendStrength)

        //logging purposes.
        if(trendStrength > trendMinStrength) {
          println("Uptrend confirmed")
        }

        //The market was going down, but now since trendStrength is greater than the noise threshold, we have confirmed
        //it to be on an upswing. Sell high
        if(microTrend == TrendType.DOWN && trendStrength >= trendMinStrength && (macroTrend == TrendType.FLAT || macroTrend == TrendType.DOWN)) {
          sell(tick)
        }

        //mark that the market is now high
        if(trendStrength > trendMinStrength) {
          microTrend == TrendType.UP
        }
      } else if (tick < lastTick) {

        //if we're crossing from an uptrend, but are not sure this constitutes a solid downtrend,
        //go ahead and reset the trend strength, and handle flags so we only reset trend strength once
        if(microTrend == TrendType.UP && !downtrendReset) {
          println("Cross detected")
          trendStrength = 0
          downtrendReset = true
          upTrendReset = false
        }

        //update and display
        trendStrength = trendStrength + math.abs(tick - lastTick)
        println("Trend Strength: " + trendStrength)

        //logging purposes
        if(trendStrength > trendMinStrength) {
          println("Downtrend confirmed")
        }

        //The market was going up, but now since trendStrength is greater than the noise threshold, we have confirmed
        //it to be on a downturn. Buy low
        if(microTrend == TrendType.UP && trendStrength >= trendMinStrength && (macroTrend == TrendType.FLAT || macroTrend == TrendType.UP)) {
          buy(tick)
        }

        if(trendStrength > trendMinStrength) {
          microTrend == TrendType.DOWN
        }
      }
      lastTick = tick
      println("")
    }

    println("Final Cash: " + cash)
    println("Final Holdings: " + holdings)
    println("Last price: " + lastTick)
  }

  /**
    * Called when we have detected a cross, to update the bounds for high or low price. Automatically sets the
    * macroTrend if it detects that we have crossed out
    * @param price
    */
  def updateTrends(price: Double): Unit = {

    if (priceBoundLine.nonEmpty) {

      val priceSum = priceBoundLine.foldLeft(0.0)((total, price) => price + total)
      val meanPrice = priceSum / priceBoundLine.length
      val stdDev = math.sqrt(priceBoundLine.map(_ - meanPrice).map(t => t * t).sum / priceBoundLine.length)

      // check to see whether we have broken out of oscillation. We have done so if the current price is outside the
      // standard dev for the last n prices
      if (math.abs(meanPrice - price) > stdDev * 9) {

        if (microTrend == TrendType.UP) {
          macroTrend = TrendType.UP
        }
        else {
          macroTrend = TrendType.DOWN
        }

        priceBoundLine.clear()
      } else {
        priceBoundLine.enqueue(price)
      }
    }
  }
}
