import java.util.Date

import com.leetcode.waverider.data.RawMarketDay
import com.leetcode.waverider.data.indicators.western.generic.signals.{MACD, MACDBuilder}
import com.tictactec.ta.lib.Core
import org.junit.Assert.assertEquals
import org.junit.Test

import scala.collection.mutable.ListBuffer

/**
  * Created by Ben on 6/27/2017.
  */
class MACDTest {

  /**
    * MACD signal = fastAvg - slowAvg
    * MACD avg = xDay avg of MACD signal
    *
    * Using the straight ascending sequence 1, 2, 3...
    *
    * slow period1: 1-26: 13.5
    * slow period2: 2-27: 14.5
    * slow period3: 3-28: 15.5
    * slow period4: 4-29: 16.5
    * slow period5: 5-30: 17.5
    * slow period6: 6-31: 18.5
    * slow period7: 7-32: 19.5
    * slow period8: 8-33: 20.5
    * slow period9: 9-34: 21.5
    * fast period 1: 15-26: 20.5
    * fast period 2: 16-27: 21.5
    * fast period 3: 17-28: 22.5
    * fast period 4: 18-29: 23.5
    * fast period 5: 19-30: 24.5
    * fast period 6: 20-31: 25.5
    * fast period 7: 21-32: 26.5
    * fast period 8: 22-33: 27.5
    * fast period 9: 23-34: 28.5
    *
    * macd1: 20.5 - 13.5: 7
    * macd2: 21.5 - 14.5: 7macd3: 22.5 - 15.5: 7
    * macd4: 23.5 - 16.5: 7
    * macd5: 24.5 - 17.5: 7
    * macd6: 25.5 - 18.5: 7
    * macd7: 26.5 - 19.5: 7
    * macd8: 27.5 - 20.5: 7
    * macd9: 28.5 - 21.5: 7
    *
    * final val: 7
    * signal val: 7
    *

    * For series less than 34, None
    * For 34: 7 & 7
    * For 35, 7 & 7
    *
    */
  @Test
  def testMACD(): Unit = {
    val core = new Core

    val builder = MACDBuilder(26, 12, 9, "close")
    val macd1 = builder.instantiateIndicator(core, getUndersized(), null, null, null).asInstanceOf[MACD]
    val desired = new MACD(builder)
    assertEquals(macd1.macd, desired.macd)
    assertEquals(macd1.macdSignal, desired.macdSignal)

    val macd2 = builder.instantiateIndicator(core, getProperSized(), null, null, null).asInstanceOf[MACD]
    desired.macdSignal = Some(7.0)
    desired.macd = Some(7.0)
    assertEquals(macd2.macdSignal, desired.macdSignal)
    assertEquals(macd2.macd, desired.macd)

    val macd3 = builder.instantiateIndicator(core, getPlusSized(), null, null, null).asInstanceOf[MACD]
    assertEquals(macd2.macdSignal, desired.macdSignal)
    assertEquals(macd2.macd, desired.macd)
  }

  def getUndersized(): ListBuffer[RawMarketDay] = {
    val buffer = new ListBuffer[RawMarketDay]
    for(i <- 1 to 20) {
      buffer.append(new RawMarketDay(new Date(), 0.0, 0.0, 0.0, i.toDouble, 0, 0.0))
    }

    buffer
  }

  def getProperSized(): ListBuffer[RawMarketDay] = {
    val buffer = new ListBuffer[RawMarketDay]
    for(i <- 1 to 34) {
      buffer.append(new RawMarketDay(new Date(), 0.0, 0.0, 0.0, i.toDouble, 0, 0.0))
    }

    buffer
  }

  def getPlusSized(): ListBuffer[RawMarketDay] = {
    val buffer = new ListBuffer[RawMarketDay]
    for(i <- 1 to 35) {
      buffer.append(new RawMarketDay(new Date(), 0.0, 0.0, 0.0, i.toDouble, 0, 0.0))
    }

    buffer
  }
}
