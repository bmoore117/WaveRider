import java.util.Date

import com.leetcode.waverider.data.RawMarketDay
import com.leetcode.waverider.data.indicators.western.generic.rate.{ROCR, ROCRBuilder}
import com.tictactec.ta.lib.Core
import org.junit.Assert.assertEquals
import org.junit.Test

import scala.collection.mutable.ListBuffer

/**
  * Created by Ben on 6/26/2017.
  */
class ROCRTest {

  val core = new Core

  /**
    * Momentum formula = price(today) - price(n days ago)
    * For today - 1, results should be:
    * None,
    * 2,
    * 1.5,
    * 1.33
    *
    * For today - 2, results should be:
    * None,
    * None,
    * 3,
    * 2
    */
  @Test
  def testROCR(): Unit = {
    val day1 = new RawMarketDay("", 0.0, 0.0, 0.0, 1.0, 0, 0.0)
    val day2 = new RawMarketDay("", 0.0, 0.0, 0.0, 2.0, 0, 0.0)
    val day3 = new RawMarketDay("", 0.0, 0.0, 0.0, 3.0, 0, 0.0)
    val day4 = new RawMarketDay("", 0.0, 0.0, 0.0, 4.0, 0, 0.0)
    val slice1 = ListBuffer(day1)
    val slice2 = ListBuffer(day1, day2)
    val slice3 = ListBuffer(day1, day2, day3)
    val slice4 = ListBuffer(day1, day2, day3, day4)

    var builder = ROCRBuilder(1, "close")
    var result1 = builder.instantiateIndicator(core, slice1, null, null, null).asInstanceOf[ROCR]
    var desired = new ROCR(builder)
    assertEquals(result1.value, desired.value)

    var result2 = builder.instantiateIndicator(core, slice2, null, null, null).asInstanceOf[ROCR]
    desired = new ROCR(builder)
    desired.value = Some(2.0)
    assertEquals(result2.value, desired.value)

    var result3 = builder.instantiateIndicator(core, slice3, null, null, null).asInstanceOf[ROCR]
    desired = new ROCR(builder)
    desired.value = Some(1.5)
    assertEquals(result3.value, desired.value)

    var result4 = builder.instantiateIndicator(core, slice4, null, null, null).asInstanceOf[ROCR]
    desired = new ROCR(builder)
    desired.value = Some(1.3333333333333333)
    assertEquals(result4.value, desired.value)

    //____________________________________________________________________________
    builder = ROCRBuilder(2, "close")
    result1 = builder.instantiateIndicator(core, slice1, null, null, null).asInstanceOf[ROCR]
    desired = new ROCR(builder)
    assertEquals(result1.value, desired.value)

    result2 = builder.instantiateIndicator(core, slice2, null, null, null).asInstanceOf[ROCR]
    desired = new ROCR(builder)
    assertEquals(result2.value, desired.value)

    result3 = builder.instantiateIndicator(core, slice3, null, null, null).asInstanceOf[ROCR]
    desired = new ROCR(builder)
    desired.value = Some(3.0)
    assertEquals(result3.value, desired.value)

    result4 = builder.instantiateIndicator(core, slice4, null, null, null).asInstanceOf[ROCR]
    desired = new ROCR(builder)
    desired.value = Some(2.0)
    assertEquals(result4.value, desired.value)

  }
}
