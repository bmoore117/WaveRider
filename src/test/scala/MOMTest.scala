import java.util.Date

import com.leetcode.waverider.data.RawMarketDay
import com.leetcode.waverider.data.indicators.western.generic.rate.{MOM, MOMBuilder}
import com.tictactec.ta.lib.Core
import org.junit.Assert.assertEquals
import org.junit.Test

import scala.collection.mutable.ListBuffer

/**
  * Created by Ben on 6/26/2017.
  */
class MOMTest {

  val core = new Core

  /**
    * Momentum formula = price(today) - price(n days ago)
    * For period today - 1, using sequence 1, 2, 3, 4 as data:
    * None,
    * 1,
    * 1,
    * 1,
    *
    * For today - 2, results should be:
    * None,
    * None,
    * 2,
    * 2,
    *
    */
  @Test
  def testMOM(): Unit = {
    val day1 = new RawMarketDay("", 0.0, 0.0, 0.0, 1.0, 0, 0.0)
    val day2 = new RawMarketDay("", 0.0, 0.0, 0.0, 2.0, 0, 0.0)
    val day3 = new RawMarketDay("", 0.0, 0.0, 0.0, 3.0, 0, 0.0)
    val day4 = new RawMarketDay("", 0.0, 0.0, 0.0, 4.0, 0, 0.0)
    val slice1 = ListBuffer(day1)
    val slice2 = ListBuffer(day1, day2)
    val slice3 = ListBuffer(day1, day2, day3)
    val slice4 = ListBuffer(day1, day2, day3, day4)

    var builder = MOMBuilder(1, "close")
    var mom1 = builder.instantiateIndicator(core, slice1, null, null, null).asInstanceOf[MOM]
    var desired = new MOM(builder)
    assertEquals(mom1.value, desired.value)

    var mom2 = builder.instantiateIndicator(core, slice2, null, null, null).asInstanceOf[MOM]
    desired = new MOM(builder)
    desired.value = Some(1.0)
    assertEquals(mom2.value, desired.value)

    var mom3 = builder.instantiateIndicator(core, slice3, null, null, null).asInstanceOf[MOM]
    desired = new MOM(builder)
    desired.value = Some(1.0)
    assertEquals(mom3.value, desired.value)

    var mom4 = builder.instantiateIndicator(core, slice4, null, null, null).asInstanceOf[MOM]
    desired = new MOM(builder)
    desired.value = Some(1.0)
    assertEquals(mom4.value, desired.value)

    //____________________________________________________

    builder = MOMBuilder(2, "close")
    mom1 = builder.instantiateIndicator(core, slice1, null, null, null).asInstanceOf[MOM]
    desired = new MOM(builder)
    assertEquals(mom1.value, desired.value)

    mom2 = builder.instantiateIndicator(core, slice2, null, null, null).asInstanceOf[MOM]
    desired = new MOM(builder)
    assertEquals(mom2.value, desired.value)

    mom3 = builder.instantiateIndicator(core, slice3, null, null, null).asInstanceOf[MOM]
    desired = new MOM(builder)
    desired.value = Some(2.0)
    assertEquals(mom3.value, desired.value)

    mom4 = builder.instantiateIndicator(core, slice4, null, null, null).asInstanceOf[MOM]
    desired = new MOM(builder)
    desired.value = Some(2.0)
    assertEquals(mom4.value, desired.value)
  }
}
