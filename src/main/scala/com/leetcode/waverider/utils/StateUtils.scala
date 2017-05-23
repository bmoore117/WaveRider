package com.leetcode.waverider.utils

import java.util

import com.leetcode.waverider.data.Sample
import com.mongodb.client.model.Filters
import com.mongodb.{BasicDBObject, MongoClient}
import org.bson.Document
import org.bson.types.ObjectId

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * Created by Ben on 5/21/2017.
  * This class will take in a sample from the database and do the correlations on it to reduce it down to a single integer
  * It will be used to supply data for the MarketStates
  */
object StateUtils {

  val FEATURE_COUNT = 10
  object Features extends Enumeration {
    type Feature = Value
    val High, Low, Volume, Last, BaseVolume, Bid, Ask, OpenBuyOrders, OpenSellOrders, PrevDay = Value
  }

  val client = new MongoClient()
  val clusterCombinations:Seq[Set[Features.Feature]] = preComputeSetIndices()

  val observedStates = new mutable.ListBuffer[String]()

  def preComputeSetIndices(): Seq[Set[Features.Feature]] = {
    val set = Set(Features.High, Features.Low, Features.Volume, Features.Last, Features.BaseVolume, Features.Bid, Features.Ask, Features.OpenBuyOrders, Features.OpenSellOrders, Features.PrevDay)

    val builder = new ArrayBuffer[Set[Features.Feature]]()

    set.subsets().foreach(subset => {
      builder.append(subset)
    })

    builder.toList
  }

  def getNextSample(sampleIdx:Option[BigInt]):Map[String, Array[Double]] = {

    val db = client.getDatabase("db")
    val collection = db.getCollection("marketdata")

    var result:Document = null

    if(sampleIdx.isDefined) {
      val sampleId = new ObjectId(sampleIdx.get.toString(16))
      result = collection.find(Filters.gt("_id", sampleId)).first()
    } else {
      result = collection.find().first()
    }

    val sample = result.get("sample").asInstanceOf[util.ArrayList[Document]]
    val highs, lows, volumes, lasts, baseVolumes, bids, asks, openBuyOrders, openSellOrders, prevDays = Array.ofDim[Double](sample.size())

    for(i <- 0 until sample.size()) {
      val doc = sample.get(i)
      highs(i) = doc.getDouble("High")
      lows(i) = doc.getDouble("Low")
      volumes(i) = doc.getDouble("Volume")
      lasts(i) = doc.getDouble("Last")
      baseVolumes(i) = doc.getDouble("BaseVolume")
      bids(i) = doc.getDouble("Bid")
      asks(i) = doc.getDouble("Ask")
      openBuyOrders(i) = doc.getInteger("OpenBuyOrders").doubleValue()
      openSellOrders(i) = doc.getInteger("OpenSellOrders").doubleValue()
      prevDays(i) = doc.getDouble("PrevDay")
    }

    Map("high" -> highs, "low" -> lows, "volume" -> volumes, "last" -> lasts, "baseVolume" -> baseVolumes,
      "bid" -> bids, "ask" -> asks, "openBuyOrders" -> openBuyOrders, "openSellOrders" -> openSellOrders, "prevDay" -> prevDays)
  }

  def sampleToCluster(sample:Map[String, Array[Double]]): Int = {

    val keyList = sample.keySet.toIndexedSeq
    val builder = new StringBuilder

    for(i <- keyList.indices) {
      val firstName = keyList(i)
      val first = sample(firstName)

      for(j <- i + 1 until keyList.length) {
        val secondName = keyList(j)
        val second = sample(secondName)

        var corr = MathUtils.sampleCorrelation(first, second)
        if(corr < 0.05) {
          corr = 0
        }
        builder.append(math.signum(corr))
      }
    }

    val state = builder.mkString

    if(!observedStates.contains(state)) {
      observedStates.append(state)
    }

    observedStates.indexOf(state)
  }

  def getMaxSampleId():BigInt = {
    val db = client.getDatabase("db")
    val collection = db.getCollection("marketdata")
    val result = collection.find().sort(new BasicDBObject("_id", -1)).first()

    BigInt.apply(result.getObjectId("_id").toHexString, 16)
  }

  def getFirstSampleId():BigInt = {
    val db = client.getDatabase("db")
    val collection = db.getCollection("marketdata")
    val result = collection.find().first()

    BigInt.apply(result.getObjectId("_id").toHexString, 16)
  }
}
