package com.leetcode.waverider.utils

import com.mongodb.{BasicDBObject, MongoClient}
import org.bson.{BSONObject, Document}
import org.bson.types.ObjectId

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

  val clusterCombinations:Seq[Set[Features.Feature]] = preComputeSetIndices()

  def preComputeSetIndices(): Seq[Set[Features.Feature]] = {
    val set = Set(Features.High, Features.Low, Features.Volume, Features.Last, Features.BaseVolume, Features.Bid, Features.Ask, Features.OpenBuyOrders, Features.OpenSellOrders, Features.PrevDay)

    val builder = new ArrayBuffer[Set[Features.Feature]]()

    set.subsets().foreach(subset => {
      builder.append(subset)
    })

    builder.toList
  }

  val client = new MongoClient()

  def getState(sampleId:BigInt):Int = {

    val db = client.getDatabase("db")
    val collection = db.getCollection("marketdata")

    val obj = new BasicDBObject()
    obj.append("_id", new ObjectId(sampleId.toString(16)))

    val sample = collection.find(obj).first()

    //turn sample array into native objs

    //do correlations
    //then indices

    1
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
    val first = collection.find().first().getObjectId("_id")

    BigInt.apply(first.toHexString, 16)
  }

  def enumState(doc: Document): Int = {




   1
  }
}
