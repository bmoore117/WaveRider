package com.leetcode.waverider.services

import java.util

import com.leetcode.waverider.adapters.DataSource
import com.mongodb.{BasicDBObject, MongoClient}
import com.mongodb.client.model.Filters
import org.bson.Document
import org.bson.types.ObjectId

/**
  * Created by Ben on 5/27/2017.
  */
class DBService extends DataSource {

  val client = new MongoClient()

  var lastSample:Option[ObjectId] = None

  def getNextSample:Option[Map[String, Array[Double]]] = {

    val db = client.getDatabase("db")
    val collection = db.getCollection("marketdata")

    var result:Document = null

    if(lastSample.isDefined) {
      if(lastSample.get.equals(getMaxSampleId)) {
        return None
      }
      result = collection.find(Filters.gt("_id", lastSample.get)).first()
    } else {
      result = collection.find().first()
    }
    lastSample = Some(result.getObjectId("_id"))
    //println(result.getObjectId().toHexString)

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

    Some(Map("high" -> highs, "low" -> lows, "volume" -> volumes, "last" -> lasts, "baseVolume" -> baseVolumes,
      "bid" -> bids, "ask" -> asks, "openBuyOrders" -> openBuyOrders, "openSellOrders" -> openSellOrders, "prevDay" -> prevDays))
  }

  def getMaxSampleId:ObjectId = {
    val db = client.getDatabase("db")
    val collection = db.getCollection("marketdata")
    val result = collection.find().sort(new BasicDBObject("_id", -1)).first()

    result.getObjectId("_id")
  }

  def getFirstSampleId:ObjectId = {
    val db = client.getDatabase("db")
    val collection = db.getCollection("marketdata")
    val result = collection.find().first()

    result.getObjectId("_id")
  }

  def getPriceForCurrentSample:Double = {
    val db = client.getDatabase("db")
    val collection = db.getCollection("marketdata")

    val result = collection.find(Filters.eq("_id", lastSample.get)).first()
    val sample = result.get("sample").asInstanceOf[util.ArrayList[Document]]

    val asks = Array.ofDim[Double](sample.size())

    for(i <- 0 until sample.size()) {
      val doc = sample.get(i)
      asks(i) = doc.getDouble("Ask")
    }

    asks.sum / asks.length
  }

}
