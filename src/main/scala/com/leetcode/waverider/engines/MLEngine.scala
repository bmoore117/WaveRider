package com.leetcode.waverider.engines

import java.io.File

import org.datavec.api.records.reader.impl.csv.CSVRecordReader
import org.datavec.api.split.FileSplit
import org.deeplearning4j.datasets.datavec.RecordReaderDataSetIterator
import org.deeplearning4j.nn.multilayer.MultiLayerNetwork
import org.nd4j.linalg.dataset.api.iterator.DataSetIterator

/**
  * Created by Ben on 4/26/2017.
  */
class MLEngine(val trainPath:String) {

  var network:MultiLayerNetwork = _


  def train(): Unit = {

    val recordReader = new CSVRecordReader(1, ",")
    recordReader.initialize(new FileSplit(new File(trainPath)))
    //reader,label index,number of possible labels
    val iterator = new RecordReaderDataSetIterator(recordReader, 100, 0, 1257, true)



  }


  def getDatasetIterator(): DataSetIterator = {



  }


}
