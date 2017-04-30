package com.leetcode.waverider.engines

import java.io.File

import com.leetcode.waverider.data.AnalyzedMarketDay
import org.datavec.api.records.reader.impl.csv.CSVRecordReader
import org.datavec.api.split.FileSplit
import org.datavec.api.transform.TransformProcess
import org.datavec.api.transform.schema.Schema
import org.datavec.api.transform.transform.normalize.Normalize
import org.deeplearning4j.datasets.datavec.RecordReaderDataSetIterator
import org.deeplearning4j.nn.api.OptimizationAlgorithm
import org.deeplearning4j.nn.conf.layers.{DenseLayer, OutputLayer}
import org.deeplearning4j.nn.conf.{NeuralNetConfiguration, Updater}
import org.deeplearning4j.nn.multilayer.MultiLayerNetwork
import org.deeplearning4j.nn.weights.WeightInit
import org.nd4j.linalg.activations.Activation
import org.nd4j.linalg.dataset.api.iterator.DataSetIterator
import org.nd4j.linalg.dataset.api.preprocessor.{DataNormalization, NormalizerStandardize}
import org.nd4j.linalg.lossfunctions.LossFunctions

/**
  * Created by Ben on 4/26/2017.
  */
class MLEngine(val trainPath:String, val testPath:String) {


  val seed = 12345
  val iterations = 10
  val nEpochs = 200
  val nSamples = 1000
  val batchSize = 100
  val learningRate = 0.01

  var network:MultiLayerNetwork = _

  def train(): Unit = {

    val trainIterator = getTrainingSet()
    val testIterator = getTestSet()

    val numInput = 15
    val numOutputs = 2
    val nHidden = 15

    network = new MultiLayerNetwork(new NeuralNetConfiguration.Builder()
      .seed(seed)
      .iterations(iterations)
      .optimizationAlgo(OptimizationAlgorithm.STOCHASTIC_GRADIENT_DESCENT)
      .learningRate(learningRate)
      .weightInit(WeightInit.XAVIER)
      .updater(Updater.NESTEROVS).momentum(0.9)
      .list()
      .layer(0, new DenseLayer.Builder().nIn(numInput).nOut(nHidden)
        .activation(Activation.TANH)
        .build())
      .layer(1, new DenseLayer.Builder().nIn(numInput).nOut(nHidden)
        .activation(Activation.TANH)
        .build())
      .layer(2, new OutputLayer.Builder(LossFunctions.LossFunction.MSE)
        .activation(Activation.IDENTITY)
        .nIn(nHidden).nOut(numOutputs).build())
      .pretrain(false).backprop(true).build()
    )

    for(i <- 1 to nEpochs) {
      trainIterator.reset()
      network.fit(trainIterator)
    }

    val results = network.output(testIterator)

    for(i <- 0 until results.rows() - 1) {
      println("valueChg: " + results.getRow(i).getColumn(0))
      println("trendLength: " + results.getRow(i).getColumn(1))
    }

  }

  def getTrainingSet(): DataSetIterator = {
    val trainReader = new CSVRecordReader(1, ",")
    trainReader.initialize(new FileSplit(new File(trainPath)))
    val trainIterator = new RecordReaderDataSetIterator(trainReader, 100, 15, 16, true) //15, 16 is idx where labels begin & end

    val trainNormalizer = new NormalizerStandardize()
    trainNormalizer.fit(trainIterator)

    trainIterator.reset()
    trainIterator.setPreProcessor(trainNormalizer)

    trainIterator
  }

  def getTestSet(): DataSetIterator = {
    val testReader = new CSVRecordReader(1, ",")
    testReader.initialize(new FileSplit(new File(testPath)))
    val testIterator = new RecordReaderDataSetIterator(testReader, 100, 15, 16, true) //15, 16 is idx where labels begin & end

    val testNormalizer = new NormalizerStandardize()
    testNormalizer.fit(testIterator)

    testIterator.reset()
    testIterator.setPreProcessor(testNormalizer)

    testIterator
  }


}
