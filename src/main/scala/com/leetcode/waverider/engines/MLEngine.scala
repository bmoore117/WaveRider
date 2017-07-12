package com.leetcode.waverider.engines

import java.io.File

import org.datavec.api.records.reader.impl.csv.CSVRecordReader
import org.datavec.api.split.FileSplit
import org.deeplearning4j.datasets.datavec.RecordReaderDataSetIterator
import org.deeplearning4j.nn.api.OptimizationAlgorithm
import org.deeplearning4j.nn.conf.layers.{DenseLayer, OutputLayer}
import org.deeplearning4j.nn.conf.{NeuralNetConfiguration, Updater}
import org.deeplearning4j.nn.multilayer.MultiLayerNetwork
import org.deeplearning4j.nn.weights.WeightInit
import org.nd4j.linalg.activations.Activation
import org.nd4j.linalg.dataset.api.iterator.DataSetIterator
import org.nd4j.linalg.dataset.api.preprocessor.NormalizerStandardize
import org.nd4j.linalg.lossfunctions.LossFunctions

/**
  * Created by Ben on 4/26/2017.
  */
class MLEngine(val trainPath:String, val testPath:String, val numFeatures:Int) {

  val seed = 12345
  val iterations = 10
  val nEpochs = 5
  val learningRate = 0.02

  var network:MultiLayerNetwork = _

  def evaluate(): Double = {
    val trainIterator = getTrainingSet()
    val testIterator = getTestSet()

    //val learningRateSpace = new ContinuousParameterSpace(0.0001, 0.1)
    val network = new MultiLayerNetwork(new NeuralNetConfiguration.Builder()
      .seed(seed)
      .iterations(iterations)
      .optimizationAlgo(OptimizationAlgorithm.STOCHASTIC_GRADIENT_DESCENT)
      .learningRate(learningRate)
      .weightInit(WeightInit.XAVIER)
      .updater(Updater.SGD)
      //.regularization(true).l2(0.1)
      .list()
      .layer(0, new DenseLayer.Builder().nIn(trainIterator.inputColumns()).nOut(50)
        .activation(Activation.TANH)
        .build())
      .layer(1, new DenseLayer.Builder().nIn(50).nOut(100)
        .activation(Activation.TANH)
        .build())
      .layer(2, new DenseLayer.Builder().nIn(100).nOut(50)
        .activation(Activation.TANH)
        .build())
      .layer(3, new OutputLayer.Builder(LossFunctions.LossFunction.MSE)
        .activation(Activation.SOFTMAX)
        .nIn(50).nOut(2).build())
        .pretrain(true).backprop(true).build()
    )

    for(_ <- 1 to nEpochs) {
      trainIterator.reset()
      network.fit(trainIterator)
    }

    trainIterator.reset()
    var results = network.evaluate(trainIterator)
    println("Train set price accuracy: " + results.accuracy())

    results = network.evaluate(testIterator)
    println("Test set price accuracy: " + results.accuracy())
    results.accuracy()
  }

  private def getTrainingSet(): DataSetIterator = {
    val trainReader = new CSVRecordReader(1, ",")
    trainReader.initialize(new FileSplit(new File(trainPath)))
    val trainIterator = new RecordReaderDataSetIterator(trainReader, 100, -1, 2)
    val trainNormalizer = new NormalizerStandardize()
    trainNormalizer.fit(trainIterator)

    trainIterator.reset()
    trainIterator.setPreProcessor(trainNormalizer)

    trainIterator
  }

  private def getTestSet(): DataSetIterator = {
    val testReader = new CSVRecordReader(1, ",")
    testReader.initialize(new FileSplit(new File(testPath)))
    val testIterator = new RecordReaderDataSetIterator(testReader, 100, -1, 2)

    val testNormalizer = new NormalizerStandardize()
    testNormalizer.fit(testIterator)

    testIterator.reset()
    testIterator.setPreProcessor(testNormalizer)

    testIterator
  }
}
