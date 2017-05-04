package com.leetcode.waverider.engines

import java.io.File

import com.leetcode.waverider.data.AnalyzedMarketDay
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
class MLEngine(val trainPath:String, val testPath:String) {


  val seed = 12345
  val iterations = 10
  val nEpochs = 200
  val learningRate = 0.02

  var network:MultiLayerNetwork = _

  def train(): Unit = {

    val trainIterator = getTrainingSet()
    val testIterator = getTestSet()

    val numOutputs = 2
    val nHidden = 15

    network = new MultiLayerNetwork(new NeuralNetConfiguration.Builder()
      .seed(seed)
      .iterations(iterations)
      .optimizationAlgo(OptimizationAlgorithm.STOCHASTIC_GRADIENT_DESCENT)
      .learningRate(learningRate)
      .weightInit(WeightInit.XAVIER)
      .updater(Updater.RMSPROP)
      .list()
      .layer(0, new DenseLayer.Builder().nIn(AnalyzedMarketDay.numberOfFeatures).nOut(AnalyzedMarketDay.numberOfFeatures)
        .activation(Activation.TANH)
        .build())
      .layer(1, new DenseLayer.Builder().nIn(AnalyzedMarketDay.numberOfFeatures).nOut(12)
        .activation(Activation.TANH)
        .build())
      .layer(2, new OutputLayer.Builder(LossFunctions.LossFunction.MSE)
        .activation(Activation.IDENTITY)
        .nIn(12).nOut(numOutputs).build())
      .pretrain(false).backprop(true).build()
    )

    for(i <- 1 to nEpochs) {
      trainIterator.reset()
      network.fit(trainIterator)
    }

    val results = network.evaluateRegression(testIterator)

    println("Test set price r2: " + results.correlationR2(0))
    println("Test set duration r2: " + results.correlationR2(1))

    println("Test set price RMSE: " + results.rootMeanSquaredError(0))
    println("Test set duration RMSE: " + results.rootMeanSquaredError(1))


    println("")
    trainIterator.reset()
    val trainResults = network.evaluateRegression(trainIterator)

    println("Train set price r2: " + trainResults.correlationR2(0))
    println("Train set duration r2: " + trainResults.correlationR2(1))

    println("Train set price RMSE: " + trainResults.rootMeanSquaredError(0))
    println("Train set duration RMSE: " + trainResults.rootMeanSquaredError(1))
  }

  def getTrainingSet(): DataSetIterator = {
    val trainReader = new CSVRecordReader(1, ",")
    trainReader.initialize(new FileSplit(new File(trainPath)))
    val trainIterator = new RecordReaderDataSetIterator(trainReader, 100, AnalyzedMarketDay.numberOfFeatures, AnalyzedMarketDay.numberOfFeatures + 1, true) //15, 16 is idx where labels begin & end

    val trainNormalizer = new NormalizerStandardize()
    trainNormalizer.fit(trainIterator)

    trainIterator.reset()
    trainIterator.setPreProcessor(trainNormalizer)

    trainIterator
  }

  def getTestSet(): DataSetIterator = {
    val testReader = new CSVRecordReader(1, ",")
    testReader.initialize(new FileSplit(new File(testPath)))
    val testIterator = new RecordReaderDataSetIterator(testReader, 100, AnalyzedMarketDay.numberOfFeatures, AnalyzedMarketDay.numberOfFeatures + 1, true) //15, 16 is idx where labels begin & end

    val testNormalizer = new NormalizerStandardize()
    testNormalizer.fit(testIterator)

    testIterator.reset()
    testIterator.setPreProcessor(testNormalizer)

    testIterator
  }


}
