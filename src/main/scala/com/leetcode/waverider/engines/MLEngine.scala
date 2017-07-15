package com.leetcode.waverider.engines

import java.io.File
import java.nio.file.Paths
import java.util.concurrent.TimeUnit

import org.datavec.api.records.reader.impl.csv.CSVRecordReader
import org.datavec.api.split.FileSplit
import org.deeplearning4j.arbiter.{DL4JConfiguration, MultiLayerSpace}
import org.deeplearning4j.arbiter.data.DataSetIteratorProvider
import org.deeplearning4j.arbiter.layers.{DenseLayerSpace, OutputLayerSpace}
import org.deeplearning4j.arbiter.optimize.api.saving.InMemoryResultSaver
import org.deeplearning4j.arbiter.optimize.api.termination.{MaxCandidatesCondition, MaxTimeCondition, TerminationCondition}
import org.deeplearning4j.arbiter.optimize.candidategenerator.RandomSearchGenerator
import org.deeplearning4j.arbiter.optimize.config.OptimizationConfiguration
import org.deeplearning4j.arbiter.optimize.parameter.continuous.ContinuousParameterSpace
import org.deeplearning4j.arbiter.optimize.parameter.integer.IntegerParameterSpace
import org.deeplearning4j.arbiter.optimize.runner.LocalOptimizationRunner
import org.deeplearning4j.arbiter.optimize.ui.ArbiterUIServer
import org.deeplearning4j.arbiter.optimize.ui.listener.UIOptimizationRunnerStatusListener
import org.deeplearning4j.arbiter.saver.local.multilayer.LocalMultiLayerNetworkSaver
import org.deeplearning4j.arbiter.scoring.multilayer.TestSetAccuracyScoreFunction
import org.deeplearning4j.arbiter.task.MultiLayerNetworkTaskCreator
import org.deeplearning4j.datasets.datavec.RecordReaderDataSetIterator
import org.deeplearning4j.earlystopping.EarlyStoppingConfiguration
import org.deeplearning4j.earlystopping.saver.{InMemoryModelSaver, LocalFileModelSaver}
import org.deeplearning4j.earlystopping.scorecalc.DataSetLossCalculator
import org.deeplearning4j.earlystopping.termination.{MaxEpochsTerminationCondition, MaxTimeIterationTerminationCondition}
import org.deeplearning4j.nn.api.OptimizationAlgorithm
import org.deeplearning4j.nn.conf.Updater
import org.deeplearning4j.nn.multilayer.MultiLayerNetwork
import org.deeplearning4j.nn.weights.WeightInit
import org.nd4j.linalg.dataset.api.iterator.DataSetIterator
import org.nd4j.linalg.dataset.api.preprocessor.NormalizerStandardize
import org.nd4j.linalg.lossfunctions.LossFunctions

/**
  * Created by Ben on 4/26/2017.
  */
class MLEngine(trainPath: String, testPath: String) {

  val seed = 12345
  val iterations = 1
  val epochs = 100
  val learningRate = 0.01

  var network:MultiLayerNetwork = _

  def evaluate(): Double = {
    val trainIterator = getTrainingSet()
    val testIterator = getTestSet()
    val dataProvider = new DataSetIteratorProvider(trainIterator, testIterator)

    val learningRateSpace = new ContinuousParameterSpace(0.01, 0.7)
    val layer1SizeSpace = new IntegerParameterSpace(trainIterator.inputColumns(), 50)
    val layer2SizeSpace = new IntegerParameterSpace(50, 150)
    val layer3SizeSpace = new IntegerParameterSpace(50, 256)


    val earlySaveDir = "earlyStoppingModels"
    val temp = new File(earlySaveDir)
    if(temp.exists()) temp.delete()
    temp.mkdir()

    val stopConf = new EarlyStoppingConfiguration.Builder()
      .epochTerminationConditions(new MaxEpochsTerminationCondition(epochs))
      .iterationTerminationConditions(new MaxTimeIterationTerminationCondition(20, TimeUnit.MINUTES))
      .scoreCalculator(new DataSetLossCalculator(testIterator, true))
      .evaluateEveryNEpochs(1)
      .modelSaver(new InMemoryModelSaver[MultiLayerNetwork]())
      .build()

    val paramSpace = new MultiLayerSpace.Builder()
      .seed(seed)
      .iterations(iterations)
      .optimizationAlgo(OptimizationAlgorithm.STOCHASTIC_GRADIENT_DESCENT)
      .learningRate(learningRateSpace)
      .weightInit(WeightInit.XAVIER)
      .updater(Updater.NESTEROVS).momentum(0.5)
      .earlyStoppingConfiguration(stopConf)
      .addLayer(new DenseLayerSpace.Builder()
        .nIn(trainIterator.inputColumns())
        .nOut(layer1SizeSpace)
        .activation("tanh")
        .build())
      .addLayer(new DenseLayerSpace.Builder()
        .nIn(layer1SizeSpace)
        .nOut(layer2SizeSpace)
        .activation("tanh")
        .build())
      .addLayer(new DenseLayerSpace.Builder()
        .nIn(layer2SizeSpace)
        .nOut(layer3SizeSpace)
        .activation("tanh")
        .build())
      .addLayer(new OutputLayerSpace.Builder()
        .nIn(layer3SizeSpace)
        .nOut(2)
        .lossFunction(LossFunctions.LossFunction.MCXENT)
        .activation("softmax")
        .build())
      .pretrain(true).backprop(true).build()

    val candidateGenerator = new RandomSearchGenerator(paramSpace)

    val baseSaveDirectory = "arbiter"
    val f = new File(baseSaveDirectory)
    if(f.exists()) f.delete()
    f.mkdir()
    val modelSaver = new InMemoryResultSaver[DL4JConfiguration, MultiLayerNetwork, Object]()

    val scoreFunction = new TestSetAccuracyScoreFunction()

    val terminationConditions = new java.util.ArrayList[TerminationCondition]()
    terminationConditions.add(new MaxTimeCondition(15, TimeUnit.MINUTES))
    terminationConditions.add(new MaxCandidatesCondition(20))

    val optimizationConfig = new OptimizationConfiguration.Builder()
      .candidateGenerator(candidateGenerator)
      .dataProvider(dataProvider)
      .modelSaver(modelSaver)
      .scoreFunction(scoreFunction)
      .terminationConditions(terminationConditions)
      .build()

    val runner = new LocalOptimizationRunner[DL4JConfiguration, MultiLayerNetwork, DataSetIterator, Object](optimizationConfig, new MultiLayerNetworkTaskCreator())

    val server = ArbiterUIServer.getInstance()
    runner.addListeners(new UIOptimizationRunnerStatusListener(server))

    runner.execute()

    val sb = new StringBuilder()
    sb.append("Best score: ").append(runner.bestScore()).append("\n")
      .append("Index of model with best score: ").append(runner.bestScoreCandidateIndex()).append("\n")
      .append("Number of configurations evaluated: ").append(runner.numCandidatesCompleted()).append("\n")

    val indexOfBestResult = runner.bestScoreCandidateIndex
    val allResults = runner.getResults
    val bestResult = allResults.get(indexOfBestResult).getResult

    val bestModel = bestResult.getResult

    println("Configuration of best model:")
    println(bestModel.getLayerWiseConfigurations.toJson)

    Thread.sleep(60000)

    runner.bestScore()
  }

  private def getTrainingSet(): DataSetIterator = {
    val trainReader = new CSVRecordReader(0, ",")
    trainReader.initialize(new FileSplit(new File(trainPath)))
    val trainIterator = new RecordReaderDataSetIterator(trainReader, 100, -1, 2)
    val trainNormalizer = new NormalizerStandardize()
    trainNormalizer.fit(trainIterator)

    trainIterator.reset()
    trainIterator.setPreProcessor(trainNormalizer)

    trainIterator
  }

  private def getTestSet(): DataSetIterator = {
    val testReader = new CSVRecordReader(0, ",")
    testReader.initialize(new FileSplit(new File(testPath)))
    val testIterator = new RecordReaderDataSetIterator(testReader, 100, -1, 2)

    val testNormalizer = new NormalizerStandardize()
    testNormalizer.fit(testIterator)

    testIterator.reset()
    testIterator.setPreProcessor(testNormalizer)

    testIterator
  }
}
