package kernel.testers

import breeze.linalg.{any, BitVector, DenseVector, DenseMatrix}
import comparison.ViterbiMarkovModel
import kernel.caches.EdgeBasedCache
import kernel.kernels.LinearKernel
import kernel.models.MessageParam
import kernel.models.edge.{DeterministicHMMModel, EdgeModel, Inferer}
import kernel.parsing.HMMParser
import kernel.propagation.EdgeBasedMessagePasser

object KernelTester extends App {
  val model = new DeterministicHMMModel(10)
  val tester = new KernelTester(model)

  for (i <- 0 until model.testObservations.length) {
    val (kernelResults, compResults) = tester.testSentence(i)
    println(kernelResults)
    println(compResults)
    println()
  }
}

class KernelTester(model: EdgeModel) {
  val kernel = new LinearKernel()
  val msgParam: MessageParam = MessageParam(1.0, 1.0)
  val parser = new HMMParser(msgParam, kernel)
  val trainingData = reconstructTrainingData()
  val testData = reconstructTestData()
  val compModel = new ViterbiMarkovModel()
  compModel.train(trainingData)

  def reconstructTrainingData() = {
    val keys = model.keyArray
    val data = model.trainData

    var output = Seq[Seq[(String, String)]]()

    for (sentence <- data) {
      val hidden = sentence.slice(0, sentence.length / 2)
      val visible = sentence.slice(sentence.length / 2, sentence.length)

      var hiddenSeq = Seq[String]()
      var visibleSeq = Seq[String]()

      for (node <- hidden) {
        val featureIdx = node.findAll(_.equals(1.0))(0)
        hiddenSeq :+= keys(featureIdx).split(":")(1)
      }
      for (node <- visible) {
        val featureIdx = node.findAll(_.equals(1.0))(0)
        visibleSeq :+= keys(featureIdx).split(":")(1)
      }

      output :+= (hiddenSeq zip visibleSeq)
    }

    output
  }

  def reconstructTestData() = {
    val keys = model.keyArray
    val observations = model.testObservations
    val labels = model.testLabels

    var output = Seq[Seq[(String, String)]]()

    for ((hidden, visible) <- labels zip observations) {
      var hiddenSeq = Seq[String]()
      var visibleSeq = Seq[String]()

      for (nodeIdx <- 0 until hidden.size) {
        val node = hidden(nodeIdx).toDenseVector
        val featureIdx = node.findAll(_.equals(1.0))(0)
        hiddenSeq :+= keys(featureIdx).split(":")(1)
      }
      for (nodeIdx <- visible.size until 2 * visible.size) {
        val node = visible(nodeIdx).toDenseVector
        val featureIdx = node.findAll(_.equals(1.0))(0)
        visibleSeq :+= keys(featureIdx).split(":")(1)
      }

      output :+= (hiddenSeq zip visibleSeq)
    }

    output
  }

  def testSentence(testIdx: Int) = {
    val edges = model.edges
    val obsArr = model.testObservations
    val testArr = model.testLabels
    val (labelKeys, testMatrix) = model.testMatrix

    val observations = obsArr(testIdx)
    val testSet = testArr(testIdx)

    val cache = parser.buildCache(edges, observations.size)

    val passer = buildPasser(cache, observations.keySet)
    val betaArr = passer.passMessages(observations)

    val inferer = new Inferer(testMatrix)

    val kernelResults = (0 until cache.numNodes / 2).map(testNode => {
      val correctPrediction = testSet(testNode)
      testToken(testNode, correctPrediction, cache, betaArr, inferer, labelKeys)
    })

    val compResults = compModel.testSentence(testData(testIdx))

    (kernelResults, compResults)
  }

  private def buildPasser(cache: EdgeBasedCache, observedNodes: Set[Int]) = new EdgeBasedMessagePasser(cache, observedNodes)

  private def testToken(testNode: Int,
                correctPrediction: DenseMatrix[Double],
                cache: EdgeBasedCache,
                betaArr: Array[Array[DenseMatrix[Double]]],
                inferer: Inferer,
                labelKeys: Array[String]) = {
    val probs = inferer.calculateKernelCondRootMarginal(testNode, cache, betaArr)
    isPredictionCorrect(labelKeys, probs, model.keyIndex, correctPrediction)
  }

  private def isPredictionCorrect(labelKeys: Array[String], probs: DenseVector[Double], keyIndex: Map[String, Int], correct: DenseMatrix[Double]) = {
    if (containsNaN(probs))
      false
    else {
      val predictedLabel = labelKeys(probs.argmax)
      println(s"predicted: $predictedLabel")
      val predictedFeature = keyIndex(predictedLabel)
      correct(0, predictedFeature) != 0
    }
  }

  private def containsNaN(vec: DenseVector[Double]) = {
    val test: BitVector = vec :== Double.NaN
    any(test)
  }
}
