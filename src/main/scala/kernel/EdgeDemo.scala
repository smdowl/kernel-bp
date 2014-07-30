package kernel

import java.lang.Class

import breeze.linalg.{BitVector, any, DenseMatrix, DenseVector}
import kernel.caches.EdgeBasedCache
import kernel.kernels.{RBFKernel, LinearKernel}
import kernel.models.MessageParam
import kernel.models.edge.{Inferer, HMMModel}
import kernel.parsing.HMMParser
import kernel.propagation.EdgeBasedMessagePasser

object EdgeDemo {
  val numSamples = 25
  val msgParam: MessageParam = MessageParam(0.1, 3.0)
  val model = new HMMModel(numSamples)
  val kernel = new LinearKernel()
  val parser = new HMMParser(msgParam, kernel)

  def buildPasser(cache: EdgeBasedCache, observedNodes: Set[Int]) = new EdgeBasedMessagePasser(cache, observedNodes)

  def runDemo() = {

    val testIdx = 0

    val results = testSentence(testIdx)

    println(results)
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

    val results = testSet.map{ case (testNode, correctPrediction) =>
        testToken(testNode, correctPrediction, cache, betaArr, inferer, labelKeys)
    }

    results
  }

  def testToken(testNode: Int,
                correctPrediction: DenseMatrix[Double],
                cache: EdgeBasedCache,
                betaArr: Array[Array[DenseMatrix[Double]]],
                inferer: Inferer,
                labelKeys: Array[String]) = {
    val probs = inferer.calculateKernelCondRootMarginal(testNode, cache, betaArr)
    isPredictionCorrect(labelKeys, probs, model.keyIndex, correctPrediction)
  }

  def isPredictionCorrect(labelKeys: Array[String], probs: DenseVector[Double], keyIndex: Map[String, Int], correct: DenseMatrix[Double]) = {
    if (containsNaN(probs))
      false
    else {
      val predictedLabel = labelKeys(probs.argmax)
      val predictedFeature = keyIndex(predictedLabel)
      correct(0, predictedFeature) != 0
    }
  }

  private def containsNaN(vec: DenseVector[Double]) = {
    val test: BitVector = vec :== Double.NaN
    any(test)
  }

  def main(args: Array[String]): Unit = {
    runDemo()
  }
}
