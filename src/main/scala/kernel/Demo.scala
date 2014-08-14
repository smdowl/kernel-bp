package kernel

import breeze.linalg._
import kernel.caches.Cache
import kernel.kernels.{RBFKernel, LinearKernel}
import kernel.models.components.{MessageParam, Inferer}
import kernel.models.RealPOSModel
import kernel.models.edge._
import kernel.parsing.HMMParser
import kernel.propagation.MessagePasser

object Demo {
  val numSamples = 50
  val msgParam: MessageParam = MessageParam(1.0, 1.0)
  val model = new RealPOSModel()
  val kernel = new LinearKernel()
  val parser = new HMMParser(msgParam, kernel)

  def buildPasser(cache: Cache, observedNodes: Set[Int]) = new MessagePasser(cache, observedNodes)

  def runDemo() = {

    var correct = 0.0
    var total = 0.0

    for (testIdx <- 0 until min(numSamples, 10)) {
      val results = testSentence(testIdx)

      for (r <- results)
        if (r) correct += 1

      println(results)

      total += results.size
    }

    println(s"${correct / total} are correct")
  }

  def testSentence(testIdx: Int) = {
    val edges = model.edges
    val obsArr = model.testObservations
    val testArr = model.testLabels
    val (labelKeys, testMatrix) = model.testMatrix

    val observations = obsArr(testIdx).map{case (key, matrix) =>
      (key, matrix.toDenseMatrix)
    }
    val testSet = testArr(testIdx).map{case (key, matrix) =>
      (key, matrix.toDenseMatrix)
    }

    val cache = parser.buildCache(edges, observations.size)

    val passer = buildPasser(cache, observations.keySet)
    val betaArr = passer.passMessages(observations)

    val inferer = new Inferer(testMatrix)

    val results = (0 until cache.numNodes / 2).map(testNode => {
      val correctPrediction = testSet(testNode)
      testToken(testNode, correctPrediction, cache, betaArr, inferer, labelKeys)
    })

    results
  }

  def testToken(testNode: Int,
                correctPrediction: DenseMatrix[Double],
                cache: Cache,
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
      println(s"predicted: $predictedLabel")
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
