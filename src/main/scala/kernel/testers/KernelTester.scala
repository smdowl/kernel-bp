package kernel.testers

import breeze.linalg.{any, BitVector, DenseVector, DenseMatrix}
import comparison.MarkovModel
import kernel.caches.Cache
import kernel.kernels._
import kernel.models.components.{MessageParam, Inferer}
import kernel.models._
import kernel.models.toyextractors._
import kernel.parsing.HMMParser
import kernel.propagation.MessagePasser

object ToyConfig {
  val NUM_SAMPLES = 30
  val NUM_TEST = 10
  val NUM_REPEATS = 2
  
  val models = Seq(new NonDeterministicHMMModel(NUM_SAMPLES, NUM_TEST))
  val extractors = Seq(new BigramFeatureExtractor)
  val kernels = Seq(new LinearKernel, new RBFKernel)
}

object KernelTester extends App {

  runExperiments()

  def runExperiments() = {
    for (model <- ToyConfig.models) {
      for (extractor <- ToyConfig.extractors) {
        model.setExtractor(extractor)

        var kernelAccuracy, viterbiAccuracy, forwardBackwardAccuracy = 0.0

        for (kernel <- ToyConfig.kernels) {
          for (i <- 0 until ToyConfig.NUM_REPEATS) {
            model.initialise()

            println(s"${getName(model)} with ${getName(extractor)} and ${getName(kernel)}")
            val results = runTest(model, kernel)

            kernelAccuracy += results._1
            viterbiAccuracy += results._2
            forwardBackwardAccuracy += results._3
          }

          kernelAccuracy /= ToyConfig.NUM_REPEATS
          viterbiAccuracy /= ToyConfig.NUM_REPEATS
          forwardBackwardAccuracy /= ToyConfig.NUM_REPEATS

          println(s"Kernel Accuracy: $kernelAccuracy\nViterbi Accuracy: $viterbiAccuracy\nForward-Backward Accuracy: $forwardBackwardAccuracy")
        }
      }
    }
  }

  def getName(obj: Object): String = {
    obj.getClass.getSimpleName
  }

  protected def runTest(model: Model, kernel: Kernel) = {
    val viterbiModel = new MarkovModel()
    val tester = new KernelTester(model, viterbiModel, kernel)

    var kernelAccuracy = 0.0
    var viterbiAccuracy = 0.0
    var forwardBackwardAccuracy = 0.0
    var total = 0
    for (i <- 0 until model.testObservations.length) {
      val (kernelResults, viterbiResults, fbResults) = tester.testSentence(i)
      assert(kernelResults.length == viterbiResults.length, "Should be exactly the same length")
      assert(kernelResults.length == fbResults.length, "Should be exactly the same length")

      kernelAccuracy += kernelResults.count(_ == true)
      viterbiAccuracy += viterbiResults.count(_ == true)
      forwardBackwardAccuracy += fbResults.count(_ == true)
      total += kernelResults.length
    }

    kernelAccuracy /= total
    viterbiAccuracy /= total
    forwardBackwardAccuracy /= total

    (kernelAccuracy, viterbiAccuracy, forwardBackwardAccuracy)
  }


}

class KernelTester(kernelModel: Model, compModel: MarkovModel, kernel: Kernel) {
  val msgParam: MessageParam = MessageParam(1.0, 1.0)
  val parser = new HMMParser(msgParam, kernel)
  val trainingData = reconstructTrainingData()
  val testData = reconstructTestData()
  compModel.train(trainingData)

  def reconstructTrainingData() = {
    val keys = kernelModel.keyArray
    val data = kernelModel.trainData

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
    val keys = kernelModel.keyArray
    val observations = kernelModel.testObservations
    val labels = kernelModel.testLabels

    var output = Seq[Seq[(String, String)]]()

    for ((hidden, visible) <- labels zip observations) {
      var hiddenSeq = Seq[String]()
      var visibleSeq = Seq[String]()

      for (nodeIdx <- 0 until hidden.size) {
        val node = hidden(nodeIdx).toDenseMatrix
        val featureIdx = node.findAll(_.equals(1.0))(0)._2
        hiddenSeq :+= keys(featureIdx).split(":")(1)
      }
      for (nodeIdx <- visible.size until 2 * visible.size) {
        val node = visible(nodeIdx).toDenseMatrix
        val featureIdx = node.findAll(_.equals(1.0))(0)._2
        visibleSeq :+= keys(featureIdx).split(":")(1)
      }

      output :+= (hiddenSeq zip visibleSeq)
    }

    output
  }

  def testSentence(testIdx: Int) = {
    val edges = kernelModel.edges
    val obsArr = kernelModel.testObservations
    val testArr = kernelModel.testLabels
    val (labelKeys, testMatrix) = kernelModel.testMatrix

    val observations = obsArr(testIdx).map{ case (key, sparse) => key -> sparse.toDenseMatrix}
    val testSet = testArr(testIdx).map{ case (key, sparse) => key -> sparse.toDenseMatrix}

    val cache = parser.buildCache(edges, observations.size)

    val passer = buildPasser(cache, observations.keySet)
    val betaArr = passer.passMessages(observations)

    val inferer = new Inferer(testMatrix)

    val kernelResults = (0 until cache.numNodes / 2).map(testNode => {
      val correctPrediction = testSet(testNode)
      testToken(testNode, correctPrediction, cache, betaArr, inferer, labelKeys)
    })

    val viterbiResults = compModel.viterbiTestSentence(testData(testIdx))
    val fbResults = compModel.forwardBackwardTestSentence(testData(testIdx))

    (kernelResults, viterbiResults, fbResults)
  }

  private def buildPasser(cache: Cache, observedNodes: Set[Int]) = new MessagePasser(cache, observedNodes)

  private def testToken(testNode: Int,
                correctPrediction: DenseMatrix[Double],
                cache: Cache,
                betaArr: Array[Array[DenseMatrix[Double]]],
                inferer: Inferer,
                labelKeys: Array[String]) = {
    val probs = inferer.calculateKernelCondRootMarginal(testNode, cache, betaArr)
    isPredictionCorrect(labelKeys, probs, kernelModel.keyIndex, correctPrediction)
  }

  private def isPredictionCorrect(labelKeys: Array[String], probs: DenseVector[Double], keyIndex: Map[String, Int], correct: DenseMatrix[Double]) = {
    if (containsNaN(probs))
      false
    else {
      val predictedLabel = labelKeys(probs.argmax)
//      println(s"predicted: $predictedLabel")
      val predictedFeature = keyIndex(predictedLabel)
      correct(0, predictedFeature) != 0
    }
  }

  private def containsNaN(vec: DenseVector[Double]) = {
    val test: BitVector = vec :== Double.NaN
    any(test)
  }
}
