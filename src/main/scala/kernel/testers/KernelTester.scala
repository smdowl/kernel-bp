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
  val TEST = false

  def NUM_SAMPLES = if (TEST) 1 else 10
  def NUM_TEST = if (TEST) 0 else 10
  def NUM_REPEATS = if (TEST) 1 else 20
  def SENTENCE_LENGTH = if (TEST) 3 else 15
  def model = if (TEST) new DeterministicHMMModel(NUM_SAMPLES, NUM_TEST) else new TrigramModel(NUM_SAMPLES, NUM_TEST)

  val models = Seq(model)
//  val extractors = Seq(new UnigramFeatureExtractor, new BigramFeatureExtractor, new TrigramFeatureExtractor)
//  val extractors = Seq(new UnigramChainOnlyFeatureExtractor, new BigramChainOnlyFeatureExtractor, new TrigramChainOnlyFeatureExtractor)

  val extractors = Seq(new JointTrigramFeatureExtractor)
  val kernels = Seq(new LinearKernel)
  val smooth = Seq(false)
  val numIter = Seq(50)
  val lambda: Seq[Double] = Seq(1e6)
}

object KernelTester extends App {

  runExperiments()

  def runExperiments() = {
    var model: ProbabalisticHMMModel = null

    for (nextModel <- ToyConfig.models) {
      model = nextModel
      model.minLength = ToyConfig.SENTENCE_LENGTH

      println(s"${getName(model)}")
      println(s"transition matrix:\n${model.getTransitionMatrix}")
      println(s"emission matrix:\n${model.getEmissionMatrix}")
      println()

      for (numIter <- ToyConfig.numIter) {

        for (extractor <- ToyConfig.extractors) {
          model.setExtractor(extractor)
          for (kernel <- ToyConfig.kernels) {
            for (smooth <- ToyConfig.smooth) {
              for (lambda <- ToyConfig.lambda) {
              val smoothString = if (smooth) " (smoothed)" else ""

              println(s"${getName(extractor)} and ${getName(kernel)}$smoothString with $numIter iterations lambda=$lambda")

              var kernelAccuracy, viterbiAccuracy, forwardBackwardAccuracy = 0.0

              for (i <- 0 until ToyConfig.NUM_REPEATS) {
                model.initialise()

                val results = runTest(model, kernel, smooth, numIter, lambda)

                kernelAccuracy += results._1
                viterbiAccuracy += results._2
                forwardBackwardAccuracy += results._3
              }

              kernelAccuracy /= ToyConfig.NUM_REPEATS
              viterbiAccuracy /= ToyConfig.NUM_REPEATS
              forwardBackwardAccuracy /= ToyConfig.NUM_REPEATS

              println(s"Kernel Accuracy: $kernelAccuracy\nViterbi Accuracy: $viterbiAccuracy\nForward-Backward Accuracy: $forwardBackwardAccuracy")
              println()

              }
            }
          }
        }

        println()

        if (model.isInstanceOf[RandomNonDeterministicHMMModel])
          model = new RandomNonDeterministicHMMModel(ToyConfig.NUM_SAMPLES, ToyConfig.NUM_TEST)
      }
    }
  }

  def getName(obj: Object): String = {
    obj.getClass.getSimpleName
  }

  protected def runTest(model: Model, kernel: Kernel, smooth: Boolean, numIter: Int, lambda: Double) = {
    val viterbiModel = new MarkovModel()
    val tester = new KernelTester(model, viterbiModel, kernel, smooth, numIter, lambda)

    var kernelAccuracy = 0.0
    var viterbiAccuracy = 0.0
    var forwardBackwardAccuracy = 0.0
    var total = 0
    for (i <- 0 until model.testObservations.length) {
      val (kernelResults, viterbiResults, fbResults) = tester.testSentence(i)

//      println(kernelResults)

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

class KernelTester(kernelModel: Model, compModel: MarkovModel, kernel: Kernel, smooth: Boolean, numIter: Int, lambda: Double) {
  val msgParam: MessageParam = MessageParam(lambda, 1.0)
  val parser = new HMMParser(msgParam, kernel)
  val trainingData = Reconstructor.reconstructTrainingData(kernelModel)
  val testData = Reconstructor.reconstructTestData(kernelModel)
  compModel.train(trainingData)

//  println(s"Training: $trainingData")
//  println(s"Test: $testData")

  def testSentence(testIdx: Int) = {
    val edges = kernelModel.edges
    val obsArr = kernelModel.testObservations
    val testArr = kernelModel.testLabels
    val (labelKeys, testMatrix) = kernelModel.testMatrix

    if (trainingData.length == 1) {
      println(labelKeys.mkString(","))
      kernelModel.printEdgeFeatures()
    }

    val observations = obsArr(testIdx).map{ case (key, sparse) => key -> sparse.toDenseMatrix}
    val testSet = testArr(testIdx).map{ case (key, sparse) => key -> sparse.toDenseMatrix}

    val cache = parser.buildCache(edges, observations.size, smooth=smooth)

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

  private def buildPasser(cache: Cache, observedNodes: Set[Int]) = new MessagePasser(cache, observedNodes, numIter)

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
