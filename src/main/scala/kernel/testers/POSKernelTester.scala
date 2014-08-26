package kernel.testers

import breeze.linalg.{any, BitVector, DenseVector, DenseMatrix}
import comparison.MarkovModel
import kernel.caches.Cache
import kernel.kernels._
import kernel.models.components.{MessageParam, Inferer}
import kernel.models._
import kernel.parsing.HMMParser
import kernel.propagation.MessagePasser
import pos.features.extractors._

object RealConfig {

//  val extractors = Seq(new UnigramChainOnlyPOSFeatureExtractor, new UnigramPOSFeatureExtractor, new UnigramWordVecFeatureExtractor, new BigramChainOnlyPOSFeatureExtractor, new BigramPOSFeatureExtractor, new BigramWordVecFeatureExtractor)
  val extractors = Seq(new TrigramChainOnlyPOSFeatureExtractor, new TrigramPOSFeatureExtractor, new TrigramWordVecFeatureExtractor)

  val models = extractors.map(new RealPOSModel(_))
  val kernels = Seq(new LinearKernel)
  val smooth = Seq(false)
  val numIter = Seq(10)
  val lambda: Seq[Double] = Seq(1e3)
}

object POSKernelTester extends App {

  runExperiments()

  def runExperiments() = {
    var model: RealPOSModel = null
    var first = true

    for (nextModel <- RealConfig.models) {
      model = nextModel
      model.initialise()

      if (first) {
        println(s"${model.getNumberOfSamples} training samples")
        first = false
      }

      println(s"${getName(model)} with ${getName(model.getExtractor)}")
      println()

      for (numIter <- RealConfig.numIter) {
        for (kernel <- RealConfig.kernels) {
          for (smooth <- RealConfig.smooth) {
            for (lambda <- RealConfig.lambda) {
              val smoothString = if (smooth) " (smoothed)" else ""

              println(s"${getName(kernel)}$smoothString with $numIter iterations lambda=$lambda")

              var kernelAccuracy, viterbiAccuracy, forwardBackwardAccuracy = 0.0

              val results = runTest(model, kernel, smooth, numIter, lambda)

              kernelAccuracy += results._1
              viterbiAccuracy += results._2
              forwardBackwardAccuracy += results._3

              println(s"Kernel Accuracy: $kernelAccuracy\nViterbi Accuracy: $viterbiAccuracy\nForward-Backward Accuracy: $forwardBackwardAccuracy")
              println()

            }
          }
        }

        println()
      }
    }
  }

  def getName(obj: Object): String = {
    obj.getClass.getSimpleName
  }

  protected def runTest(model: Model, kernel: Kernel, smooth: Boolean, numIter: Int, lambda: Double) = {
    val viterbiModel = new MarkovModel()
    val tester = new POSKernelTester(model, viterbiModel, kernel, smooth, numIter, lambda)

    var kernelAccuracy = 0.0
    var viterbiAccuracy = 0.0
    var forwardBackwardAccuracy = 0.0
    var total = 0
    for (i <- 0 until model.testObservations.length) {
      val (kernelResults, viterbiResults, fbResults) = tester.testSentence(i)

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

class POSKernelTester(kernelModel: Model, compModel: MarkovModel, kernel: Kernel, smooth: Boolean, numIter: Int, lambda: Double, 
                      shouldCompare: Boolean = false) {
  val msgParam: MessageParam = MessageParam(lambda, 1.0)
  val parser = new HMMParser(msgParam, kernel)

  var trainingData: Seq[Seq[(String, String)]] = _
  var testData: Seq[Seq[(String, String)]] = _

  if (shouldCompare) {
    trainingData = Reconstructor.reconstructTrainingData(kernelModel)
    testData = Reconstructor.reconstructTestData(kernelModel)
    compModel.train(trainingData)
  }

//    println(s"Training: $trainingData")
//    println(s"Test: $testData")

  def testSentence(testIdx: Int) = {
    val edges = kernelModel.edges
    val obsArr = kernelModel.testObservations
    val testArr = kernelModel.testLabels
    val (labelKeys, testMatrix) = kernelModel.testMatrix

//    println(labelKeys.mkString(","))

    if (trainingData != null && trainingData.length == 1)
      kernelModel.printEdgeFeatures()

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

    val (viterbiResults, fbResults) = if (shouldCompare) {
      (compModel.viterbiTestSentence(testData(testIdx)), compModel.forwardBackwardTestSentence(testData(testIdx)))
    } else {
      val n = kernelResults.length
      (Seq.fill(n)(false), Seq.fill(n)(false))
    }

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
