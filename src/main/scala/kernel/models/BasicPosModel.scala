package kernel.models

import app.Constants
import breeze.linalg.{any, DenseVector, DenseMatrix}
import input.ConllParser
import pos.features.extractors.BasicFeatureExtractor
import pos.output.ParsedFeaturesOutput

object BasicPosModel extends App {
  val model = new BasicPosModel(8, 26)
  val data = model.generateData()
  println(data)
}

class BasicPosModel(n: Int, length: Int) extends PosModel(n, length) with ParsedModel {

  val parser = new ConllParser()
  val extractor = new BasicFeatureExtractor

  override val msgParam: MessageParam = MessageParam(0.1, 0.3)

  var testData: Array[DenseMatrix[Double]] = _
  var featureKeys: Array[String] = _

  override def generateData(): Array[DenseMatrix[Double]] = {
    val (tempFeatureKeys, featureArrays, testFeatureArrays) = ParsedFeaturesOutput(parser, extractor, length, Constants.MINI_TRAIN_FILE, Constants.MINI_TEST_FILE)
    featureKeys = tempFeatureKeys

    testData = parseTest(testFeatureArrays)
    parseTraining(featureArrays)
  }

  def parseTest(featureArrays: Array[Array[DenseVector[Double]]]) = {
    val filteredArrays = featureArrays.filter(_.length == length)
    assert(filteredArrays.length > 0, s"Need at least one test example.")

    convertToDataMatrices(filteredArrays.toArray)
  }

  def parseTraining(featureArrays: Array[Array[DenseVector[Double]]]) = {
    // Ensure all the arrays fit with this model size
    val filteredArrays = featureArrays.filter(_.length == length)
    assert(filteredArrays.length >= n, s"Not enough sentences for requested sample size. Wanted $n but only ${filteredArrays.length} available")

    convertToDataMatrices(filteredArrays.toArray)
  }


  private def convertToDataMatrices(featureArrays: Array[Array[DenseVector[Double]]]) = {
    val output = Array.ofDim[DenseMatrix[Double]](this.length)
    val numSamples = featureArrays.length

    for (i <- 0 until output.length)
      output(i) = DenseMatrix.zeros[Double](numSamples, d)

    for (sample <- 0 until numSamples)
      for (i <- 0 until output.length) {
        output(i)(sample, ::) := featureArrays(sample)(i).t

        if (i > 0)
          assert(any(output(i)(sample, ::).t), "Should still have some non-zero entries")
      }

    output
  }


  override def generateTestData(): Array[DenseMatrix[Double]] = {
    testData
  }

  def d = featureKeys.size
}
