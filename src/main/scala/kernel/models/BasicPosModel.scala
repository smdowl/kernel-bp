package kernel.models

import app.Constants
import breeze.linalg.{DenseVector, DenseMatrix}
import input.ConllParser
import pos.features.extractors.BasicFeatureExtractor
import pos.output.ParsedFeaturesOutput

object BasicPosModel extends App {
  val model = new BasicPosModel(8, 26)
  val data = model.generateData()
  println(data)
}

class BasicPosModel(n: Int, length: Int) extends ChainModel(n, length) {
  override val msgParam: MessageParam = MessageParam(0.1, 0.3)

  var featureKeys: Array[String] = _

  override def generateData(): Array[DenseMatrix[Double]] = {
    val parser = new ConllParser()
    val extractor = new BasicFeatureExtractor
    val (tempFeatureKeys, featureArrays) = ParsedFeaturesOutput(parser, extractor, Constants.MINI_TRAIN_FILE)
    featureKeys = tempFeatureKeys

    // Ensure all the arrays fit with this model size
    val filteredArrays = featureArrays.filter(_.length == length)
    assert(filteredArrays.length > n, s"Not enough sentences for requested sample size. Wanted $length but only ${filteredArrays.length} available")

    convertToDataMatrices(filteredArrays.toArray)
  }

  private def convertToDataMatrices(featureArrays: Array[Array[DenseVector[Double]]]) = {
    val output = Array.ofDim[DenseMatrix[Double]](length)
    for (i <- 0 until output.length)
      output(i) = DenseMatrix.zeros[Double](n, d)

    for (sample <- 0 until n)
      for (i <- 0 until length)
        output(i)(sample, ::) := featureArrays(sample)(i).t

    output
  }

  def d = featureKeys.size
}
