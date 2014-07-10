package kernel.models

import app.Constants
import breeze.linalg.{DenseMatrix, any, DenseVector}
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

  private var testData: Array[DenseMatrix[Double]] = _
  private var testLabels: Array[Array[String]] = _
  private var featureKeys: Array[String] = _
  private var _labelKeys: Set[String] = _
  private var keyIndex: Map[String, Int] = _

  override def generateData(): Array[DenseMatrix[Double]] = {
    val (featureKeys, featureArrays, testFeatureArrays) = ParsedFeaturesOutput(parser, extractor, length, Constants.MINI_TRAIN_FILE, Constants.MINI_TEST_FILE)
    this.featureKeys = featureKeys
    _labelKeys = getOnlyLabelKeys(featureKeys)
    keyIndex = buildKeyIndex(featureKeys)

    // TODO: Convert to sparse
    testData = parseTestData(testFeatureArrays)
    testLabels = parseAndRemoveLabels(testData)

    parseTraining(featureArrays)
  }

  private def getOnlyLabelKeys(keys: Iterable[String]) = {
    keys.filter(_.startsWith("label:")).toSet
  }

  private def buildKeyIndex(featureKeys: Array[String]) = {
    featureKeys.zipWithIndex.foldLeft(Map[String, Int]()) {
      case (index, (key, idx)) =>
        index + (key -> idx)
    }
  }

  private def parseTestData(featureArrays: Array[Array[DenseVector[Double]]]) = {
    val filteredArrays = featureArrays.filter(_.length == length)
    assert(filteredArrays.length > 0, s"Need at least one test example.")

    convertToDataMatrices(filteredArrays.toArray)
  }

  private def parseAndRemoveLabels(data: Array[DenseMatrix[Double]]) = {
    val labels = Array.ofDim[Array[String]](data.length)
    for (i <- 0 until data.length) {
      val matrix = data(i)
      labels(i) = Array.ofDim[String](matrix.rows)

      for (j <- 0 until matrix.rows) {
        val row = matrix(j, ::).t
        val label = getLabel(row)
        matrix(j, keyIndex(label)) = 0.0
        labels(i)(j) = label
      }
    }
    labels
  }

  private def getLabel(vec: DenseVector[Double]) = {
    val keys = labelKeys.filter(labelKey => {
      vec(keyIndex(labelKey)) != 0.0
    })

    assert(keys.size == 1, "Should only be one label.")
    keys.head
  }

  private def parseTraining(featureArrays: Array[Array[DenseVector[Double]]]) = {
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

  override def labelKeys: Set[String] = _labelKeys

  override def getIndexForKey(key: String): Int = keyIndex(key)

  override def getTestLabels: Array[Array[Double]] = testLabels
}
