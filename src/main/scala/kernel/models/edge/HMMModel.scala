package kernel.models.edge

import breeze.linalg.{DenseMatrix, DenseVector}
import kernel.models.toys.extractors.ToyFeatureExtractor

abstract class HMMModel extends EdgeModel {
  initialise()

  /**
   * Generate data for the model. The output format is an array where each position is the training data
   * relevant to a given node and each row of those matrices is a single sample.
   */
  protected def initialise() = {
    val (keyArray, trainData, testData) = generateFeatureVectors()

    _keyArray = keyArray
    _testObservations = getObservations(testData)
    _testLabels = getTestLabels(testData)

    _edges = Map[String, Edge]()

    _edges += ("hidden" -> buildTransitionEdges(trainData))
    _edges += ("visible" -> buildEmissionEdges(trainData))
  }

  protected def generateFeatureVectors(): (Array[String], Array[Array[DenseVector[Double]]], Array[Array[DenseVector[Double]]])

  final def getObservations(testData: Array[Array[DenseVector[Double]]]): Array[Map[Int, DenseMatrix[Double]]] = {
    testData.map(sample => {
      convertRangeToMap(sample, sample.length / 2, sample.length)
    })
  }

  final def getTestLabels(testData: Array[Array[DenseVector[Double]]]): Array[Map[Int, DenseMatrix[Double]]] = {
    testData.map(sample => {
      convertRangeToMap(sample, 0, sample.length / 2)
    })
  }

  private def convertRangeToMap(sample: Array[DenseVector[Double]], from: Int, until: Int) = {
    var map = Map[Int, DenseMatrix[Double]]()
    for (i <- from until until) {
      map += i -> sample(i).toDenseMatrix
    }
    map
  }

  private def buildTransitionEdges(data: Array[Array[DenseVector[Double]]]) = {
    var leftData = List[DenseMatrix[Double]]()
    var rightData = List[DenseMatrix[Double]]()

    data.foreach(sample => {
      val sentenceLength = sample.length / 2
      for (i <- 0 until sentenceLength) {
        leftData :+= sample(i).toDenseMatrix
        rightData :+= sample(i+1).toDenseMatrix
      }
    })

    Edge(mergeData(leftData), mergeData(rightData))
  }

  private def buildEmissionEdges(data: Array[Array[DenseVector[Double]]]) = {
    var leftData = List[DenseMatrix[Double]]()
    var rightData = List[DenseMatrix[Double]]()

    data.foreach(sample => {
      val sentenceLength = sample.length / 2
      for (i <- 0 until sentenceLength) {
        leftData :+= sample(i).toDenseMatrix
        rightData :+= sample(sentenceLength + i).toDenseMatrix
      }
    })

    Edge(mergeData(leftData), mergeData(rightData))
  }
}
