package kernel.models

import breeze.linalg.{CSCMatrix, DenseMatrix, SparseVector}
import kernel.models.components.Edge

abstract class HMMModel extends Model {
  /**
   * Generate data for the model. The output format is an array where each position is the training data
   * relevant to a given node and each row of those matrices is a single sample.
   */
  def initialise() = {
    val (keyArray, trainData, testData) = generateFeatureVectors()
    this.trainData = trainData
    _keyArray = keyArray
    _testObservations = getObservations(testData)
    _testLabels = getTestLabels(testData)

    _edges = Map[String, Edge]()

    _edges += ("hidden" -> buildTransitionEdges(trainData))
    _edges += ("visible" -> buildEmissionEdges(trainData))
  }

  protected def generateFeatureVectors(): (Array[String], Array[Array[SparseVector[Double]]], Array[Array[SparseVector[Double]]])

  final def getObservations(testData: Array[Array[SparseVector[Double]]]): Array[Map[Int, CSCMatrix[Double]]] = {
    testData.map(sample => {
      convertRangeToMap(sample, sample.length / 2, sample.length)
    })
  }

  final def getTestLabels(testData: Array[Array[SparseVector[Double]]]): Array[Map[Int, CSCMatrix[Double]]] = {
    testData.map(sample => {
      convertRangeToMap(sample, 0, sample.length / 2)
    })
  }

  private def convertRangeToMap(sample: Array[SparseVector[Double]], from: Int, until: Int) = {
    var map = Map[Int, CSCMatrix[Double]]()
    for (i <- from until until) {
      val matrix = CSCMatrix.zeros[Double](1, sample(i).length)
      for (j <- 0 until sample(i).length)
        matrix(0, j) = sample(i)(j)
      map += i -> matrix
    }
    map
  }

  private def buildTransitionEdges(data: Array[Array[SparseVector[Double]]]) = {
    var leftData = List[DenseMatrix[Double]]()
    var rightData = List[DenseMatrix[Double]]()

    data.foreach(sample => {
      val sentenceLength = sample.length / 2
      for (i <- 0 until sentenceLength) {
        leftData :+= sample(i).toDenseVector.toDenseMatrix
        rightData :+= sample(i+1).toDenseVector.toDenseMatrix
      }
    })

    Edge(mergeData(leftData), mergeData(rightData))
  }

  private def buildEmissionEdges(data: Array[Array[SparseVector[Double]]]) = {
    var leftData = List[DenseMatrix[Double]]()
    var rightData = List[DenseMatrix[Double]]()

    data.foreach(sample => {
      val sentenceLength = sample.length / 2
      for (i <- 0 until sentenceLength) {
        leftData :+= sample(i).toDenseVector.toDenseMatrix
        rightData :+= sample(sentenceLength + i).toDenseVector.toDenseMatrix
      }
    })

    Edge(mergeData(leftData), mergeData(rightData))
  }
}
