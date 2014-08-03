package kernel.models.edge

import breeze.linalg.{DenseMatrix, DenseVector}
import breeze.stats.distributions.Multinomial
import kernel.models.toys.extractors.{SimpleHMMFeatureExtractor, ToyFeatureExtractor}
import pos.features.extractors.FeatureArrayBuilder

import scala.util.Random

abstract class HMMModel(n: Int) extends EdgeModel {
  private val minLength = 10
  private val maxLength = minLength

  protected def hiddenStates: Seq[String]
  protected def visibleStates: Seq[String]

  protected val initDist = new Multinomial(DenseVector.ones[Double](hiddenStates.length) * (1.0 / hiddenStates.length))

  protected def transitionMatrix: DenseMatrix[Double]
  protected def emissionMatrix: DenseMatrix[Double]

  protected def transitionDists = {
    (0 until transitionMatrix.rows).map(i => {
      new Multinomial(transitionMatrix(i, ::).t)
    })
  }

  protected def emissionDists = {
    (0 until emissionMatrix.rows).map(i => {
      new Multinomial(emissionMatrix(i, ::).t)
    })
  }

  protected def extractor: ToyFeatureExtractor

  initialise()

  /**
   * Generate data for the model. The output format is an array where each position is the training data
   * relevant to a given node and each row of those matrices is a single sample.
   */
  protected def initialise() = {
    assert(transitionMatrix.rows == hiddenStates.length)
    assert(emissionMatrix.rows == visibleStates.length)

    val sampleSequences = for (i <- 0 until n) yield drawSample()
    val testSequences = for (i <- 0 until n) yield drawSample()

    val (keyArray, trainData, testData) = FeatureArrayBuilder.buildFeatureArray(sampleSequences, testSequences)

    _keyArray = keyArray
    _testObservations = getObservations(testData)
    _testLabels = getTestLabels(testData)

    _edges = Map[String, Edge]()

    _edges += ("hidden" -> buildTransitionEdges(trainData))
    _edges += ("visible" -> buildEmissionEdges(trainData))
  }

  /**
   * Draw a single sample of hidden and visible states
   */
  private def drawSample() = {
    val (hiddenSample, visibleSample) = makeSequence()
    extractor.extractFeatures(hiddenSample.map(hiddenStates.apply) ++ visibleSample.map(visibleStates.apply))
  }

  protected def makeSequence(): (Seq[Int], Seq[Int])

  protected def sampleLength() = {
    val sampleRange = maxLength - minLength
    val upper = if (sampleRange > 0) Random.nextInt(maxLength - minLength) else 0
    minLength + upper
  }

  private def getObservations(testData: Array[Array[DenseVector[Double]]]): Array[Map[Int, DenseMatrix[Double]]] = {
    testData.map(sample => {
      convertRangeToMap(sample, sample.length / 2, sample.length)
    })
  }

  private def getTestLabels(testData: Array[Array[DenseVector[Double]]]): Array[Map[Int, DenseMatrix[Double]]] = {
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

  private def mergeData(dataSeq: Seq[DenseMatrix[Double]]) = {
    if (dataSeq.length > 0)
      dataSeq.tail.foldLeft(dataSeq.head)((a, b) => DenseMatrix.vertcat(a, b)) + 1e-9
    else
      DenseMatrix.zeros[Double](0, 0)
  }
}
