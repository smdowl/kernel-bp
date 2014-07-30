package kernel.models.edge

import breeze.linalg.{DenseMatrix, DenseVector}
import breeze.stats.distributions.Multinomial
import kernel.models.toys.extractors.{SimpleHMMFeatureExtractor, ToyFeatureExtractor}
import pos.features.extractors.FeatureArrayBuilder

import scala.util.Random

object HMMModel extends App {
  val model = new HMMModel(1)
  val edges = model.edges
}

class HMMModel(n: Int) extends EdgeModel {
  private val minLength = 15
  private val maxLength = 30

  private val hiddenStates = Seq("A", "B", "C")
  private val visibleStates = Seq("X", "Y", "Z")

  private val initDist = new Multinomial(DenseVector.ones[Double](3) * (1.0 / 3))

  private val transitionDists = Seq(
    new Multinomial(DenseVector(0.0, 1.0, 0.0)),
    new Multinomial(DenseVector(0.0, 0.0, 1.0)),
    new Multinomial(DenseVector(1.0, 0.0, 0.0))
  )

  private val emissionDists = Seq(
    new Multinomial(DenseVector(1.0, 0.0, 0.0)),
    new Multinomial(DenseVector(0.0, 1.0, 0.0)),
    new Multinomial(DenseVector(0.0, 0.0, 1.0))
  )

  private val extractor: ToyFeatureExtractor = new SimpleHMMFeatureExtractor()

  private var _edges: Map[String, Edge] = _
  private var _testObservations: Array[Map[Int, DenseMatrix[Double]]] = _
  private var _testLabels: Array[Map[Int, DenseMatrix[Double]]] = _
  private var _keyArray: Array[String] = _

  override def edges: Map[String, Edge] = _edges
  override def testObservations: Array[Map[Int, DenseMatrix[Double]]] = _testObservations
  override def testLabels: Array[Map[Int, DenseMatrix[Double]]] = _testLabels
  override def keyArray: Array[String] = _keyArray

  initialise()

  /**
   * Generate data for the model. The output format is an array where each position is the training data
   * relevant to a given node and each row of those matrices is a single sample.
   */
  private def initialise() = {
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
    val length = minLength + Random.nextInt(maxLength - minLength)

    val hiddenSample = (0 until length).foldLeft(Seq[Int](initDist.draw()))((b, _) => {
      b :+ transitionDists(b.last).draw()
    })

    val visibleSample: Seq[Int] = hiddenSample.map(hiddenState => {
      emissionDists(hiddenState).draw()
    })

    extractor.extractFeatures(hiddenSample.map(hiddenStates.apply) ++ visibleSample.map(visibleStates.apply))
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
      for (i <- 0 until sentenceLength - 1) {
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
      for (i <- 0 until sentenceLength - 1) {
        leftData :+= sample(i).toDenseMatrix
        rightData :+= sample(sentenceLength + i).toDenseMatrix
      }
    })

    Edge(mergeData(leftData), mergeData(rightData))
  }

  private def mergeData(dataSeq: Seq[DenseMatrix[Double]]) = dataSeq.tail.foldLeft(dataSeq.head)((a, b) => DenseMatrix.vertcat(a, b)) + 1e-12
}
