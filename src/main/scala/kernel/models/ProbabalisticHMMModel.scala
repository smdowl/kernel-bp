package kernel.models

import breeze.linalg.{DenseMatrix, DenseVector, SparseVector}
import breeze.stats.distributions.Multinomial
import computation.FeatureVector
import kernel.models.toyextractors.{UnigramFeatureExtractor, ToyFeatureExtractor}
import pos.features.extractors.FeatureArrayBuilder

import scala.util.Random

abstract class ProbabalisticHMMModel(n: Int, numTest: Int = 10) extends HMMModel {
  var minLength = 20
  protected def maxLength = minLength

  protected def hiddenStates: Seq[String]
  protected def visibleStates: Seq[String]

  protected var _extractor: ToyFeatureExtractor = new UnigramFeatureExtractor()
  final protected def extractor: ToyFeatureExtractor = _extractor
  def setExtractor(extractor: ToyFeatureExtractor) = {
    _extractor = extractor
  }

  protected def initDist = new Multinomial(DenseVector.ones[Double](hiddenStates.length) * (1.0 / hiddenStates.length))

  def getTransitionMatrix: DenseMatrix[Double] = transitionMatrix
  def getEmissionMatrix: DenseMatrix[Double] = emissionMatrix

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

  override def initialise() = {
    assert(transitionMatrix.rows == hiddenStates.length)
    assert(emissionMatrix.rows == visibleStates.length)

    super.initialise()
  }


  override protected def generateFeatureVectors(): (Array[String], Array[Array[SparseVector[Double]]], Array[Array[SparseVector[Double]]]) = {
    val training = sampleSequences
    val test = testSequences
    FeatureArrayBuilder.buildFeatureArray(training, test)
  }

  protected def sampleSequences: Seq[Seq[FeatureVector]] = for (i <- 0 until n) yield drawSample()
  protected def testSequences: Seq[Seq[FeatureVector]] = sampleSequences.slice(0, numTest)

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

}
