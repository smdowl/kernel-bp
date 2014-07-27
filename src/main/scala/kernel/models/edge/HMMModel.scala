package kernel.models.edge

import breeze.linalg.{DenseMatrix, DenseVector}
import breeze.stats.distributions.Multinomial
import kernel.models.toys.extractors.{SimpleFeatureExtractor, ToyFeatureExtractor}
import pos.features.extractors.FeatureArrayBuilder

class HMMModel(n: Int) extends EdgeModel {
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

  private val extractor: ToyFeatureExtractor = new SimpleFeatureExtractor()

  override def generateEdges(): Map[String, Edge] = ???


  /**
   * Generate data for the model. The output format is an array where each position is the training data
   * relevant to a given node and each row of those matrices is a single sample.
   */
  private def generateData(): Array[DenseMatrix[Double]] = {
    val sampleSequences = for (i <- 0 until n) yield drawSample()
    val testSequences = for (i <- 0 until n) yield drawSample()

    val (keyArray, trainData, testData) = FeatureArrayBuilder.buildFeatureArray(sampleSequences, testSequences)
    trainData
    null
  }

  private def drawSample() = {
    val hiddenSample = (0 until length).foldLeft(Seq[Int](initDist.draw()))((b, _) => {
      b :+ transitionDists(b.last).draw()
    })

    val visibleSample: Seq[Int] = hiddenSample.map(hiddenState => {
      emissionDists(hiddenState).draw()
    })

    extractor.extractFeatures(hiddenSample.map(hiddenStates.apply) ++ visibleSample.map(visibleStates.apply))
  }
}
