package kernel.models.toys

import breeze.linalg.{DenseVector, DenseMatrix}
import breeze.stats.distributions.Multinomial
import kernel.models.toys.extractors.{SimpleHMMFeatureExtractor, ToyFeatureExtractor}
import kernel.models.{HMMModel, ParsedModel, MessageParam}
import pos.features.extractors.FeatureArrayBuilder

object DeterministicHMMModel extends App {
  val model = new DeterministicHMMModel(10, 3)
  val data = model.generateData()
  println(model.sharedSets)
}

class DeterministicHMMModel(n: Int, length: Int) extends HMMModel(n, length) with ParsedModel {

  override val msgParam: MessageParam = MessageParam(0.1, 0.3)

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

  /**
   * Generate data for the model. The output format is an array where each position is the training data
   * relevant to a given node and each row of those matrices is a single sample.
   */
  override def generateData(): Array[DenseMatrix[Double]] = {
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

  override def generateTestData(): Array[DenseMatrix[Double]] = ???

  override def getTestLabels: Array[Array[String]] = ???

}

