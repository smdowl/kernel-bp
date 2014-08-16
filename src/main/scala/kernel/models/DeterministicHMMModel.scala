package kernel.models

import breeze.linalg.DenseMatrix
import kernel.models.toyextractors.{BigramFeatureExtractor, UnigramFeatureExtractor, ToyFeatureExtractor}

object DeterministicHMMModel extends App {
  val model = new DeterministicHMMModel(1)
  val edges = model.edges
}

class DeterministicHMMModel(n: Int, numTest: Int = 10) extends ProbabalisticHMMModel(n) {
  protected def hiddenStates = Seq("A", "B", "C")
  protected def visibleStates = Seq("X", "Y", "Z")

  protected def transitionMatrix: DenseMatrix[Double] = {
    DenseMatrix(
      (0.0, 1.0, 0.0),
      (0.0, 0.0, 1.0),
      (1.0, 0.0, 0.0)
    )
  }

  protected def emissionMatrix: DenseMatrix[Double] = {
    DenseMatrix(
      (1.0, 0.0, 0.0),
      (0.0, 1.0, 0.0),
      (0.0, 0.0, 1.0)
    )
  }

  protected def extractor: ToyFeatureExtractor = new BigramFeatureExtractor()

  /**
   * Draw a single sample of hidden and visible states
   */
  protected def makeSequence() = {

    val length = sampleLength()

    val hiddenSample = (0 until length - 1).foldLeft(Seq[Int](initDist.draw()))((b, _) => {
      b :+ transitionDists(b.last).draw()
    })

    val visibleSample: Seq[Int] = hiddenSample.map(hiddenState => {
      emissionDists(hiddenState).draw()
    })

    (hiddenSample, visibleSample)
  }
}
