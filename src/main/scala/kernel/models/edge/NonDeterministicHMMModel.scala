package kernel.models.edge

import breeze.linalg.DenseMatrix
import kernel.models.toys.extractors.{SimpleHMMFeatureExtractor, ToyFeatureExtractor}

class NonDeterministicHMMModel(n: Int) extends DeterministicHMMModel(n) {
  override protected def hiddenStates = Seq("A", "B")

  override protected def visibleStates = Seq("X", "Y")

  override protected def transitionMatrix: DenseMatrix[Double] = {
    DenseMatrix(
      (0.0, 1.0),
      (0.95, 0.05)
    )
  }

  override protected def emissionMatrix: DenseMatrix[Double] = {
    DenseMatrix(
      (0.95, 0.05),
      (0.05, 0.95)
    )
  }

  override protected def extractor: ToyFeatureExtractor = new SimpleHMMFeatureExtractor()
}
