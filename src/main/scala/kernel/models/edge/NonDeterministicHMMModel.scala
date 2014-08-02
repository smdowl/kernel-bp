package kernel.models.edge

import breeze.linalg.DenseMatrix
import kernel.models.toys.extractors.{SimpleHMMFeatureExtractor, ToyFeatureExtractor}

class NonDeterministicHMMModel(n: Int) extends DeterministicHMMModel(n) {
  override protected def hiddenStates = Seq("A", "B", "C")

  override protected def visibleStates = Seq("X", "Y", "Z")

  override protected def transitionMatrix: DenseMatrix[Double] = {
    DenseMatrix(
      (0.05, 0.9, 0.05),
      (0.1, 0.0, 0.9),
      (0.9, 0.1, 0.0)
    )
  }

  override protected def emissionMatrix: DenseMatrix[Double] = {
    DenseMatrix(
      (1.0, 0.0, 0.0),
      (0.0, 1.0, 0.0),
      (0.0, 0.0, 1.0)
    )
  }

  override protected def extractor: ToyFeatureExtractor = new SimpleHMMFeatureExtractor()
}
