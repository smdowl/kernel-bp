package kernel.models

import breeze.linalg.DenseMatrix
import kernel.models.toyextractors.{UnigramFeatureExtractor, ToyFeatureExtractor}

class NonDeterministicHMMModel(n: Int, numTest: Int = 10) extends DeterministicHMMModel(n) {
  override protected def hiddenStates = Seq("A", "B")

  override protected def visibleStates = Seq("X", "Y")

  override protected def transitionMatrix: DenseMatrix[Double] = {
    val xx = 0.0
    val yy = 0.1
    DenseMatrix(
      (xx, 1 - xx),
      (1-yy, yy)
    )
  }

  override protected def emissionMatrix: DenseMatrix[Double] = {
    val ax = 1.0
    val bx = 0.0
    DenseMatrix(
      (ax, 1-ax),
      (bx, 1-bx)
    )
  }

  override protected def extractor: ToyFeatureExtractor = new UnigramFeatureExtractor()
}
