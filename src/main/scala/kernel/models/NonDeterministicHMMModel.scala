package kernel.models

import breeze.linalg.DenseMatrix

class NonDeterministicHMMModel(n: Int, numTest: Int = 10) extends DeterministicHMMModel(n, numTest) {
  override protected def hiddenStates = Seq("A", "B")

  override protected def visibleStates = Seq("X", "Y")

  override protected def transitionMatrix: DenseMatrix[Double] = {
    val xx = 0.8
    val yy = 0.2
    DenseMatrix(
      (xx, 1.0 - xx),
      (1.0-yy, yy)
    )
  }

  override protected def emissionMatrix: DenseMatrix[Double] = {
    val ax = 0.9
    val bx = 0.1
    DenseMatrix(
      (ax, 1.0-ax),
      (bx, 1.0-bx)
    )
  }
}
