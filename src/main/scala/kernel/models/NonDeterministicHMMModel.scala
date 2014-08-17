package kernel.models

import breeze.linalg.DenseMatrix

class NonDeterministicHMMModel(n: Int, numTest: Int = 10) extends DeterministicHMMModel(n, numTest) {
  override protected def hiddenStates = Seq("A", "B")

  override protected def visibleStates = Seq("X", "Y")

  override protected def transitionMatrix: DenseMatrix[Double] = {
    val xx = 0.0
    val yy = 0.0
    DenseMatrix(
      (xx, 1 - xx),
      (1-yy, yy)
    )
  }

  override protected def emissionMatrix: DenseMatrix[Double] = {
    val ax = 0.8
    val bx = 0.2
    DenseMatrix(
      (ax, 1-ax),
      (bx, 1-bx)
    )
  }
}
