package kernel.models

import breeze.linalg.DenseMatrix

class NonDeterministicHMMModel(n: Int, numTest: Int = 10) extends DeterministicHMMModel(n, numTest) {
  override protected def hiddenStates = Seq("A", "B", "C")

  override protected def visibleStates = Seq("X", "Y", "Z")

  override protected def transitionMatrix: DenseMatrix[Double] = {
    val aa = 0.0
    val ab = 0.8

    val ba = 0.2
    val bb = 0.0

    val ca = 0.79
    val cb = 0.2

    DenseMatrix(
      (aa, ab, 1.0 - aa - ab),
      (ba, bb, 1.0 - ba - bb),
      (ca, cb, 1.0 - ca - cb)
    )
  }

  override protected def emissionMatrix: DenseMatrix[Double] = {
    val ax = 0.7
    val ay = 0.0

    val bx = 0.0
    val by = 0.8

    val cx = 0.0
    val cy = 0.1

    DenseMatrix(
      (ax, ay, 1.0-ax-ay),
      (bx, by, 1.0-bx-by),
      (cx, cy, 1.0-cx-cy)
    )
  }
}
