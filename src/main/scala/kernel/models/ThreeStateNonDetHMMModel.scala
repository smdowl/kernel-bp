package kernel.models

import breeze.linalg.DenseMatrix

class ThreeStateNonDetHMMModel(n: Int, numTest: Int = 10) extends DeterministicHMMModel(n) {
  override protected def hiddenStates = Seq("A", "B", "C")

  override protected def visibleStates = Seq("X", "Y", "Z")

  override protected def transitionMatrix: DenseMatrix[Double] = {
    val aa = 0.0
    val ab = 1.0

    val ba = 0.0
    val bb = 0.0

    val ca = 1.0
    val cb = 0.0

    DenseMatrix(
      (aa, ab, 1 - aa - ab),
      (ba, bb, 1 - ba - bb),
      (ca, cb, 1 - ca - cb)
    )
  }

  override protected def emissionMatrix: DenseMatrix[Double] = {
    val ax = 0.8
    val ay = 0.0

    val bx = 0.0
    val by = 0.8

    val cx = 0.0
    val cy = 0.05

    DenseMatrix(
      (ax, ay, 1-ax-ay),
      (bx, by, 1-bx-by),
      (cx, cy, 1-cx-cy)
    )
  }
}
