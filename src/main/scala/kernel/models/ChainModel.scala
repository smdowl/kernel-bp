package kernel.models

import breeze.linalg.DenseMatrix

abstract class ChainModel(n: Int, length: Int) extends Model(n) {
  override val rootNode: Int = 0
  private var cachedA: DenseMatrix[Int] = _

  override protected def _A: DenseMatrix[Int] = {
    if (cachedA == null) {
      cachedA = DenseMatrix.zeros[Int](length, length)
      for (i <- 1 until length) {
        cachedA(i-1, i) = 1
      }
    }

    cachedA
  }
}
