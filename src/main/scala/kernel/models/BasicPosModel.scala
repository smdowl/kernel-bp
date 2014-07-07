package kernel.models

import breeze.linalg.DenseMatrix

class BasicPosModel(n: Int, length: Int) extends Model(n) {
  override val msgParam: MessageParam = MessageParam(0.1, 0.3)

  override def generateData(): Array[DenseMatrix[Double]] = ???

  override protected def _A: DenseMatrix[Int] = DenseMatrix.eye[Int](length)

  override val rootNode: Int = 0
}
