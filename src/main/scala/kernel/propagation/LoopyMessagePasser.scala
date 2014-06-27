package kernel.propagation

import breeze.linalg.DenseMatrix
import kernel.kernels.Kernel
import kernel.models.Model

class LoopyMessagePasser(model: Model, kernel: Kernel) extends MessagePasser(model, kernel) {
  override def passMessages(sampleArr: Array[DenseMatrix[Double]], observations: Map[Int, DenseMatrix[Double]]): Array[DenseMatrix[Double]] = {

  }
}
