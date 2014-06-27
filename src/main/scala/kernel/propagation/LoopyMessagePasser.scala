package kernel.propagation

import breeze.linalg.DenseMatrix
import kernel.kernels.Kernel
import kernel.models.Model

class LoopyMessagePasser(model: Model, kernel: Kernel) extends MessagePasser(model, kernel) {
  override protected def calculateObservedMessages(observations: Map[Int, DenseMatrix[Double]]): Unit = {

  }

  override protected def calculateInternalMessages(observations: Map[Int, DenseMatrix[Double]]): Unit = {
    
  }
}
