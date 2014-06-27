package kernel.propagation

import breeze.linalg.DenseMatrix
import kernel.Cache
import kernel.kernels.Kernel
import kernel.models.Model

abstract class MessagePasser(model: Model, kernel: Kernel) {
  protected var cache: Cache = _
  protected var betaArr: Array[DenseMatrix[Double]] = _

  def getCache = this.cache

  def passMessages(sampleArr: Array[DenseMatrix[Double]], observations: Map[Int, DenseMatrix[Double]]): Array[DenseMatrix[Double]]
}
