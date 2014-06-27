package kernel.propagation

import breeze.linalg.DenseMatrix
import kernel.Cache
import kernel.kernels.Kernel
import kernel.models.Model

abstract class MessagePasser(model: Model, kernel: Kernel) {
  protected var cache: Cache = _
  protected var betaArr: Array[DenseMatrix[Double]] = _

  def getCache = this.cache

  def passMessages(sampleArr: Array[DenseMatrix[Double]], observations: Map[Int, DenseMatrix[Double]]): Array[DenseMatrix[Double]] = {
    cache = Cache.buildCache(sampleArr, kernel, model)
    betaArr = Array.ofDim[DenseMatrix[Double]](model.numNodes)

    calculateObservedMessages(observations)
    calculateInternalMessages(observations)

    betaArr
  }

  protected def calculateObservedMessages(observations: Map[Int, DenseMatrix[Double]]): Unit
  protected def calculateInternalMessages(observations: Map[Int, DenseMatrix[Double]]): Unit
}
