package kernel.propagation

import breeze.linalg.DenseMatrix
import kernel.caches.{LoopyCache, Cache}
import kernel.kernels.Kernel
import kernel.models.Model

class LoopyMessagePasser(model: Model, kernel: Kernel) {

  protected var cache: LoopyCache = _
  protected var betaArr: Array[DenseMatrix[Double]] = _

  def getCache = this.cache

  def passMessages(sampleArr: Array[DenseMatrix[Double]], observations: Map[Int, DenseMatrix[Double]]): Array[DenseMatrix[Double]] = {
    cache = LoopyCache.buildCache(sampleArr, kernel, model)
    betaArr = Array.ofDim[DenseMatrix[Double]](model.numNodes)

    calculateObservedMessages(observations)
    calculateInternalMessages(observations)

    betaArr
  }

  protected def calculateObservedMessages(observations: Map[Int, DenseMatrix[Double]]): Unit = {
    for ((leafId, idx) <- observations.keys.zipWithIndex) {
      val parentId = model.getParents(leafId)(0)

      val Kt = cache.kArr(leafId)(leafId)
      val Ks = cache.kArr(leafId)(parentId)
      val I = DenseMatrix.eye[Double](Kt.rows)

      val kt = kernel(cache.leafArr(leafId), observations(leafId), model.msgParam.sig)

      betaArr(leafId) = observedMessage(Kt, Ks, kt, I, model.msgParam.lambda)
    }
  }

  protected def calculateInternalMessages(observations: Map[Int, DenseMatrix[Double]]): Unit = {

  }
}
