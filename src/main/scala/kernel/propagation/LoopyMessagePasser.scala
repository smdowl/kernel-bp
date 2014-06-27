package kernel.propagation

import breeze.linalg.{max, norm, DenseMatrix}
import kernel.caches.{LoopyCache, Cache}
import kernel.kernels.Kernel
import kernel.models.Model

class LoopyMessagePasser(model: Model, kernel: Kernel) {

  protected var cache: LoopyCache = _
  protected var betaArr: Array[Array[DenseMatrix[Double]]] = _

  def getCache = this.cache

  def passMessages(sampleArr: Array[DenseMatrix[Double]], observations: Map[Int, DenseMatrix[Double]]): Array[Array[DenseMatrix[Double]]] = {
    cache = LoopyCache.buildCache(sampleArr, kernel, model)
    betaArr = Array.ofDim[DenseMatrix[Double]](model.numNodes, model.numNodes)

    calculateObservedMessages(observations)
    calculateInternalMessages(observations)

    betaArr
  }

  protected def calculateObservedMessages(observations: Map[Int, DenseMatrix[Double]]): Unit = {
    for ((leafId, idx) <- observations.keys.zipWithIndex) {
      val neighbours = model.getNeighbours(leafId)

      neighbours.foreach(neighbourId => {
        val Kt = cache.kArr(leafId)
        val Ks = cache.kArr(neighbourId)
        val I = DenseMatrix.eye[Double](Kt.rows)

        val kt = kernel(observations(leafId), observations(leafId), model.msgParam.sig)

        betaArr(leafId)(neighbourId) = observedMessage(Kt, Ks, kt, I, model.msgParam.lambda)
        normMessage(leafId, neighbourId)
      })
    }
  }

  private def observedMessage(Kt: DenseMatrix[Double], Ks: DenseMatrix[Double],
                              kt: DenseMatrix[Double], I: DenseMatrix[Double], lambda: Double) = {
    // Have to split because type seems not to be infered otherwise
    val left: DenseMatrix[Double] = Kt :+ I * lambda
    val right: DenseMatrix[Double] = Ks + I * lambda
    val lr = left * right
    val sol = lr \ kt
    sol
  }

  private def normMessage(i: Int, j: Int) = {
    betaArr(i)(j) = betaArr(i)(j) / max(betaArr(i)(j))
  }

  protected def calculateInternalMessages(observations: Map[Int, DenseMatrix[Double]]): Unit = {

  }
}
