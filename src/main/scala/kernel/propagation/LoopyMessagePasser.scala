package kernel.propagation

import breeze.linalg.{inv, max, norm, DenseMatrix}
import breeze.numerics.sqrt
import kernel.caches.{LoopyCache, Cache}
import kernel.kernels.Kernel
import kernel.models.Model

class LoopyMessagePasser(model: Model, kernel: Kernel) {

  private val numIter = 100

  protected var cache: LoopyCache = _
  protected var betaArr: Array[Array[DenseMatrix[Double]]] = _
  protected var KarrInv: Array[DenseMatrix[Double]] = _
  protected var observations: Map[Int, DenseMatrix[Double]] = _

  def getCache = this.cache

  def passMessages(sampleArr: Array[DenseMatrix[Double]], observations: Map[Int, DenseMatrix[Double]]): Array[Array[DenseMatrix[Double]]] = {
    this.observations = observations
    cache = LoopyCache.buildCache(sampleArr, kernel, model)
    betaArr = Array.ofDim[DenseMatrix[Double]](model.numNodes, model.numNodes)
    KarrInv = calculateInverses()

    initBetas()

    calculateObservedMessages()
    calculateInternalMessages()

    betaArr
  }

  def calculateInverses() = {
    val out = Array.ofDim[DenseMatrix[Double]](model.numNodes)

    for (nodeId <- unobservedNodes)
      out(nodeId) = inv( cache.kArr(nodeId) + DenseMatrix.eye[Double](model.n) * model.msgParam.lambda )

    out
  }

  def unobservedNodes = (0 until model.numNodes).filterNot(observations.keySet.contains)

  protected def calculateObservedMessages(): Unit = {
    for ((leafId, idx) <- observations.keys.zipWithIndex) {
      val neighbours = model.getNeighbours(leafId)

      neighbours.foreach(neighbourId => {
        val Kt = cache.kArr(leafId)
        val Ks = cache.kArr(neighbourId)
        val I = DenseMatrix.eye[Double](Kt.rows)

        val kt = cache.kArr(neighbourId)

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

  private def initBetas() = {
    unobservedNodes.foreach(nodeId => {
      val neighbours = model.getNeighbours(nodeId)
      neighbours.foreach(neighbourId => {
        betaArr(nodeId)(neighbourId) = DenseMatrix.ones[Double](model.n, 1) / sqrt(model.n)
      })
    })
  }

  protected def calculateInternalMessages(): Unit = {
    for(i <- 0 until numIter) {
      val nodes = (0 until model.numNodes).filterNot(observations.keySet.contains)

      for (nodeIdx <- nodes) {
        val neighbours = model.getNeighbours(nodeIdx)
        val nodesToUpdate = neighbours.filterNot(observations.keySet.contains).toSet

        for (outMessageIdx <- nodesToUpdate) {
          val Ktu_beta = DenseMatrix.ones[Double](model.n, 1)

          for (inMessageIdx <- nodesToUpdate - outMessageIdx)
            Ktu_beta :*= (cache.kArr(nodeIdx) * betaArr(inMessageIdx)(nodeIdx))

          betaArr(nodeIdx)(outMessageIdx) = KarrInv(outMessageIdx) * Ktu_beta
          normMessage(nodeIdx, outMessageIdx)
        }
      }

    }
  }
}
