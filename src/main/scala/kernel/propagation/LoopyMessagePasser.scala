package kernel.propagation

import breeze.linalg.{inv, max, norm, DenseMatrix}
import breeze.numerics.{abs, sqrt}
import kernel.caches.{LoopyCache, Cache}
import kernel.kernels.Kernel
import kernel.models.Model

class LoopyMessagePasser(model: Model, kernel: Kernel) {

  private val numIter = 100

  protected var cache: LoopyCache = _
  protected var betaArr: Array[Array[DenseMatrix[Double]]] = _
  protected var KarrInv: Array[DenseMatrix[Double]] = _
  protected var sampleArr: Array[DenseMatrix[Double]] = _
  protected var observations: Map[Int, DenseMatrix[Double]] = _

  def getCache = this.cache

  def passMessages(sampleArr: Array[DenseMatrix[Double]], observations: Map[Int, DenseMatrix[Double]]): Array[Array[DenseMatrix[Double]]] = {
    this.sampleArr = sampleArr
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
    for (leafId <- observations.keys) {
      val neighbours = model.getNeighbours(leafId)

      neighbours.foreach(neighbourId => {
        val Kt = cache.kArr(leafId)
        val Ks = cache.kArr(neighbourId)
        val I = DenseMatrix.eye[Double](Kt.rows)

        val kt = kernel(sampleArr(leafId), observations(leafId), model.msgParam.sig)

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
    // TODO: Make sure this doesn't do something too crazy in higher dimensions
    val matNorm = norm(betaArr(i)(j).toDenseVector)
    val maxNorm = abs(max(betaArr(i)(j)))
    betaArr(i)(j) /= matNorm
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
      println(s"Starting iteration $i")
      propagate()
    }
  }

  private def propagate() = {
    val nodes = unobservedNodes

    for (nodeIdx <- nodes)
      updateMessageForNode(nodeIdx)
  }

  private def updateMessageForNode(nodeId: Int) = {
    val neighbours = model.getNeighbours(nodeId)
    val nodesToUpdate = neighbours.filterNot(observations.keySet.contains)

    for (outMessageIdx <- nodesToUpdate) {
      val Ktu_beta = DenseMatrix.ones[Double](model.n, 1)

      for (inMessageIdx <- (neighbours.toSet - outMessageIdx).toSeq.sorted) {
        val multFactor = cache.kArr(nodeId) * betaArr(inMessageIdx)(nodeId)
        Ktu_beta :*= multFactor
      }

      betaArr(nodeId)(outMessageIdx) = KarrInv(outMessageIdx) * Ktu_beta
      normMessage(nodeId, outMessageIdx)
    }
  }
}
