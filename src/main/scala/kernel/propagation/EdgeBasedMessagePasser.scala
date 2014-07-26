package kernel.propagation

import breeze.linalg.{inv, max, norm, DenseMatrix}
import breeze.numerics.{abs, sqrt}
import kernel.caches.{LoopyCache, EdgeBasedCache}
import kernel.kernels.Kernel
import kernel.models.Model

class EdgeBasedMessagePasser(model: Model, kernel: Kernel, sampleArr: Array[DenseMatrix[Double]], observedNodes: Set[Int]) {

  private val numIter = 100

  protected var cache: EdgeBasedCache = EdgeBasedCache.buildCache(sampleArr, kernel, model)
  protected var betaArr: Array[Array[DenseMatrix[Double]]] = Array.ofDim[DenseMatrix[Double]](model.numNodes, model.numNodes)
  protected var KarrInv: Array[Array[DenseMatrix[Double]]] = calculateInverses()
  protected var observations: Map[Int, DenseMatrix[Double]] = _

  def getCache = this.cache

  def passMessages(observations: Map[Int, DenseMatrix[Double]]): Array[Array[DenseMatrix[Double]]] = {
    this.observations = observations

    assert(observations.keySet.equals(observedNodes), "Should have exactly the same observations")

    initBetas()

    calculateObservedMessages()
    calculateInternalMessages()

    betaArr
  }

  private def initBetas() = {
    unobservedNodes.foreach(nodeId => {
      val neighbours = model.getNeighbours(nodeId)
      neighbours.foreach(neighbourId => {
        val numSamples = cache.numSamples(nodeId, neighbourId)
        betaArr(nodeId)(neighbourId) = DenseMatrix.ones[Double](numSamples, 1) / sqrt(numSamples)
      })
    })
  }

  def calculateInverses() = {
    val out = Array.ofDim[DenseMatrix[Double]](model.numNodes, model.numNodes)

    for (i <- unobservedNodes)
      for (j <- model.getNeighbours(i).filter(unobservedNodes.contains))
        out(i)(j) = inv( cache.kArr(i)(j) + DenseMatrix.eye[Double](cache.numSamples(i, j)) * model.msgParam.lambda )

    out
  }

  def unobservedNodes = (0 until model.numNodes).filterNot(observedNodes.contains)

  protected def calculateObservedMessages(): Unit = {
    for (leafId <- observations.keys) {
      val neighbours = model.getNeighbours(leafId)

      neighbours.foreach(neighbourId => {
        val Kt = cache.kArr(leafId)(neighbourId)
        val Ks = cache.kArr(neighbourId)(leafId)
        val I = DenseMatrix.eye[Double](Kt.rows)

        val kt = kernel(cache.dataArr(neighbourId)(leafId), observations(leafId), model.msgParam.sig)

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
      val Ktu_beta = DenseMatrix.ones[Double](cache.numSamples(nodeId, outMessageIdx), 1)

      for (inMessageIdx <- (neighbours.toSet - outMessageIdx).toSeq.sorted) {
        val multFactor = cache.kArr(nodeId)(inMessageIdx) * betaArr(inMessageIdx)(nodeId)
        Ktu_beta :*= multFactor
      }

      betaArr(nodeId)(outMessageIdx) = KarrInv(outMessageIdx)(nodeId) * Ktu_beta
      normMessage(nodeId, outMessageIdx)
    }
  }
}

