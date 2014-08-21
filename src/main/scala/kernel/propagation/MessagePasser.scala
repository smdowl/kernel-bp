package kernel.propagation

import breeze.linalg.{inv, max, norm, DenseMatrix}
import breeze.numerics.{abs, sqrt}
import kernel.caches.Cache

class MessagePasser(cache: Cache, observedNodes: Set[Int], numIter: Int = 50) {


  protected var betaArr: Array[Array[DenseMatrix[Double]]] = Array.ofDim[DenseMatrix[Double]](cache.numNodes, cache.numNodes)
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
      val neighbours = cache.getNeighbours(nodeId)
      neighbours.foreach(neighbourId => {
        val numSamples = cache.numSamples(nodeId, neighbourId)
        betaArr(nodeId)(neighbourId) = DenseMatrix.ones[Double](numSamples, 1) / sqrt(numSamples)
      })
    })
  }

  def calculateInverses() = {
    val out = Array.ofDim[DenseMatrix[Double]](cache.numNodes, cache.numNodes)

    for (i <- unobservedNodes) {
      val neighbours = cache.getNeighbours(i)
      val filtered = neighbours.filter(unobservedNodes.contains)
      for (j <- filtered) {
        val invMatrix = cache.kArr(j)(i) + DenseMatrix.eye[Double](cache.numSamples(j, i)) * cache.msgParam.lambda
        val inverse = inv(invMatrix)
        out(i)(j) = inverse
      }
    }

    out
  }

  def unobservedNodes = (0 until cache.numNodes).filterNot(observedNodes.contains)

  protected def calculateObservedMessages(): Unit = {
    for (leafId <- observations.keys) {
      val neighbours = cache.getNeighbours(leafId)

      neighbours.foreach(neighbourId => {
        val Kt = cache.kArr(leafId)(neighbourId)
        val Ks = cache.kArr(neighbourId)(leafId)
        val I = DenseMatrix.eye[Double](Kt.rows)

        val kt = cache.kernel(cache.dataArr(leafId)(neighbourId), observations(leafId), cache.msgParam.sig)

        val observed = observedMessage(Kt, Ks, kt, I, cache.msgParam.lambda)
        betaArr(leafId)(neighbourId) = observed
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
    for(i <- 0 until numIter)
      propagate()
  }

  private def propagate() = {
    val nodes = unobservedNodes

    for (nodeIdx <- nodes)
      updateMessageForNode(nodeIdx)
  }

  private def updateMessageForNode(nodeId: Int) = {
    val neighbours = cache.getNeighbours(nodeId)
    val nodesToUpdate = neighbours.filterNot(observations.keySet.contains)
    var non = null

    for (updatedNodeId <- nodesToUpdate) {
      val Ktu_beta = DenseMatrix.ones[Double](cache.numSamples(nodeId, updatedNodeId), 1)

      val incomingMessageIdices = (neighbours.toSet - updatedNodeId).toSeq.sorted
      for (incomingNodeId <- incomingMessageIdices) {

        val beta = betaArr(incomingNodeId)(nodeId)

        val grammMatrix = if (incomingNodeId < cache.numNodes / 2)
          cache.kArr(incomingNodeId)(nodeId)
        else if (updatedNodeId > nodeId)
          cache.translatedKArr(incomingNodeId)(nodeId)("backward")
        else
          cache.translatedKArr(incomingNodeId)(nodeId)("forward")

        val multFactor = grammMatrix * beta

        assert(multFactor.cols == 1, "Should be a vector.")
        assert(Ktu_beta.rows == multFactor.rows, "Should be same dimension.")
        Ktu_beta :*= multFactor
      }

      if (updatedNodeId == 0)
        non = null

      val newBeta = KarrInv(nodeId)(updatedNodeId) * Ktu_beta
      betaArr(nodeId)(updatedNodeId) = newBeta
      normMessage(nodeId, updatedNodeId)
    }
  }
}

