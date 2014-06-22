package kernel

import breeze.linalg.{Vector, DenseVector, DenseMatrix, Matrix}

class MessagePasser(model: Model, kernel: Kernel) {
  private var cache: Cache = _
  private var betaArr: Array[DenseMatrix[Double]] = _

  def passMessages(sampleArr: DenseMatrix[Double], observations: Map[Int, Double]): Array[DenseMatrix[Double]] = {

    cache = buildCache(sampleArr)
    betaArr = Array.ofDim[DenseMatrix[Double]](model.numNodes - observations.size)

    calculateObservedMessages(observations)
    calculateInternalMessages(observations)

    betaArr
  }

  private def buildCache(sampleArr: DenseMatrix[Double]): Cache = {

    val numNodes = model.numNodes
    val sig = model.msgParam.sig

    val kArr = Array.ofDim[DenseMatrix[Double]](numNodes, numNodes)
    val leafArr = Array.ofDim[Vector[Double]](numNodes)

    for (nodeInd <- 0 until numNodes) {
      val children = model.getChildren(nodeInd)
      for (childInd <- children)
        kArr(nodeInd)(childInd) = kernel(sampleArr(::, nodeInd), sampleArr(::, nodeInd), sig)

      if (children.length == 0) {
        kArr(nodeInd)(nodeInd) = kernel(sampleArr(::, nodeInd), sampleArr(::, nodeInd), sig)
        leafArr(nodeInd) = sampleArr(::, nodeInd)
      }

      for (parentInd <- model.getParents(nodeInd))
        kArr(nodeInd)(parentInd) = kernel(sampleArr(::, parentInd), sampleArr(::, parentInd), sig)
    }

    Cache(kArr, leafArr)
  }

  private def calculateObservedMessages(observations: Map[Int, Double]): Unit = {
    for ((leafId, idx) <- observations.keys.zipWithIndex) {
      val parentId = model.getParents(leafId)(0)

      val Kt = cache.kArr(leafId)(leafId)
      val Ks = cache.kArr(leafId)(parentId)
      val I = DenseMatrix.eye[Double](Kt.rows)

      val kt = kernel(cache.leafArr(leafId), DenseVector(observations(leafId)), model.msgParam.sig)

      // Have to split because type seems not to be infered otherwise
      val left: DenseMatrix[Double] = Kt :+ I * model.msgParam.lambda
      val right: DenseMatrix[Double] = Ks + I * model.msgParam.lambda
      betaArr(leafId) = left * right \ kt
    }
  }

  private def calculateInternalMessages(observations: Map[Int, Double]): Unit = {
    val (prunedA, prunedNodes) = model.getPrunedTree(observations.keySet)
    var computedList = observations.keySet

    var numUpdates = 1
    while (numUpdates > 0) {
      numUpdates = 0

      val updateNodes = (0 until model.numNodes).filter(n =>
        !computedList.contains(n) && !prunedNodes.contains(n) && n != model.rootNode
      )

      updateNodes.foreach(nodeId => {
        val childInds = model.getChildren(nodeId, prunedA)

        // We know there is a parent since we have excluded the root
        val parentId = model.getParents(nodeId, prunedA)(0)

        if (childInds.forall(computedList.contains)) {
          val Ks = cache.kArr(nodeId)(parentId)
          val nts = Ks.rows
          val Ktu_beta: DenseMatrix[Double] = DenseMatrix.ones[Double](nts, 1)

          for (childId <- childInds)
            Ktu_beta :*= (cache.kArr(nodeId)(childId) * betaArr(childId))

          betaArr(nodeId) = (Ks + DenseMatrix.eye[Double](nts) * model.msgParam.lambda) \ Ktu_beta
          computedList += nodeId
          numUpdates += 1
        }

      })
    }
  }
}
