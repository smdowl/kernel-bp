package kernel

import breeze.linalg.{DenseVector, DenseMatrix}

class MessagePasser(model: Model, kernel: Kernel) {
  private var cache: Cache = _
  private var betaArr: Array[DenseMatrix[Double]] = _

  def passMessages(sampleArr: DenseMatrix[Double], observations: Map[Int, DenseVector[Double]]): Array[DenseMatrix[Double]] = {

    cache = Cache.buildCache(sampleArr, kernel, model)
    betaArr = Array.ofDim[DenseMatrix[Double]](model.numNodes - observations.size)

    calculateObservedMessages(observations)
    calculateInternalMessages(observations)

    betaArr
  }

  private def calculateObservedMessages(observations: Map[Int, DenseVector[Double]]): Unit = {
    for ((leafId, idx) <- observations.keys.zipWithIndex) {
      val parentId = model.getParents(leafId)(0)

      val Kt = cache.kArr(leafId)(leafId)
      val Ks = cache.kArr(leafId)(parentId)
      val I = DenseMatrix.eye[Double](Kt.rows)

      val kt = kernel(cache.leafArr(leafId), observations(leafId), model.msgParam.sig)

      // Have to split because type seems not to be infered otherwise
      val left: DenseMatrix[Double] = Kt :+ I * model.msgParam.lambda
      val right: DenseMatrix[Double] = Ks + I * model.msgParam.lambda
      betaArr(leafId) = left * right \ kt
    }
  }

  private def calculateInternalMessages(observations: Map[Int, DenseVector[Double]]): Unit = {
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
