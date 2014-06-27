package kernel.propagation

import breeze.linalg.DenseMatrix
import kernel.Cache
import kernel.kernels.Kernel
import kernel.models.Model

class TreeMessagePasser(model: Model, kernel: Kernel) extends MessagePasser(model, kernel) {
  def passMessages(sampleArr: Array[DenseMatrix[Double]], observations: Map[Int, DenseMatrix[Double]]): Array[DenseMatrix[Double]] = {

    cache = Cache.buildCache(sampleArr, kernel, model)
    betaArr = Array.ofDim[DenseMatrix[Double]](model.numNodes - observations.size)

    calculateObservedMessages(observations)
    calculateInternalMessages(observations)

    betaArr
  }

  private def calculateObservedMessages(observations: Map[Int, DenseMatrix[Double]]): Unit = {
    for ((leafId, idx) <- observations.keys.zipWithIndex) {
      val parentId = model.getParents(leafId)(0)

      val Kt = cache.kArr(leafId)(leafId)
      val Ks = cache.kArr(leafId)(parentId)
      val I = DenseMatrix.eye[Double](Kt.rows)

      val kt = kernel(cache.leafArr(leafId), observations(leafId), model.msgParam.sig)

      betaArr(leafId) = observedMessage(Kt, Ks, kt, I, model.msgParam.lambda)
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

  private def calculateInternalMessages(observations: Map[Int, DenseMatrix[Double]]): Unit = {
    val (prunedA, prunedNodes) = model.getPrunedTree(observations.keySet)
    var computedList = observations.keySet

    var numUpdates = 1
    while (numUpdates > 0) {
      numUpdates = 0

      val nodesToUpdate = (0 until model.numNodes).filter(n =>
        !computedList.contains(n) && !prunedNodes.contains(n) && n != model.rootNode
      )

      nodesToUpdate.foreach(nodeId => {
        val childIds = model.getChildren(nodeId, prunedA)

        // We know there is a parent since we have excluded the root
        val parentId = model.getParents(nodeId, prunedA)(0)

        if (childIds.forall(computedList.contains)) {
          val Ks = cache.kArr(nodeId)(parentId)
          val nts = Ks.rows
          val Ktu_beta = DenseMatrix.ones[Double](nts, 1)

          // Calculate product of all incoming messages
          for (childId <- childIds) {
            Ktu_beta :*= cache.kArr(nodeId)(childId) * betaArr(childId)
          }

          betaArr(nodeId) = (Ks + DenseMatrix.eye[Double](nts) * model.msgParam.lambda) \ Ktu_beta

          computedList += nodeId
          numUpdates += 1
        }

      })
    }
  }
}
