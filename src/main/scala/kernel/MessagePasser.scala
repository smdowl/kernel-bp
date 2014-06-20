package kernel

import breeze.linalg.{Vector, DenseVector, DenseMatrix, Matrix}

class MessagePasser(model: Model, kernel: Kernel) {

  var cache: Cache = _

  def passMessages(sampleArr: DenseMatrix[Double], observations: Map[Int, Double]) = {

    cache = buildCache(sampleArr)

    // Observed leaf messages
    val betaArr = Array.ofDim[Matrix[Double]](model.numNodes - observations.size)

    for ((leafId, idx) <- observations.keys.zipWithIndex) {
      val parentId = model.getParents(leafId)(0)

      val Kt = cache.kArr(leafId)(leafId)
      val Ks = cache.kArr(leafId)(parentId)
      val I = DenseMatrix.eye[Double](Kt.rows)

      val kt = kernel(cache.leafArr(leafId), DenseVector(observations(leafId)), model.msgParam.sig)

      // Have to split because type seems not to be infered otherwise
      val left: DenseMatrix[Double] = Kt :+ I * model.msgParam.lambda
      val right: DenseMatrix[Double] = Ks + I * model.msgParam.lambda
      betaArr(idx) = left * right \ kt
    }

    val prunedA = model.getPrunedTree(observations.keySet)
    var computedList = observations.keySet

    var numUpdates = 1
    while (numUpdates > 0) {
      numUpdates = 0

//      for ()
    }
    println()
  }

  def buildCache(sampleArr: DenseMatrix[Double]): Cache = {

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
}
