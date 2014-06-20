package kernel

import breeze.linalg.{Vector, DenseVector, DenseMatrix, Matrix}

class MessagePasser {

  def passMessages(model: Model, cache: Cache, observations: Map[Int, Double]) = {
    // Observed leaf messages
    val betaArr = Array.ofDim[Matrix[Double]](model.numNodes - observations.size)

    for ((leafId, idx) <- observations.keys.zipWithIndex) {
      val parentId = model.getParents(leafId)(0)

      val Kt = cache.kArr(leafId)(leafId)
      val Ks = cache.kArr(leafId)(parentId)
      val I = DenseMatrix.eye[Double](Kt.rows)

      val kt = rbfDot(cache.leafArr(leafId), DenseVector(observations(leafId)), model.msgParam.sig)

      // Have to split because type seems not to be infered otherwise
      val left: DenseMatrix[Double] = Kt :+ I * model.msgParam.lambda
      val right: DenseMatrix[Double] = Ks + I * model.msgParam.lambda
      betaArr(idx) = left * right \ kt
    }

    val prunedA = model.getPrunedTree(observations.keys)
    var computedList = observations.keySet

    var numUpdates = 1
    while (numUpdates > 0) {
      numUpdates = 0

//      for ()
    }
    println()
  }

  def rbfDot(p1: Vector[Double], p2: Vector[Double], deg: Double = 1.0): DenseMatrix[Double] = {
    // TODO: Currently slow version, but fast version is in MATLAB code
    val out = DenseMatrix.zeros[Double](p1.length, p2.length)

    for (i <- 0 until p1.length)
      for (j <- 0 until p2.length)
        out(i, j) = Math.exp(- Math.pow(p1(i) - p2(j), 2) / (2 * Math.pow(deg, 2)))

    out
  }
}
