package kernel.models.components

import breeze.linalg.{Axis, DenseMatrix, DenseVector, sum}
import kernel.caches.Cache

class Inferer(testMatrix: DenseMatrix[Double]) {

  def calculateKernelMarginal(nodeId: Int, cache: Cache): DenseVector[Double] = {
    val neighbour = cache.getNeighbours(nodeId)(0)
    val kernelRes = cache.kernel(testMatrix, cache.dataArr(nodeId)(neighbour), cache.msgParam.sig)
    sum(kernelRes, Axis._1)
  }

  def calculateKernelCondRootMarginal(nodeId: Int, cache: Cache, betaArr: Array[Array[DenseMatrix[Double]]]): DenseVector[Double] = {
    var condRootMarginal: DenseVector[Double] = calculateKernelMarginal(nodeId, cache)

    for (neighbour <- cache.getNeighbours(nodeId)) {

      val dotLeft = testMatrix
      val dotRight = cache.dataArr(nodeId)(neighbour)
      val multFactor: DenseMatrix[Double] = cache.kernel(dotLeft, dotRight, cache.msgParam.sig) * betaArr(neighbour)(nodeId)

      condRootMarginal :*= multFactor.toDenseVector
    }

    condRootMarginal
  }
}
