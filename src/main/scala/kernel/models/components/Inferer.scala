package kernel.models.components

import breeze.linalg.{Axis, DenseMatrix, DenseVector, sum}
import breeze.numerics.abs
import kernel.caches.Cache

class Inferer(testMatrix: DenseMatrix[Double]) {

  def calculateKernelMarginal(nodeId: Int, cache: Cache): DenseVector[Double] = {
    val neighbour = cache.getNeighbours(nodeId)(0)
    val kernelRes = cache.kernel(testMatrix, cache.dataArr(nodeId)(neighbour), cache.msgParam.sig)
    sum(kernelRes, Axis._1)
  }

  def calculateKernelCondRootMarginal(nodeId: Int, cache: Cache, betaArr: Array[Array[DenseMatrix[Double]]]): DenseVector[Double] = {
    var condRootMarginal: DenseVector[Double] = DenseVector.ones[Double](testMatrix.rows)

    for (neighbour <- cache.getNeighbours(nodeId)) {

      val data = cache.dataArr(nodeId)(neighbour).toDenseMatrix

      val kernelDot = cache.kernel(testMatrix, data, cache.msgParam.sig)
      val beta = betaArr(neighbour)(nodeId)

      val multFactor: DenseMatrix[Double] = kernelDot * beta

      condRootMarginal :*= multFactor.toDenseVector
    }

    abs(condRootMarginal).toDenseVector
  }
}
