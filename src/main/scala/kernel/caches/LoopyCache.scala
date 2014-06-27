package kernel.caches

import breeze.linalg.DenseMatrix
import kernel.kernels.Kernel
import kernel.models.Model

object LoopyCache {
  def buildCache(sampleArr: Array[DenseMatrix[Double]], kernel: Kernel, model: Model): LoopyCache = {

    val numNodes = model.numNodes
    val sig = model.msgParam.sig

    val kArr = Array.ofDim[DenseMatrix[Double]](numNodes)
    val obsArr = Array.ofDim[DenseMatrix[Double]](numNodes)

    for (nodeInd <- 0 until numNodes)
      kArr(nodeInd) = kernel(sampleArr(nodeInd), sampleArr(nodeInd), sig)

    LoopyCache(kArr)
  }

}

case class LoopyCache(kArr: Array[DenseMatrix[Double]])
