package kernel

import breeze.linalg.{DenseVector, Vector, DenseMatrix}
import kernel.models.Model

object Cache {
  def buildCache(sampleArr: Array[DenseMatrix[Double]], kernel: Kernel, model: Model): Cache = {

    val numNodes = model.numNodes
    val sig = model.msgParam.sig

    val kArr = Array.ofDim[DenseMatrix[Double]](numNodes, numNodes)
    val leafArr = Array.ofDim[DenseMatrix[Double]](numNodes)

    for (nodeInd <- 0 until numNodes) {
      val children = model.getChildren(nodeInd)
      for (childInd <- children)
        kArr(nodeInd)(childInd) = kernel(sampleArr(nodeInd), sampleArr(nodeInd), sig)

      if (children.length == 0) {
        kArr(nodeInd)(nodeInd) = kernel(sampleArr(nodeInd), sampleArr(nodeInd), sig)
        leafArr(nodeInd) = sampleArr(nodeInd)
      }

      for (parentInd <- model.getParents(nodeInd))
        kArr(nodeInd)(parentInd) = kernel(sampleArr(parentInd), sampleArr(parentInd), sig)
    }

    Cache(kArr, leafArr)
  }
}

case class Cache(kArr: Array[Array[DenseMatrix[Double]]], leafArr: Array[DenseMatrix[Double]])
