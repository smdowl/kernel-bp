package kernel.parsing

import breeze.linalg.{CSCMatrix, DenseMatrix}
import kernel.caches.Cache
import kernel.kernels.Kernel
import kernel.models.components._

class HMMParser(msgParam: MessageParam, kernel: Kernel) extends EdgeParser(kernel) {
  private var length: Int = _

  private var dataArr: Array[Array[CSCMatrix[Double]]] = _
  private var kArr: Array[Array[DenseMatrix[Double]]] = _
  var translatedKArr: Array[Array[Map[String, DenseMatrix[Double]]]] = _
  private val REQUIRED_KEYS = Set("hidden", "visible")

  override def buildCache(edges: Map[String, Edge], length: Int, smooth: Boolean = false): Cache = {

    assert(edges.keySet.equals(REQUIRED_KEYS), "Should have exactly the right edges defined")

    val numNodes = length * 2

    this.length = length
    dataArr = Array.ofDim[CSCMatrix[Double]](numNodes, numNodes)
    translatedKArr = Array.ofDim[Map[String, DenseMatrix[Double]]](numNodes, numNodes)
    kArr = Array.ofDim[DenseMatrix[Double]](numNodes, numNodes)

    fillHidden(edges)
    fillVisible(edges)

    if (smooth)
      smoothKArr()

    Cache(dataArr, kArr, kernel, translatedKArr, msgParam)
  }

  private def fillHidden(edges: Map[String, Edge]) = {
    val edge = edges("hidden")

    for (i <- 0 until length - 1) {
      dataArr(i)(i+1) = edge.startData
      kArr(i)(i+1) = kernel(edge.startData, edge.endData, msgParam.sig)

      dataArr(i+1)(i) = edge.endData
      kArr(i+1)(i) = kernel(edge.endData, edge.startData, msgParam.sig)
    }
  }

  private def fillVisible(edges: Map[String, Edge]) = {
    val edge = edges("visible")
    val hiddenEdge = edges("hidden")

    for (i <- 0 until length) {
      dataArr(i)(i+length) = edge.startData
      kArr(i)(i+length) = kernel(edge.startData, edge.startData, msgParam.sig)

      dataArr(i+length)(i) = edge.endData
      kArr(i+length)(i) = kernel(edge.endData, edge.endData, msgParam.sig)

      // TODO: Work out the right direction and set the correct values.
      translatedKArr(i+length)(i) = Map[String, DenseMatrix[Double]]()
      translatedKArr(i+length)(i) += "forward" -> kernel(hiddenEdge.startData, edge.startData, msgParam.sig)
      translatedKArr(i+length)(i) += "backward" -> kernel(hiddenEdge.endData, edge.startData, msgParam.sig)
    }
  }

  private def smoothKArr() = {
    for (i <- 0 until kArr.length) {
      for (j <- 0 until kArr(i).length) {
        if (kArr(i)(j) != null)
          kArr(i)(j) :+= 1e-16
      }
    }
  }
}
