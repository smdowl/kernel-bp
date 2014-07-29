package kernel.parsing

import breeze.linalg.DenseMatrix
import kernel.caches.EdgeBasedCache
import kernel.kernels.Kernel
import kernel.models.edge.Edge

class HMMParser(kernel: Kernel) extends EdgeParser(kernel) {
  private var length: Int = _
  // TODO: Pass in somehow
  private val sig = 0.7

  private var dataArr: Array[Array[DenseMatrix[Double]]] = _
  private var kArr: Array[Array[DenseMatrix[Double]]] = _
  private val REQUIRED_KEYS = Set("hidden", "visible")

  override def buildCache(edges: Map[String, Edge], length: Int): EdgeBasedCache = {

    assert(edges.keySet.equals(REQUIRED_KEYS), "Should have exactly the right edges defined")

    val numNodes = length * 2

    this.length = length
    dataArr = Array.ofDim[DenseMatrix[Double]](numNodes, numNodes)
    kArr = Array.ofDim[DenseMatrix[Double]](numNodes, numNodes)

    fillHidden(edges)
    fillVisible(edges)

    EdgeBasedCache(dataArr, kArr, kernel)
  }

  private def fillHidden(edges: Map[String, Edge]) = {
    val edge = edges("hidden")

    for (i <- 0 until length - 1) {
      dataArr(i)(i+1) = edge.startData
      kArr(i)(i+1) = kernel(edge.startData, edge.startData, sig)

      dataArr(i+1)(i) = edge.endData
      kArr(i+1)(i) = kernel(edge.endData, edge.endData, sig)
    }
  }

  private def fillVisible(edges: Map[String, Edge]) = {
    val edge = edges("visible")

    for (i <- 0 until length) {
      dataArr(i)(i+length) = edge.startData
      kArr(i)(i+length) = kernel(edge.startData, edge.startData, sig)

      dataArr(i+length)(i) = edge.endData
      kArr(i+length)(i) = kernel(edge.endData, edge.endData, sig)
    }
  }
}
