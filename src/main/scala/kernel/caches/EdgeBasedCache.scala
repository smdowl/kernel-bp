package kernel.caches

import breeze.linalg.{CSCMatrix, DenseMatrix}
import kernel.kernels.Kernel
import kernel.models.{MessageParam, Model}

case class EdgeBasedCache(dataArr: Array[Array[CSCMatrix[Double]]],
                          kArr: Array[Array[DenseMatrix[Double]]],
                          kernel: Kernel,
                          msgParam: MessageParam ) {
  def numSamples(i: Int, j: Int) = kArr(i)(j).rows

  def numNodes = kArr.length

  def getNeighbours(nodeId: Int): Seq[Int] = {
    var out = Seq[Int]()

    (kArr(nodeId) zipWithIndex).foreach( pair => {
      if (pair._1 != null)
        out :+= pair._2
    })

    out
  }
}
