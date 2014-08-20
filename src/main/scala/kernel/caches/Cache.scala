package kernel.caches

import breeze.linalg.{CSCMatrix, DenseMatrix}
import kernel.kernels.Kernel
import kernel.models.components.MessageParam

case class Cache(dataArr: Array[Array[CSCMatrix[Double]]],
                          kArr: Array[Array[DenseMatrix[Double]]],
                          kernel: Kernel,
                          translatedKArr: Array[Array[Map[String, DenseMatrix[Double]]]],
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
