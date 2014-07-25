package kernel.caches

import breeze.linalg.DenseMatrix
import kernel.kernels.Kernel
import kernel.models.Model

object EdgeBasedCache {
  def buildCache(sampleArr: Array[DenseMatrix[Double]], kernel: Kernel, model: Model): EdgeBasedCache = {

    val numNodes = model.numNodes
    val sig = model.msgParam.sig

    val kArr = Array.ofDim[DenseMatrix[Double]](numNodes, numNodes)

    val sharedSets = model.sharedSets

    for (set <- sharedSets) {
      val (leftData, rightData) = buildData(set, sampleArr)

      set.foreach{ case (l, r) =>
        kArr(l)(r) = leftData
        kArr(r)(l) = rightData
      }
    }

//    EdgeBasedCache(kArr)
    EdgeBasedCache(null)
  }

  def buildData(set: List[(Int, Int)], sampleArr: Array[DenseMatrix[Double]]) = {
    var leftData = List[DenseMatrix[Double]]()
    var rightData = List[DenseMatrix[Double]]()

    set.foreach{ case (l, r) =>
      leftData :+= sampleArr(l)
      rightData :+= sampleArr(r)
    }

    (mergeData(leftData), mergeData(rightData))
  }

  def mergeData(dataSeq: Seq[DenseMatrix[Double]]) = dataSeq.tail.foldLeft(dataSeq.head)((a, b) => DenseMatrix.vertcat(a, b))
}

case class EdgeBasedCache(kArr: Array[DenseMatrix[Double]])
