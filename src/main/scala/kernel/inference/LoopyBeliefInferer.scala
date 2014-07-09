package kernel.inference

import breeze.linalg._
import kernel.plotting.LoopyResult

class LoopyBeliefInferer(result: LoopyResult) {
 
  def calculateKernelRootMarginal() = {
    val kernelRes = result.kernel(result.axisBelief, result.sampleArr(result.model.rootNode), result.sigRoot)
    sum(kernelRes, Axis._1)
  }

  def calculateKernelCondRootMarginal(rootIdx: Int, rootMarginal: DenseVector[Double]) = {
    var condRootMarginal: DenseVector[Double] = rootMarginal.copy

    for (neighbour <- result.model.getNeighbours(result.model.rootNode)) {

      val dotLeft = result.axisBelief
      val dotRight = result.sampleArr(result.model.rootNode)
      val multFactor: DenseMatrix[Double] = result.kernel(dotLeft, dotRight, result.model.msgParam.sig) * result.betaArr(neighbour)(rootIdx)

      condRootMarginal :*= multFactor.toDenseVector
    }

    condRootMarginal
  }

  def calculateMarginalBelief(dim: Int, points: DenseMatrix[Double], belief: DenseVector[Double]): (DenseVector[Double], DenseVector[Double]) = {
    val dimPoints = points(::, dim)

    var sums = Map[Double, Seq[Double]]()
    for ((p, b) <- dimPoints.toArray zip belief.toArray) {
      val map = sums.getOrElse(p, Seq[Double]())
      sums += p -> (map :+ b)
    }

    val support = DenseVector.zeros[Double](sums.size)
    val out = DenseVector.zeros[Double](sums.size)
    var i = 0
    sums.keySet.toSeq.sorted.foreach(key => {
      support(i) = key
      out(i) = sums(key).sum / sums(key).length
      i += 1
    })

    (support, out)
  }
}
