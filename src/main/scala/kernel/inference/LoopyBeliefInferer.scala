package kernel.inference

import breeze.linalg._
import kernel.plotting.LoopyResult

class LoopyBeliefInferer(result: LoopyResult) {
  
  def calculateEmpiricalBelief(): DenseVector[Double] = {
    // Empirical root belief
    implicit val threshold = 0.01

    val numObs = result.observations.keys.size
    val condIndicesArray = Array.ofDim[Seq[Int]](numObs)

    // Want to find all points that are suitably close to those that we have observed.
    for((nodeId, index) <- result.observations.keys.zipWithIndex) {
      val data: DenseMatrix[Double] = result.sampleArr(nodeId)
      val nodeObservations = result.observations(nodeId)
      val newIndices =  for (j <- 0 until data.rows if closeIndex(j, data, nodeObservations)) yield j

      condIndicesArray(index) = newIndices
    }

    // TODO: Only works when conditioning on a single obs, which is actually fine for what we need
    val condIndices = condIndicesArray(0)
    if (condIndices.length > 0) {
      val d = result.sampleArr(0).cols
      val condNodes = DenseMatrix.tabulate[Double](condIndices.size, d) { (i, j) => result.sampleArr(result.model.rootNode)(condIndices(i), j)}

      val beliefRootEmp = result.kernel(result.axisBelief, condNodes, result.sigRoot)
      sum(beliefRootEmp.t(::, *)).asInstanceOf[DenseMatrix[Double]].toDenseVector
    } else DenseVector.zeros[Double](result.axisBelief.rows)
  }

  def closeIndex(j: Int, data: DenseMatrix[Double], nodeObservations: DenseMatrix[Double])(implicit threshold: Double): Boolean = {
    val point: DenseVector[Double] = data(j, ::).t

    // TODO: Close to any of the points. Need to think if this is what we want.
    (0 until nodeObservations.rows).exists(i => {
      val obs = nodeObservations(i, ::).t
      val dist: Double = norm(point - obs).asInstanceOf[Double]
      dist < threshold
    })
  }

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
