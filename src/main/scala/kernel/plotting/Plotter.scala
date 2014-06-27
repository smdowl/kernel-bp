package kernel.plotting

import breeze.linalg._
import breeze.plot._
import kernel.Result

object Plotter {
  def plotData(data: Array[DenseMatrix[Double]]) = {
    val d = data(0).cols

    if (d == 1)
      plot1dData(data)
    else if (d == 2)
      plot2dData(data)
    else
      throw new Exception("Cannot handle higher dim data")
  }

  def plot1dData(data: Array[DenseMatrix[Double]]) = {
    val numNodes = data.length

    val f = Figure()
    for (i <- 0 until numNodes) {
      val p = f.subplot(3, 2, i)
      p += hist(data(i)(::, 0))
      p.title = s"Node $i"
    }
  }

  def plot2dData(data: Array[DenseMatrix[Double]]) = {
    val numNodes = data.length

    val f = Figure()
    for (i <- 0 until numNodes) {
      val p = f.subplot(3, 2, i)
      p += scatter(data(i)(::, 0), data(i)(::, 1), {_ => 0.01})
      p.title = s"Node $i"
    }
  }

  def plotResults(result: Result) = {
    val belief = calculateEmpiricalBelief(result)
    val rootMarginal: DenseVector[Double] = calculateKernelRootMarginal(result)
    val condRootMarginal = calculateKernelCondRootMarginal(rootMarginal, result)

    val f = Figure()
    val p = f.subplot(0)

    p += plot(result.axisBelief.toDenseVector, belief / sum(belief), colorcode = "b")
    p += plot(result.axisBelief.toDenseVector, condRootMarginal / sum(condRootMarginal), colorcode = "r")
    p += plot(result.axisBelief.toDenseVector, rootMarginal / sum(rootMarginal), colorcode = "g")
  }

  def calculateEmpiricalBelief(output: Result): DenseVector[Double] = {
    // Empirical root belief
    implicit val threshold = 0.01

    val numObs = output.observations.keys.size
    val condIndicesArray = Array.ofDim[Seq[Int]](numObs)

    // Want to find all points that are suitably close to those that we have observed.
    for((nodeId, index) <- output.observations.keys.zipWithIndex) {
      val data: DenseMatrix[Double] = output.sampleArr(nodeId)
      val nodeObservations = output.observations(nodeId)
      val newIndices =  for (j <- 0 until data.rows if closeIndex(j, data, nodeObservations)) yield j

      condIndicesArray(index) = newIndices
    }

    // TODO: Only works when conditioning on a single obs
    val condIndices = condIndicesArray(0)
    if (condIndices.length > 0) {
      val d = output.sampleArr(0).cols
      val condNodes = DenseMatrix.tabulate[Double](condIndices.size, d) { (i, j) => output.sampleArr(output.model.rootNode)(condIndices(i), j)}

      val beliefRootEmp = output.kernel(output.axisBelief, condNodes, output.sigRoot)
      sum(beliefRootEmp.t(::, *)).asInstanceOf[DenseMatrix[Double]].toDenseVector
    } else DenseVector.zeros[Double](output.axisBelief.rows)
  }

  def closeIndex(j: Int, data: DenseMatrix[Double], nodeObservations: DenseMatrix[Double])(implicit threshold: Double): Boolean = {
    val point: DenseVector[Double] = data(j, ::).t

    assert(nodeObservations.cols == 1, "Only want to convert to a vector when we have a vector matrix")
    val obs = nodeObservations.toDenseVector

    val dist: Double = norm(point - obs).asInstanceOf[Double]

    dist < threshold
  }

  def calculateKernelRootMarginal(r: Result) = sum(r.kernel(r.axisBelief, r.sampleArr(r.model.rootNode), r.sigRoot), Axis._1)

  def calculateKernelCondRootMarginal(rootMarginal: DenseVector[Double], r: Result) = {
    var condRootMarginal: DenseVector[Double] = rootMarginal.copy
    val (prunedA, _) = r.model.getPrunedTree(r.observations.keySet)

    for (childId <- r.model.getChildren(r.model.rootNode, prunedA)) {

      val dotLeft = r.axisBelief
      val dotRight = r.sampleArr(r.model.rootNode)
      val multFactor: DenseMatrix[Double] = (r.kernel(dotLeft, dotRight, r.model.msgParam.sig) * r.betaArr(childId))

      condRootMarginal = rootMarginal :* multFactor.toDenseVector
    }

    condRootMarginal
  }
}
