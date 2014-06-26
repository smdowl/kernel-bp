package kernel.plotting

import breeze.linalg._
import breeze.plot._
import kernel.Result

object Plotter {
  def plotData(data: DenseMatrix[Double]) = {
    val numNodes = data.cols

    val f = Figure()
    for (i <- 0 until numNodes) {
      val p = f.subplot(3, 2, i)
      p += hist(data(::, i))
      p.title = s"Node $i"
    }
  }

  def plotResults(result: Result) = {
    val belief = calculateEmpiricalBelief(result)
    val rootMarginal: DenseVector[Double] = calculateKernelRootMarginal(result)
    val condRootMarginal = calculateKernelCondRootMarginal(rootMarginal, result)

    val f = Figure()
    val p = f.subplot(0)

    p += plot(result.axisBelief, belief / sum(belief), colorcode = "b")
    p += plot(result.axisBelief, condRootMarginal / sum(condRootMarginal), colorcode = "r")
    p += plot(result.axisBelief, rootMarginal / sum(rootMarginal), colorcode = "g")
  }

  def calculateEmpiricalBelief(output: Result): DenseVector[Double] = {
    // Empirical root belief
    val threshold = 0.01

    // TODO: sums don't make any sense with higher dim data
    var condIndices = Seq[Int]()
    output.observations.keys.foreach(nodeId => {
      condIndices ++= output.sampleArr(::, nodeId).findAll(sample =>
        sample > sum(output.observations(nodeId)).asInstanceOf[Double] - threshold &&
          sample < sum(output.observations(nodeId)).asInstanceOf[Double] + threshold
      )
    })

    if (condIndices.length > 0) {
      val condNodes: DenseVector[Double] = DenseVector.tabulate[Double](condIndices.size) { i => output.sampleArr(condIndices(i), output.model.rootNode)}

      val beliefRootEmp = output.kernel(output.axisBelief, condNodes, output.sigRoot)
      sum(beliefRootEmp.t(::, *)).asInstanceOf[DenseMatrix[Double]].toDenseVector
    } else DenseVector.zeros[Double](output.axisBelief.length)
  }

  def calculateKernelRootMarginal(r: Result) = sum(r.kernel(r.axisBelief, r.sampleArr(::, r.model.rootNode), r.sigRoot), Axis._1)

  def calculateKernelCondRootMarginal(rootMarginal: DenseVector[Double], r: Result) = {
    var condRootMarginal: DenseVector[Double] = rootMarginal.copy
    val (prunedA, _) = r.model.getPrunedTree(r.observations.keySet)

    for (childId <- r.model.getChildren(r.model.rootNode, prunedA)) {

      val dotLeft = r.axisBelief
      val dotRight = r.sampleArr(::, r.model.rootNode)
      val multFactor: DenseMatrix[Double] = (r.kernel(dotLeft, dotRight, r.model.msgParam.sig) * r.betaArr(childId)).asInstanceOf[DenseMatrix[Double]]

      condRootMarginal = rootMarginal :* multFactor.toDenseVector
    }

    condRootMarginal
  }
}
