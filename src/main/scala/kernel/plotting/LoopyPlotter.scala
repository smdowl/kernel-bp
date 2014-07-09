package kernel.plotting

import breeze.linalg._
import breeze.plot._
import kernel.inference.LoopyBeliefInferer

object LoopyPlotter {
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

  def plotResults(result: LoopyResult) = {
    val belief = calculateEmpiricalBelief(result)

    val inferer = new LoopyBeliefInferer(result)

    val rootMarginal: DenseVector[Double] = inferer.calculateKernelMarginal(result.model.rootNode)
    val condRootMarginal = inferer.calculateKernelCondRootMarginal(result.model.rootNode)

    val f = Figure()

    val d = result.support.cols
    if (d == 1) {
      val p = f.subplot(0)
      p += plot(result.support.toDenseVector, belief / sum(belief), colorcode = "b")
      p += plot(result.support.toDenseVector, condRootMarginal / sum(condRootMarginal), colorcode = "r")
      p += plot(result.support.toDenseVector, rootMarginal / sum(rootMarginal), colorcode = "g")
      p.title = "Belief at root"
    } else if (d == 2) {
      for (i <- 0 until d) {
        val p = f.subplot(d, 1, i)
        val (support, marginalBelief) = calculateMarginalBelief(i, result.support, condRootMarginal)
        val y: DenseVector[Double] = marginalBelief / sum(marginalBelief)
        p += plot(support.toDenseVector, y, colorcode = "r")
        p.title = s"Belief at root, d$i"
      }
    } else throw new Exception("Cannot handle more than 2-d")
  }

  def calculateEmpiricalBelief(output: LoopyResult): DenseVector[Double] = {
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

    // TODO: Only works when conditioning on a single obs, which is actually fine for what we need
    val condIndices = condIndicesArray(0)
    if (condIndices.length > 0) {
      val d = output.sampleArr(0).cols
      val condNodes = DenseMatrix.tabulate[Double](condIndices.size, d) { (i, j) => output.sampleArr(output.model.rootNode)(condIndices(i), j)}

      val beliefRootEmp = output.kernel(output.support, condNodes, output.sigRoot)
      sum(beliefRootEmp.t(::, *)).asInstanceOf[DenseMatrix[Double]].toDenseVector
    } else DenseVector.zeros[Double](output.support.rows)
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

  def calculateKernelRootMarginal(r: LoopyResult) = {
    val kernelRes = r.kernel(r.support, r.sampleArr(r.model.rootNode), r.sigRoot)
    sum(kernelRes, Axis._1)
  }

  def calculateKernelCondRootMarginal(rootIdx: Int, rootMarginal: DenseVector[Double], r: LoopyResult) = {
    var condRootMarginal: DenseVector[Double] = rootMarginal.copy

    for (neighbour <- r.model.getNeighbours(r.model.rootNode)) {

      val dotLeft = r.support
      val dotRight = r.sampleArr(r.model.rootNode)
      val multFactor: DenseMatrix[Double] = r.kernel(dotLeft, dotRight, r.model.msgParam.sig) * r.betaArr(neighbour)(rootIdx)

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
