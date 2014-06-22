package kernel

import breeze.linalg._
import breeze.plot._

case class MessageParam(lambda: Double, sig: Double)

case class Result(model: Model,
                  kernel: Kernel,
                  sampleArr: DenseMatrix[Double],
                  observations: Map[Int, DenseVector[Double]],
                  betaArr: Array[DenseMatrix[Double]],
                  sigRoot: Double,
                  axisBelief: DenseVector[Double])

object SimpleDemo {
  def main(args: Array[String]): Unit = {

    val numSamples = 200
    val model: Model = new DemoModel(numSamples)

    val sampleArr = model.generateData()

//    plotData(sampleArr)

    // Other params
    val observations = Map(3 -> DenseVector(0.0))

    val kernel = new RBFKernel()

    val passer = new MessagePasser(model, kernel)

    val betaArr = passer.passMessages(sampleArr, observations)

    val axisBelief = linspace(-5, 5, 200)

    // Parzen window parameter at root
    val sigRoot = 0.1

    val result = new Result(model, kernel, sampleArr, observations, betaArr, sigRoot, axisBelief)

    val belief = calculateEmpiricalBelief(result)

    val rootMarginal: DenseVector[Double] = calculateKernelRootMarginal(result)

    val condRootMarginal = calculateKernelCondRootMarginal(rootMarginal, result)

    val f = Figure()
    val p = f.subplot(0)

    p += plot(axisBelief, belief / sum(belief), colorcode = "b")
    p += plot(axisBelief, condRootMarginal / sum(condRootMarginal), colorcode = "r")
    p += plot(axisBelief, rootMarginal / sum(rootMarginal), colorcode = "g")
  }

  def calculateEmpiricalBelief(output: Result) = {
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

    val condNodes: DenseVector[Double] = DenseVector.tabulate[Double](condIndices.size){i => output.sampleArr(condIndices(i), output.model.rootNode)}

    val beliefRootEmp = output.kernel(output.axisBelief, condNodes, output.sigRoot)
    sum(beliefRootEmp.t(::, *)).asInstanceOf[DenseMatrix[Double]].toDenseVector
  }

  def calculateKernelRootMarginal(r: Result) = sum(r.kernel(r.axisBelief, r.sampleArr(::, r.model.rootNode), r.sigRoot), Axis._1)

  def calculateKernelCondRootMarginal(rootMarginal: DenseVector[Double], r: Result) = {
    var condRootMarginal: DenseVector[Double] = rootMarginal.copy
    val (prunedA, _) = r.model.getPrunedTree(r.observations.keySet)

    for (childId <- r.model.getChildren(r.model.rootNode, prunedA)) {

      val dotLeft = r.axisBelief
      val dotRight = r.sampleArr(::, r.model.rootNode)
      val multFactor: DenseMatrix[Double] = (r.kernel(dotLeft, dotRight, r.model.msgParam.sig) * r.betaArr(childId)).asInstanceOf[DenseMatrix[Double]]

      condRootMarginal :*= multFactor.toDenseVector
    }

    condRootMarginal
  }

  def plotData(data: DenseMatrix[Double]) = {
    val numNodes = data.cols

    val f = Figure()
    for (i <- 0 until numNodes) {
      val p = f.subplot(3, 2, i)
      p += hist(data(::, i))
      p.title = s"Node $i"
    }
  }
}