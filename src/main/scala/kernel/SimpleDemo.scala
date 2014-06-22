package kernel

import breeze.linalg._
import breeze.plot._

case class MessageParam(lambda: Double, sig: Double)
case class Cache(kArr: Array[Array[DenseMatrix[Double]]], leafArr: Array[Vector[Double]])

object SimpleDemo {
  def main(args: Array[String]): Unit = {

    val numSamples = 200
    val model: Model = new DemoModel(numSamples)

    val sampleArr = model.generateData()

//    plotData(sampleArr)

    // Other params
    val observations = Map(3 -> 0.0)

    // Parzen window parameter at root
    val sigRoot = 0.1

    val kernel = new RBFKernel()

    val passer = new MessagePasser(model, kernel)

    val betaArr = passer.passMessages(sampleArr, observations)

    // Empirical root belief
    val threshold = 0.01

    var condIndices = Seq[Int]()
    observations.keys.foreach(nodeId => {
      condIndices ++= sampleArr(::, nodeId).findAll(sample =>
        sample > observations(nodeId) - threshold && sample < observations(nodeId) + threshold
      )
    })

    val axisBelief = linspace(-5, 5, 200)

    val condNodes: DenseVector[Double] = DenseVector.tabulate[Double](condIndices.size){i => sampleArr(condIndices(i), model.rootNode)}

    val beliefRootEmp = kernel(axisBelief, condNodes, sigRoot)
    val belief = sum(beliefRootEmp.t(::, *)).asInstanceOf[DenseMatrix[Double]].toDenseVector

    // Kernel root belief
    val rootMarginal: DenseVector[Double] = sum(kernel(axisBelief, sampleArr(::, model.rootNode), sigRoot), Axis._1)

    var condRootMarginal: DenseVector[Double] = rootMarginal.copy
    val (prunedA, prunedNodes) = model.getPrunedTree(observations.keySet)

    for (childId <- model.getChildren(model.rootNode, prunedA))
      condRootMarginal :*= (kernel(axisBelief, sampleArr(::, model.rootNode), model.msgParam.sig) * betaArr(childId)).toDenseVector

    val f = Figure()
    val p = f.subplot(0)

    p += plot(axisBelief, belief / sum(belief), colorcode = "b")
    p += plot(axisBelief, condRootMarginal / sum(condRootMarginal), colorcode = "r")
    p += plot(axisBelief, rootMarginal / sum(rootMarginal), colorcode = "g")
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