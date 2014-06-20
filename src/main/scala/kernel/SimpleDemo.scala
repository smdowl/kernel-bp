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
    passer.passMessages(sampleArr, observations)
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