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

    val numSamples = 1000
    val model: Model = new DemoModel(numSamples, "/Users/shaundowling/Google Drive/UCL/master project/code/kernelBP_source/kernelBP/sampArr")

    val sampleArr = model.generateData()
    val observations = Map(3 -> DenseVector(2.0))
//    plotData(sampleArr)

    val kernel = new RBFKernel()
    val passer = new MessagePasser(model, kernel)

    val betaArr = passer.passMessages(sampleArr, observations)

    val axisBelief = linspace(-5, 5, 200)
    val sigRoot = 0.1     // Parzen window parameter at root

//    testBetaSimilarity(model, betaArr)
//    val result = new Result(model, kernel, sampleArr, observations, betaArr, sigRoot, axisBelief)
  }


}