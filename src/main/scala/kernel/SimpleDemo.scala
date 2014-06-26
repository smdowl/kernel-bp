package kernel

import breeze.linalg._
import kernel.plotting.Plotter
import app.Constants

case class MessageParam(lambda: Double, sig: Double)

case class Result(model: Model,
                  kernel: Kernel,
                  sampleArr: DenseMatrix[Double],
                  observations: Map[Int, DenseVector[Double]],
                  betaArr: Array[DenseMatrix[Double]],
                  sigRoot: Double,
                  axisBelief: DenseVector[Double])

object SimpleDemo {
  def runDemo() = {
    val numSamples = 100
    val model: Model = new DemoModel(numSamples)

    val sampleArr = model.generateData()
    val observations = Map(3 -> DenseVector(2.0))
    Plotter.plotData(sampleArr)

    val kernel = new RBFKernel()
    val passer = new MessagePasser(model, kernel)

    val betaArr = passer.passMessages(sampleArr, observations)

    val axisBelief = linspace(-5, 5, 200)
    val sigRoot = 0.1     // Parzen window parameter at root

    val result = new Result(model, kernel, sampleArr, observations, betaArr, sigRoot, axisBelief)
    Plotter.plotResults(result)
  }

  def test() = {
    val numSamples = 400
    val model: Model = new DemoModel(numSamples, Constants.SAMPLE_DATA)

    val sampleArr = model.generateData()
    val observations = Map(3 -> DenseVector(2.0))
    Plotter.plotData(sampleArr)

    val kernel = new RBFKernel()
    val passer = new MessagePasser(model, kernel)

    val betaArr = passer.passMessages(sampleArr, observations)

    val axisBelief = linspace(-5, 5, 200)
    val sigRoot = 0.1     // Parzen window parameter at root

    val result = new Result(model, kernel, sampleArr, observations, betaArr, sigRoot, axisBelief)
    Plotter.plotResults(result)
  }

  def main(args: Array[String]): Unit = {
    test()
  }
}