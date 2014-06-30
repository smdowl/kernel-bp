package kernel

import breeze.linalg._
import kernel.linalg._
import kernel.kernels.{RBFKernel, Kernel}
import kernel.models.{LoopyDemoModel, ExtendedModel, DemoModel, Model}
import kernel.plotting.{LoopyPlotter, LoopyResult, Result, Plotter}
import app.Constants
import kernel.propagation.{LoopyMessagePasser, TreeMessagePasser}

case class MessageParam(lambda: Double, sig: Double)



object SimpleDemo {
  def runDemo() = {
    val numSamples = 400
    val model: Model = new DemoModel(numSamples)

    val sampleArr = model.generateData()
    val observations = Map(3 -> DenseMatrix(2.0))
    Plotter.plotData(sampleArr)

    val kernel = new RBFKernel()
    val passer = new TreeMessagePasser(model, kernel)

    val betaArr = passer.passMessages(sampleArr, observations)

    val axisBelief = DenseMatrix(linspace(-5, 5, 200).toArray).t
    val sigRoot = 0.1     // Parzen window parameter at root

    val result = new Result(model, kernel, sampleArr, observations, betaArr, sigRoot, axisBelief)
    Plotter.plotResults(result)
  }

  def test() = {
    val numSamples = 20
    val model: Model = new DemoModel(numSamples, Constants.SAMPLE_DATA)

    val sampleArr = model.generateData()
    val observations = Map(3 -> DenseMatrix(2.0))
    Plotter.plotData(sampleArr)

    val kernel = new RBFKernel()
    val passer = new TreeMessagePasser(model, kernel)

    val betaArr = passer.passMessages(sampleArr, observations)

    val axisBelief = DenseMatrix(linspace(-5, 5, 200).toArray).t
    val sigRoot = 0.1     // Parzen window parameter at root

    val result = new Result(model, kernel, sampleArr, observations, betaArr, sigRoot, axisBelief)
    Plotter.plotResults(result)
  }

  def higherDim() = {
    val numSamples = 100
    val model: ExtendedModel = new ExtendedModel(numSamples)

    val sampleArr = model.generateData()

    val observations = Map(3 -> DenseMatrix((0.0, 0.0)))
//    Plotter.plotData(sampleArr)

    val kernel = new RBFKernel()
    val passer = new TreeMessagePasser(model, kernel)

    val betaArr = passer.passMessages(sampleArr, observations)

    val axisRange = linspace(-5, 5, 150)
    val axisBelief = meshgrid(axisRange, axisRange)

    val sigRoot = 0.1     // Parzen window parameter at root

    val result = new Result(model, kernel, sampleArr, observations, betaArr, sigRoot, axisBelief)
    Plotter.plotResults(result)
  }

  def runLoopy() = {
    val numSamples = 50
    val model: Model = new LoopyDemoModel(numSamples, "/Users/Shaun/dev/kernelBP_source/kernelBP_loopy/test-output/sampArr")

    val sampleArr = model.generateData()
    val observations = Map(3 -> DenseMatrix(0.0))
//    Plotter.plotData(sampleArr)

    val kernel = new RBFKernel()
    val passer = new LoopyMessagePasser(model, kernel)

    val betaArr = passer.passMessages(sampleArr, observations)

    val axisBelief = DenseMatrix(linspace(-5, 5, 200).toArray).t
    val sigRoot = 0.1     // Parzen window parameter at root

    val result = new LoopyResult(model, kernel, sampleArr, observations, betaArr, sigRoot, axisBelief)
    LoopyPlotter.plotResults(result)
  }

  def main(args: Array[String]): Unit = {
    runLoopy()
  }
}