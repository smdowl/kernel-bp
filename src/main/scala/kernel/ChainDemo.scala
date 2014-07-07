package kernel

import breeze.linalg._
import kernel.kernels.RBFKernel
import kernel.models.{DemoChainModel, DemoModel, Model}
import kernel.plotting.{LoopyPlotter, LoopyResult, Result, Plotter}
import kernel.propagation.{LoopyMessagePasser, TreeMessagePasser}

object ChainDemo {
  def runDemoChain() = {
    val numSamples = 500
    val model: Model = new DemoChainModel(numSamples, 5)

    val sampleArr = model.generateData()
    val observations = Map(4 -> DenseMatrix(1.0))
    Plotter.plotData(sampleArr)

    val kernel = new RBFKernel()
    val passer = new LoopyMessagePasser(model, kernel)

    val betaArr = passer.passMessages(sampleArr, observations)

    val axisBelief = DenseMatrix(linspace(-5, 5, 200).toArray).t
    val sigRoot = 0.1     // Parzen window parameter at root

    val result = new LoopyResult(model, kernel, sampleArr, observations, betaArr, sigRoot, axisBelief)
    LoopyPlotter.plotResults(result)
  }

  def main(args: Array[String]) = {
    runDemoChain()
  }
}
