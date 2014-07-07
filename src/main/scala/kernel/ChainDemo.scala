package kernel

import breeze.linalg._
import kernel.kernels.RBFKernel
import kernel.models.{DemoChainModel, DemoModel, Model}
import kernel.plotting.{Result, Plotter}
import kernel.propagation.TreeMessagePasser

object ChainDemo {
  def runDemoChain() = {
    val numSamples = 200
    val model: Model = new DemoChainModel(numSamples, 5)

    val sampleArr = model.generateData()
    val observations = Map(4 -> DenseMatrix(2.0))
    Plotter.plotData(sampleArr)

//    val kernel = new RBFKernel()
//    val passer = new TreeMessagePasser(model, kernel)
//
//    val betaArr = passer.passMessages(sampleArr, observations)
//
//    val axisBelief = DenseMatrix(linspace(-5, 5, 200).toArray).t
//    val sigRoot = 0.1     // Parzen window parameter at root
//
//    val result = new Result(model, kernel, sampleArr, observations, betaArr, sigRoot, axisBelief)
//    Plotter.plotResults(result)
  }

  def main(args: Array[String]) = {
    runDemoChain()
  }
}
