package kernel

import breeze.linalg._
import kernel.caches.{LoopyCache, EdgeBasedCache}
import kernel.kernels.RBFKernel
import kernel.models.{DemoChainModel, DemoModel, Model}
import kernel.plotting.{LoopyPlotter, LoopyResult, Result, Plotter}
import kernel.propagation.{EdgeBasedMessagePasser, LoopyMessagePasser, TreeMessagePasser}

object ChainDemo {
  def runDemoChain() = {
    val numSamples = 10
    val model: Model = new DemoChainModel(numSamples, 5)

    val sampleArr = model.generateData()
    val observations = Map(4 -> DenseMatrix(1.0))
//    Plotter.plotData(sampleArr)

    val kernel = new RBFKernel()
    val passer = new LoopyMessagePasser(model, kernel, sampleArr, observations.keySet)
    val loopyCache = LoopyCache.buildCache(sampleArr, kernel, model)
    val edgeCache = EdgeBasedCache.buildCache(sampleArr, kernel, model)

    val cache: EdgeBasedCache = EdgeBasedCache.buildCache(sampleArr, kernel, model)
    val edgePasser = new EdgeBasedMessagePasser(cache, observations.keySet)

    val betaArr = passer.passMessages(observations)
    val edgeBetaArr = edgePasser.passMessages(observations)

    val axisBelief = DenseMatrix(linspace(-5, 5, 200).toArray).t
    val sigRoot = 0.1     // Parzen window parameter at root

    val result = new LoopyResult(model, kernel, sampleArr, observations, betaArr, sigRoot, axisBelief)
    LoopyPlotter.plotResults(result)
  }

  def main(args: Array[String]) = {
    runDemoChain()
  }
}
