package kernel

import kernel.kernels.LinearKernel
import kernel.models.edge.HMMModel
import kernel.parsing.HMMParser
import kernel.propagation.EdgeBasedMessagePasser

object EdgeDemo {
  def runDemo() = {
    val numSamples = 10
    val model = new HMMModel(numSamples)

    val edges = model.edges
    val obsArr = model.testObservations
    val testArr = model.testLabels

    val testIdx = 0

    val observations = obsArr(testIdx)

    val kernel = new LinearKernel()
    val parser = new HMMParser(kernel)

    val cache = parser.buildCache(edges, observations.size)

    val passer = new EdgeBasedMessagePasser(cache, observations.keySet)
    val betaArr = passer.passMessages(observations)

    println()
//    val axisBelief = DenseMatrix(linspace(-5, 5, 200).toArray).t
//    val sigRoot = 0.1 // Parzen window parameter at root
//
//    val result = new LoopyResult(model, kernel, sampleArr, observations, betaArr, sigRoot, axisBelief)
//    LoopyPlotter.plotResults(result)
  }

  def main(args: Array[String]): Unit = {
    runDemo()
  }
}
