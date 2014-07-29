package kernel

import kernel.kernels.LinearKernel
import kernel.models.MessageParam
import kernel.models.edge.{Inferer, HMMModel}
import kernel.parsing.HMMParser
import kernel.propagation.EdgeBasedMessagePasser

object EdgeDemo {
  def runDemo() = {
    val numSamples = 5
    val msgParam: MessageParam = MessageParam(0.1, 3.0)
    val model = new HMMModel(numSamples)

    val edges = model.edges
    val obsArr = model.testObservations
    val testArr = model.testLabels

    val testIdx = 0

    val observations = obsArr(testIdx)

    val kernel = new LinearKernel()
    val parser = new HMMParser(msgParam, kernel)

    val cache = parser.buildCache(edges, observations.size)

    val passer = new EdgeBasedMessagePasser(cache, observations.keySet)
    val betaArr = passer.passMessages(observations)

    val testMatrix = model.testMatrix

    val inferer = new Inferer(testMatrix)

    val testNode = testArr(0).head._1
    val correct = testArr(0).head._2

    val probs = inferer.calculateKernelCondRootMarginal(testNode, cache, betaArr)

    println()
  }

  def main(args: Array[String]): Unit = {
    runDemo()
  }
}
