package kernel

import breeze.linalg._
import kernel.inference.LoopyBeliefInferer
import kernel.kernels.{LinearKernel}
import kernel.models.{PosModel, BasicPosModel}
import kernel.plotting.LoopyResult
import kernel.propagation.LoopyMessagePasser

object RealRun {

  def main(args: Array[String]) = {
    val run = new RealRun()
    run.runDemoChain()
  }
}

class RealRun {
  val model: PosModel = new BasicPosModel(8, 26)

  def runDemoChain() = {
    val sampleArr = model.generateData()

    // TODO: check if this fudge factor is ok
    sampleArr.foreach(matrix => {
      matrix :+= 1e-12
    })

    val testData = model.generateTestData()
    val testLabels = model.getTestLabels
    val allLabels = model.labelKeys.toSeq

    val observations = Map(0 -> testData(0)(0, ::).t.toDenseMatrix)
    //    Plotter.plotData(sampleArr)

    val kernel = new LinearKernel()
    val passer = new LoopyMessagePasser(model, kernel)

    val betaArr = passer.passMessages(sampleArr, observations)

    for (testSet <- 0 until testData(0).rows) {
      for (nodeId <- 0 until testData.length) {
        val support = genSupport(testData(nodeId)(testSet, ::).t, allLabels)
        val result = new LoopyResult(model, kernel, sampleArr, observations, betaArr, 1.0, support)

        val inferer = new LoopyBeliefInferer(result)
        val marginal = inferer.calculateKernelMarginal(nodeId)
        val cond = inferer.calculateKernelCondRootMarginal(nodeId)

        val guessIdx = cond.findAll(_ == max(cond))
        println(allLabels(guessIdx(0)))
        println(testLabels(nodeId)(testSet))

        println()
      }
    }
  }

  private def genSupport(nodeInstance: DenseVector[Double], allLabels: Seq[String]) = {
    val outArr = DenseMatrix.zeros[Double](allLabels.length, nodeInstance.length)

    for ((label, i) <- allLabels.zipWithIndex) {
      val vec = nodeInstance.copy
      vec(model.getIndexForKey(label)) = 1.0
      outArr(i, ::) := vec.t
    }

    outArr
  }
}
