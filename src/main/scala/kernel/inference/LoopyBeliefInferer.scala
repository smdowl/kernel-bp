package kernel.inference

import breeze.linalg._
import kernel.plotting.LoopyResult

class LoopyBeliefInferer(result: LoopyResult) {

  def calculateKernelMarginal(nodeId: Int): DenseVector[Double] = {
    val kernelRes = result.kernel(result.support, result.sampleArr(nodeId), result.sigRoot)
    sum(kernelRes, Axis._1)
  }

  def calculateKernelCondRootMarginal(nodeId: Int): DenseVector[Double] = {
    var condRootMarginal: DenseVector[Double] = calculateKernelMarginal(nodeId)

    for (neighbour <- result.model.getNeighbours(nodeId)) {

      val dotLeft = result.support
      val dotRight = result.sampleArr(nodeId)
      val multFactor: DenseMatrix[Double] = result.kernel(dotLeft, dotRight, result.model.msgParam.sig) * result.betaArr(neighbour)(nodeId)

      condRootMarginal :*= multFactor.toDenseVector
    }

    condRootMarginal
  }
}
