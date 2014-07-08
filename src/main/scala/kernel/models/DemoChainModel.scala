package kernel.models

import breeze.linalg._

class DemoChainModel(n: Int, length: Int) extends ChainModel(n, length) {
  override val msgParam: MessageParam = MessageParam(0.1, 0.3)
  override val rootNode = 0

  val d = 1
  var outputArray: Array[DenseMatrix[Double]] = _
  var sampleIndex: Int = _
  val sigma = 0.2

  val rootMeans = DenseMatrix(0.0, 2.0)

  override def generateData(): Array[DenseMatrix[Double]] = {
    outputArray = Array.ofDim(length)

    for (i <- 0 until numNodes)
      outputArray(i) = DenseMatrix.zeros[Double](n, d)

    generateRandomData()

    outputArray
  }

  private def generateRandomData() = {
    sampleIndex = 0
    while (sampleIndex < n) {
      generateRandomSample()
      sampleIndex += 1
    }
  }

  private def generateRandomSample() = {
    for (nodeId <- 0 until numNodes) {

      if (nodeId == 0)
        sampleRoot()
      else
        sampleNonRoot(nodeId)

    }
  }

  private def sampleRoot() = {
    val c = rand()

    val mean = if (c <= 0.5)
      rootMeans(0, ::)
    else
      rootMeans(1, ::)

    outputArray(0)(sampleIndex, ::) := mean + genSample()
  }

  private def sampleNonRoot(nodeId: Int) = {
    val parentId = getParents(nodeId)(0)
    outputArray(nodeId)(sampleIndex, ::) := outputArray(parentId)(sampleIndex, ::) + genSample()
  }

  private def genSample() = {
    val sample: DenseVector[Double] = randn(d)
    sample.t * sigma
  }
}