package kernel.models

import breeze.linalg.{DenseVector, randn, DenseMatrix}

class DemoChainModel(n: Int, length: Int) extends Model(n) {
  override val msgParam: MessageParam = MessageParam(0.1, 0.3)
  override val rootNode = 0

  override protected def _A: DenseMatrix[Int] = DenseMatrix.eye[Int](length)

  val d = 1
  var outputArray: Array[DenseMatrix[Double]] = _
  var sampleIndex: Int = _
  val sigma = 0.2

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
    val sample: DenseVector[Double] = randn(d)
    outputArray(0)(sampleIndex, ::) := sample.t * sigma
  }

  private def sampleNonRoot(nodeId: Int) = {
    val parentId = getParents(nodeId)(0)
    val sample: DenseVector[Double] = randn(d)
    outputArray(nodeId)(sampleIndex, ::) := outputArray(parentId)(sampleIndex, ::) + sample.t * sigma
  }
}